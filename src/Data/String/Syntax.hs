{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.String.Syntax where

import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text

import Data.String.Syntax.Internal.Parse

processFile :: InterpolationMode -> FilePath -> Text -> IO Text
processFile mode path file =
  -- (\m -> m >>= \x -> putStrLn (Text.unpack x) >> pure x) $ -- uncomment to debug
  transform <$> parseHaskellFile (delimMarker mode) path file
  where
    transform code =
      id
        . addLinePragma
        . addImports mode code
        . processInterpolation mode
        $ code

    -- tells GHC to use the original path in error messages
    addLinePragma src = "{-# LINE 1 " <> showT path <> " #-}\n" <> src

processInterpolation :: InterpolationMode -> HaskellCode -> Text
processInterpolation InterpolationMode{..} (HaskellCode codeChunks) =
  Text.concat . flip map codeChunks $ \case
    UnparsedSource s -> s
    InterpolatedString var parts -> "(" <> go var parts <> ")"
  where
    go var parts = postProcess var $ renderList (map goPart parts)
    goPart = either processRaw processExpr
    renderList xs = "[" <> Text.intercalate ", " xs <> "]"

addImports :: InterpolationMode -> HaskellCode -> Text -> Text
addImports InterpolationMode{..} (HaskellCode codeChunks) src
  -- don't add imports if we don't have any interpolated strings
  | null [() | InterpolatedString{} <- codeChunks] = src
  | otherwise =
      let (pre, post) = splitAtImports (Text.lines src)
       in Text.unlines $ map genLangExt autoLangExts <> pre <> map genImport autoModules <> post
  where
    -- we'll assume that there's either at least one import,
    -- or we can put imports at the very beginning
    splitAtImports srcLines =
      let isImport line = "import " `Text.isPrefixOf` Text.stripStart line
       in case break isImport srcLines of
            (_, []) -> ([], srcLines)
            (pre, post) -> (pre, post)

    genLangExt ext = "{-# LANGUAGE " <> ext <> " #-}"
    genImport m = "import qualified " <> m <> " as XX"

{----- Mode -----}

data InterpolationMode = InterpolationMode
  { name :: String
    -- ^ The name of the mode
  , autoModules :: [Text]
    -- ^ Modules to auto-import as "XX"
  , autoLangExts :: [Text]
    -- ^ Any language extensions to automatically add
  , delimMarker :: DelimMarker
    -- ^ The interpolation delimiter to parse
  , postProcess ::
      Text -- ^ The identifier used as a delimiter
      -> HaskellCodeSrc -- ^ The interpolated expressions
      -> HaskellCodeSrc
    -- ^ Post-process the given list of interpolated expressions
  , processRaw :: Text -> HaskellCodeSrc
    -- ^ Generate the Haskell expression for the given raw string piece
  , processExpr :: HaskellCodeSrc -> HaskellCodeSrc
    -- ^ Generate the Haskell expression for the given interpolated Haskell expression
  }

parseMode :: String -> Maybe InterpolationMode
parseMode s = List.find ((== s) . name) allModes

allModes :: [InterpolationMode]
allModes =
  [ InterpolationMode
      { name = "implicit-builder"
      , autoModules = ["Data.String.Syntax.ImplicitBuilder"]
      , autoLangExts = []
      , delimMarker = OnlyDelim "s"
      , postProcess = \_ exprs -> "XX.fromBuilder . mconcat $ " <> exprs
      , processRaw = \s -> "XX.toBuilder " <> showT s
      , processExpr = \expr -> "XX.interpolate (" <> expr <> ")"
      }
  , InterpolationMode
      { name = "explicit"
      , autoModules = []
      , autoLangExts = []
      , delimMarker = OnlyDelim "s"
      , postProcess = \_ exprs -> "mconcat " <> exprs
      , processRaw = showT
      , processExpr = id
      }
  , InterpolationMode
      { name = "implicit-only-string"
      , autoModules = ["Data.String.Syntax.ImplicitOnlyString", "Data.String"]
      , autoLangExts = []
      , delimMarker = OnlyDelim "s"
      , postProcess = \_ exprs -> "mconcat " <> exprs
      , processRaw = showT
      , processExpr = \expr -> "XX.fromString (XX.interpolate (" <> expr <> "))"
      }
  , InterpolationMode
      { name = "implicit-no-builder"
      , autoModules = ["Data.String.Syntax.ImplicitNoBuilder"]
      , autoLangExts = []
      , delimMarker = OnlyDelim "s"
      , postProcess = \_ exprs -> "mconcat " <> exprs
      , processRaw = showT
      , processExpr = \expr -> "(XX.interpolate (" <> expr <> "))"
      }
  , InterpolationMode
      { name = "extensible-th"
      , autoModules = ["Data.String.Syntax.ExtensibleTH"]
      , autoLangExts = ["TemplateHaskell"]
      , delimMarker = AnyDelim
      , postProcess = \delim exprs -> "$(" <> (if delim == "s" then "XX.s" else delim) <> " " <> exprs <> ")"
      , processRaw = \s -> "Left " <> showT s
      , processExpr = \expr -> "Right [|" <> expr <> "|]"
      }
  , InterpolationMode
      { name = "extensible-hasclass"
      , autoModules = ["Data.String.Syntax.ExtensibleHasClass"]
      , autoLangExts = []
      , delimMarker = AnyDelim
      , postProcess = \delim exprs -> (if delim == "s" then "XX.s" else delim) <> " " <> exprs
      , processRaw = \s -> "Left " <> showT s
      , processExpr = \expr -> "Right (XX.HasClass (" <> expr <> "))"
      }
  , InterpolationMode -- TODO: use interpolateAppend (possibly use the proposal functions throughout?)
      { name = "uniform-interpolate-builder"
      , autoModules = ["Data.String.Syntax.UniformInterpolateBuilder"]
      , autoLangExts = []
      , delimMarker = OnlyDelim "s"
      , postProcess = \_ exprs -> "interpolateFinalize (mconcat " <> exprs <> ")"
      , processRaw = \s -> "interpolateString " <> showT s
      , processExpr = \expr -> "interpolateValue (" <> expr <> ")"
      }
  ]

showT :: Show a => a -> Text
showT = Text.pack . show
