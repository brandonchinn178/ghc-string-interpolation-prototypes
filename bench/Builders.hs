{-
Run with:

stack build --no-run-benchmarks :builders && \
  .stack-work/dist/*/ghc-*/build/builders/builders
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Criterion.Main
import Data.List (intercalate)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import UnliftIO.Temporary (withSystemTempDirectory)

main :: IO ()
main = withSystemTempDirectory "" $ \tmpdir -> do
  putStrLn "===== Setting up test cases ====="
  benchmarks <- mapM (setup tmpdir) testCases

  putStrLn "===== Running benchmarks ====="
  defaultMain benchmarks

data TestCase = TestCase
  { expr :: ExprType
  , depth :: Int -- ^ How many nested interpolations
  , len :: Int -- ^ How many interpolations per level
  }

data ExprType = String | Builder deriving (Show)

testCases :: [TestCase]
testCases =
  [ TestCase{..}
  | expr <- [String, Builder]
  -- , depth <- [0, 1, 5, 10, 50]
  -- , len <- [1, 5, 10, 50]
  , (depth, len) <- [(5, 5), (6, 6), (7, 7)]
  ]

setup :: FilePath -> TestCase -> IO Benchmark
setup tmpdir testCase = do
  let outputExe = getOutputExe testCase
  let inputFile = (tmpdir </> outputExe) ++ ".hs"
  let outputFile = getOutputFile testCase

  putStrLn $ "Setting up " <> outputExe
  writeFile inputFile outputFile
  runProc "stack" $
    [ "ghc"
    , "--package"
    , "unliftio"
    , "--"
    , "-O2"
    , inputFile
    , "-o"
    , tmpdir </> outputExe
    ]
  putStrLn $ "Finished " <> outputExe

  pure . bench outputExe . whnfIO $ runProc (tmpdir </> outputExe) []

runProc :: FilePath -> [String] -> IO ()
runProc cmd args =
  readProcessWithExitCode cmd args "" >>= \case
    (ExitSuccess, _, _) -> pure ()
    (ExitFailure _, stdout, stderr) -> do
      putStrLn "----- stdout ------"
      putStrLn stdout
      putStrLn "----- stderr ------"
      putStrLn stderr
      putStrLn "----- command -----"
      putStrLn $ show (cmd : args)
      exitFailure

getOutputExe :: TestCase -> FilePath
getOutputExe TestCase{..} =
  intercalate "_" $
    [ show expr
    , "depth" <> show depth
    , "len" <> show len
    ]

getOutputFile :: TestCase -> String
getOutputFile TestCase{..} =
  unlines
    [ "{-# LANGUAGE MultiParamTypeClasses #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "{-# LANGUAGE TypeFamilyDependencies #-}"
    , "import Data.Monoid (Endo (..))"
    , "import UnliftIO.Exception (evaluateDeep)"
    , ""
    , "main :: IO ()"
    , "main = evaluateDeep s0 *> pure ()"
    , ""
    , interpolateClass
    , ""
    , "x :: Int"
    , "x = 123"
    , "{-# NOINLINE x #-}"
    , ""
    , strings
    ]
  where
    interpolateClass =
      case expr of
        String ->
          unlines
            [ "class Interpolate a where"
            , "  interpolate :: a -> String"
            , "instance Interpolate String where"
            , "  interpolate = id"
            , "instance Interpolate Int where"
            , "  interpolate = show"
            ]
        Builder ->
          unlines
            [ "class Buildable s where"
            , "  type Builder s = b | b -> s"
            , "  toBuilder :: s -> Builder s"
            , "  fromBuilder :: Builder s -> s"
            , "instance Buildable String where"
            , "  type Builder String = Endo String"
            , "  toBuilder s = Endo (s <>)"
            , "  {-# INLINE toBuilder #-}"
            , "  fromBuilder (Endo f) = f []"
            , "  {-# INLINE fromBuilder #-}"
            , "{-# RULES \"fromBuilder/toBuilder\" forall x. fromBuilder (toBuilder x) = x #-}"
            , "{-# RULES \"toBuilder/fromBuilder\" forall x. toBuilder (fromBuilder x) = x #-}"
            , ""
            , "class Interpolate a s where"
            , "  interpolate :: a -> Builder s"
            , "instance Interpolate String String where"
            , "  interpolate = toBuilder"
            , "instance Interpolate Int String where"
            , "  interpolate = toBuilder . show"
            ]

    strings =
      {-
      s0 = s"${s1}${s1}${s1}..."
      s1 = s"${s2}${s2}${s2}..."
      ...
      sN = s"${x}${x}${x}..."
      -}
      unlines . concat $
        [ [ thisStr ++ " = " ++ finalize ++ interpolate (replicate len val)
          , "{-# NOINLINE " ++ thisStr ++ " #-}"
          ]
        | n <- [0 .. depth]
        , let thisStr = "s" ++ show n
        , let nextStr = "s" ++ show (n + 1)
        , let val = if n == depth then "x" else nextStr
        ]

    -- interpolate x <> interpolate x <> ... <> mempty
    interpolate vals = intercalate " <> " $ map ("interpolate " <>) vals ++ [empty]

    empty =
      -- don't use mempty because we need to guide type inference if there aren't any string literals
      case expr of
        String -> "\"\""
        Builder -> "toBuilder \"\""
    finalize =
      case expr of
        String -> ""
        Builder -> "fromBuilder $ "
