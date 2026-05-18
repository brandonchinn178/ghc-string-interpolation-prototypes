{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Data.String.Syntax.UniformInterpolateBuilder (
  module Data.String.Syntax.UniformInterpolateBuilder,
  FixedUTF8Writer
) where

import Data.Monoid (Endo (..))
import Data.String (IsString(..))
import Data.Word
import Data.String.Syntax.Internal.FixedUTF8Writer (FixedUTF8Writer)
import qualified Data.String.Syntax.Internal.FixedUTF8Writer as Writer

{----- Implementation of s"..." -----}

interpolateRaw :: IsString s => String -> s
interpolateRaw = fromString

interpolateValue :: (Interpolate a, InterpolatorBuilder s) => a -> s
interpolateValue = interpolate

interpolateAppend :: Monoid s => s -> s -> s
interpolateAppend = mappend

infixr 6 `interpolateAppend` -- matches (<>)

interpolateEmpty :: Monoid s => s
interpolateEmpty = mempty

interpolateFinalize :: Interpolator s => InterpolatorBuilderFor s -> s
interpolateFinalize = buildInterpolator

{----- Classes -----}

class
  ( InterpolatorBuilder (InterpolatorBuilderFor s)
  ) => Interpolator s where
  type InterpolatorBuilderFor s
  buildInterpolator :: InterpolatorBuilderFor s -> s

class (IsString b, Monoid b, Interpolator b) => InterpolatorBuilder b where
  interpolateString :: String -> b
  interpolateString = fromString

  interpolateIntegral :: (Integral a, Show a) => a -> b
  interpolateIntegral = fromString . show

  interpolateRealFloat :: (RealFloat a, Show a) => a -> b
  interpolateRealFloat = fromString . show

  -- Not part of the current proposal!
  -- TODO: Separate module containing various ... -> FixedUTF8Writer functions.
  --       (Will be difficult to match the perf of e.g. text's implementation)
  interpolateUTF8 :: FixedUTF8Writer -> b
  interpolateUTF8 = fromString . Writer.toString -- should this be interpolateString? Not sure why interpolateString exists

class Interpolate a where
  {-# MINIMAL interpolate | interpolatePrec #-}

  interpolate :: (InterpolatorBuilder b) => a -> b
  interpolate = interpolatePrec 0

  interpolatePrec :: (InterpolatorBuilder b) => Int -> a -> b
  interpolatePrec _ = interpolate

{----- StringBuilder -----}

newtype StringBuilder = StringBuilder (Endo String)
  deriving newtype (Semigroup, Monoid)
instance IsString StringBuilder where
  fromString s = StringBuilder (Endo (s <>))

instance Interpolator String where
  type InterpolatorBuilderFor String = StringBuilder
  buildInterpolator (StringBuilder (Endo f)) = f mempty

instance Interpolator StringBuilder where
  type InterpolatorBuilderFor StringBuilder = StringBuilder
  buildInterpolator = id

instance InterpolatorBuilder StringBuilder

-- Not part of the current proposal!
instance Interpolator FixedUTF8Writer where
  type InterpolatorBuilderFor FixedUTF8Writer = FixedUTF8Writer
  buildInterpolator = id

instance InterpolatorBuilder FixedUTF8Writer where
  interpolateUTF8 = id

{----- Interpolation of values -----}

instance Interpolate String where
  interpolate = interpolateString
instance Interpolate Char where
  interpolate c = interpolateString [c]

instance Interpolate Int where
  interpolate = interpolateIntegral
instance Interpolate Word8 where
  interpolate = interpolateIntegral
instance Interpolate Double where
  interpolate = interpolateRealFloat
instance Interpolate Float where
  interpolate = interpolateRealFloat
instance Interpolate Bool where
  interpolate = interpolateString . show

instance Interpolate a => Interpolate (Maybe a) where
  interpolate Nothing = interpolateString "Nothing"
  interpolate (Just a) = interpolate a


instance Interpolate FixedUTF8Writer where
  interpolate = interpolateUTF8