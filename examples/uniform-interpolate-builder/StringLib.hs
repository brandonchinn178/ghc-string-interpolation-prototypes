{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}

module StringLib where

-- string interpolation type code goes here
-- often uses internals of the string, GHC.Exts etc

import Data.String (IsString (fromString))
import Data.String.Syntax.UniformInterpolateBuilder
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Internal qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder qualified as Text (Builder)
import Data.Text.Lazy.Builder qualified as Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int qualified as Text.Lazy.Builder
import Data.Text.Lazy.Builder.RealFloat qualified as Text.Lazy.Builder
import Data.Text.Array qualified as Text.Array
import Data.Text.Internal.Builder qualified as Text.Lazy.Builder (writeN)
import Data.String.Syntax.Internal.FixedUTF8Writer
import GHC.Ptr (Ptr(Ptr))
import Data.Array.Byte
import GHC.ST (ST(ST))
import Data.Word (Word8)
import GHC.Base (Int (I#), mutableByteArrayContents#, plusAddr#, copyByteArrayToAddr#)



{----- Text -----}

instance Interpolator Text.Builder where
  type InterpolatorBuilderFor Text.Builder = Text.Builder
  buildInterpolator :: Text.Builder -> Text.Builder
  buildInterpolator = id

instance Interpolator Text where
  type InterpolatorBuilderFor Text = Text.Builder
  buildInterpolator :: Text.Builder -> Text
  buildInterpolator = Text.Lazy.toStrict . Text.Lazy.Builder.toLazyText

instance InterpolatorBuilder Text.Builder where
  interpolateString :: String -> Text.Builder
  interpolateString = fromString

  interpolateIntegral :: (Integral a, Show a) => a -> Text.Builder
  interpolateIntegral = Text.Lazy.Builder.decimal

  interpolateRealFloat :: (RealFloat a, Show a) => a -> Text.Builder
  interpolateRealFloat = Text.Lazy.Builder.realFloat

  -- TODO: This is wrong if capacity is an overestimate
  --       Text has an internal unexported writeAtMost that does what this should do.
  --       .. but if the proposal went through with this, text could implement it
  --          and people could access it via this
  interpolateUTF8 :: FixedUTF8Writer -> Text.Builder
  interpolateUTF8 (FixedUTF8Writer cap write) = Text.Lazy.Builder.writeN cap 
    (\(MutableByteArray !a) (I# !off) -> () <$ write (Ptr (plusAddr# (mutableByteArrayContents# a) off)))

instance Interpolate Text where
  -- safer
  -- interpolate = fromString . Text.unpack

  -- what text should probably do (memcpy)
  interpolate (Text.Text (ByteArray !a) (I# !offset) size@(I# !size#)) = interpolateUTF8 (FixedUTF8Writer size write) where
    write :: Ptr Word8 -> ST s Int
    write (Ptr !p) = ST (\s -> (# copyByteArrayToAddr# a offset p size# s, size #))
