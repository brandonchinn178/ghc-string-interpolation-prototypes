{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.String.Syntax.Internal.FixedUTF8Writer where
import Data.Word (Word8)
import Foreign.Ptr (plusPtr)
import Control.Monad.ST (runST)
import Data.String (IsString (fromString))
import GHC.Encoding.UTF8 (utf8EncodePtr, utf8EncodedLength, utf8DecodeByteArray#)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Array.Byte
import GHC.ST (ST (ST))
import GHC.Base (unsafeFreezeByteArray#, newPinnedByteArray#, Int (I#), mutableByteArrayContents#, resizeMutableByteArray#, Char (C#), writeCharOffAddr#, copyAddrToAddrNonOverlapping#, IO (IO), plusAddr#)
import GHC.Ptr (Ptr(Ptr))
import Data.Semigroup (Semigroup(stimes))

-- This module or a use site will almost certainly have some sort of bug with multi-byte characters.
-- At the very least, testing
--  putStr $ toString (fromString "abc◕‿œœ◕dў\n")
-- Text.putStr (interpolateFinalize (interpolateUTF8 "abc◕‿œœ◕dў\n")) -- with the example
-- I got (with a new line after)
--                                 abc◕‿œœ◕dў

data FixedUTF8Writer
  = FixedUTF8Writer
   -- | Minimum buffer capacity required.
  { capacity :: !Int
   -- | Write to the buffer [p, p + size) and return size (where 0 <= size <= capacity)
   --   Reading to characters this did not already write to is not permitted!
  , write    :: forall s . Ptr Word8 -> ST s Int
  }

toMutableByteArray :: FixedUTF8Writer -> ST s (MutableByteArray s)
toMutableByteArray (FixedUTF8Writer (I# !cap) !write) = do
  MutableByteArray !a <- ST (\(!s) -> 
    case newPinnedByteArray# cap s of
      (# !s', !a #) -> (# s', MutableByteArray a #))
  I# !size <- write (Ptr (mutableByteArrayContents# a))
  ST (\(!s) -> 
    case resizeMutableByteArray# a size s of
      (# !s', !a' #) -> (# s', MutableByteArray a' #))

toByteArray :: FixedUTF8Writer -> ByteArray
toByteArray !w = runST (toMutableByteArray w >>= (\(MutableByteArray ma) -> 
  ST (\(!s) -> 
    case unsafeFreezeByteArray# ma s of
      (# !s', !a #) -> (# s', ByteArray a #))))

-- between this and fromString, I am not certain neither of them expect nulls or something
-- which this interface shouldn't necessarily write (if an interpolation result wants \0,
-- it can simply write it after its ran the write operation, using cap+1 or equivalently
-- <> write \0)

toString :: FixedUTF8Writer -> String
toString !w = 
  case toByteArray w of
    ByteArray !a -> utf8DecodeByteArray# a

-- | A single ASCII character. Use fromString for Unicode characters.
ascii :: Char -> FixedUTF8Writer
ascii (C# !c) = FixedUTF8Writer 1 (\(Ptr !p) -> ST (\s -> (# writeCharOffAddr# p 0# c s, 1 #)))

instance Semigroup FixedUTF8Writer where
  FixedUTF8Writer !cap1 !write1 <> FixedUTF8Writer !cap2 !write2
    = FixedUTF8Writer (cap1 + cap2) write12 where
      write12 !p = do
        !size1 <- write1 p
        !size2 <- write2 $! p `plusPtr` size1
        pure $! size1 + size2

  stimes !0 _ = mempty
  stimes !1 a = a
  stimes !i (FixedUTF8Writer !cap !write) = FixedUTF8Writer (j * cap) writeN where
    j :: Int
    !j = fromIntegral i

    writeN :: Ptr Word8 -> ST s Int
    writeN !p@(Ptr !a) = do
      -- Every subsequent write should return the same size
      !size@(I# !size#) <- write p
      -- ... so we can just copy the already written bytes.
      let copy !u | I# !su <- size * u 
                  = unsafeIOToST (IO (\s -> (# copyAddrToAddrNonOverlapping# a (a `plusAddr#` su) size# s, () #)))
      -- Starts at 1 because of previous write
      mapM_ copy [1..j-1]
      -- size, j times
      return (size * j)

instance Monoid FixedUTF8Writer where
  mempty = FixedUTF8Writer 0 (\(!_) -> pure 0)

instance IsString FixedUTF8Writer where
  fromString !s | !n <- utf8EncodedLength s
    = FixedUTF8Writer n (\(!p) -> n <$ unsafeIOToST (utf8EncodePtr p s))
