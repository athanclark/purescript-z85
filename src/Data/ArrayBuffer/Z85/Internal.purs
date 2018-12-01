module Data.ArrayBuffer.Z85.Internal where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Array ((..))
import Data.Array (index) as Array
import Data.UInt (UInt, pow, zshr, (.&.))
import Data.UInt (toInt, fromInt) as UInt
-- import Data.Int (pow)
-- import Data.Int.Bits ((.&.), zshr)
import Data.Char (toCharCode)
import Data.String.CodeUnits (charAt)
import Data.Tuple.Native (t5, T5, prj)
import Data.Traversable (for_)
import Data.Typelevel.Num (d0, d1, d2, d3, d4)
import Control.Monad.ST (run) as ST
import Control.Monad.ST.Ref (new, read, modify) as STRef
import Partial.Unsafe (unsafePartial)


-- | Represents a single base85 digit between `0` and `84`
type Base85 = UInt

-- | Represents a single base256 digit between `0x00` and `0xFF` - a Byte
type Base256 = UInt

-- | Character included in the z85 character set: `0-9`, `a-z`, `A-Z`, and `:+=^!/*?&<>()[]{}@%$#`
type Z85Char = Char

-- | Represents a 32-bit word encoded as 5 z85 characters
type Z85Chunk = T5 Z85Char Z85Char Z85Char Z85Char Z85Char


-- | Sorted by their value in the z85 encoding
z85Chars :: String
z85Chars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.-:+=^!/*?&<>()[]{}@%$#"


lookupZ85Char :: Base85 -> Z85Char
lookupZ85Char idx = unsafePartial $ case charAt (UInt.toInt idx) z85Chars of
  Just c -> c


-- | Reverse-sorted by the lookup index obtained by getting the UTF-16 char code value, minus 32 -
-- | the equivalent of `"n".charCodeAt(0) - 32` in JavaScript, where `n` is some x85 character.
charCodeToBase256 :: Array Base256
charCodeToBase256 = map UInt.fromInt
  [ 0x00, 0x44, 0x00, 0x54, 0x53, 0x52, 0x48, 0x00
  , 0x4B, 0x4C, 0x46, 0x41, 0x00, 0x3F, 0x3E, 0x45
  , 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07
  , 0x08, 0x09, 0x40, 0x00, 0x49, 0x42, 0x4A, 0x47
  , 0x51, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2A
  , 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30, 0x31, 0x32
  , 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A
  , 0x3B, 0x3C, 0x3D, 0x4D, 0x00, 0x4E, 0x43, 0x00
  , 0x00, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10
  , 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18
  , 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20
  , 0x21, 0x22, 0x23, 0x4F, 0x00, 0x50, 0x00, 0x00
  ]


lookupBase256 :: Z85Char -> Maybe Base256
lookupBase256 c = Array.index charCodeToBase256 (toCharCode c - 32)


-- | Encodes the value by extracting it out of a little-endian packed word, and packing it into five x85 chars
encodeWord :: UInt -> Z85Chunk
encodeWord word =
  let value = ST.run do
        valueRef <- STRef.new (UInt.fromInt 0)
        for_ (0 .. 3) \j ->
          let base256 :: Base256
              base256 = UInt.fromInt 0xFF
              byteShift :: UInt
              byteShift = UInt.fromInt (8 * (4 - j - 1))
              byteChunk :: Base256
              byteChunk = (word `zshr` byteShift) .&. base256
          in  void (STRef.modify (\val -> (val * UInt.fromInt 256) + byteChunk) valueRef)
        STRef.read valueRef
      getChar :: Int -> Z85Char
      getChar n =
        let divisor :: UInt
            divisor = UInt.fromInt 85 `pow` UInt.fromInt n
            idx :: Base85
            idx = (value `div` divisor) `mod` UInt.fromInt 85
        in  lookupZ85Char idx
  in  t5 (getChar 4) (getChar 3) (getChar 2) (getChar 1) (getChar 0)


decodeWord :: Z85Chunk -> Maybe UInt
decodeWord chunk = case mBase256Values of
  Nothing -> Nothing
  Just base256Values ->
    let value :: UInt
        value = ST.run do
          valueRef <- STRef.new (UInt.fromInt 0)
          let addPartValue part = void (STRef.modify (\val -> (val * UInt.fromInt 85) + part) valueRef)
          addPartValue (prj d0 base256Values)
          addPartValue (prj d1 base256Values)
          addPartValue (prj d2 base256Values)
          addPartValue (prj d3 base256Values)
          addPartValue (prj d4 base256Values)
          STRef.read valueRef
        divisor :: Int -> UInt
        divisor n = UInt.fromInt 256 `pow` UInt.fromInt n
    in  Just $ ST.run do
          wordRef <- STRef.new (UInt.fromInt 0)
          for_ (3 .. 0) \n ->
            let go word =
                  let magnitude = word * UInt.fromInt 256
                      dust = (value `div` divisor n) `mod` UInt.fromInt 256
                  in  magnitude + dust
            in  void (STRef.modify go wordRef)
          STRef.read wordRef
  where
    mBase256Values :: Maybe (T5 Base256 Base256 Base256 Base256 Base256)
    mBase256Values = t5 <$> lookupBase256 (prj d0 chunk)
                        <*> lookupBase256 (prj d1 chunk)
                        <*> lookupBase256 (prj d2 chunk)
                        <*> lookupBase256 (prj d3 chunk)
                        <*> lookupBase256 (prj d4 chunk)



