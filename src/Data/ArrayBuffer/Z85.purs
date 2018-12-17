module Data.ArrayBuffer.Z85 where

import Data.ArrayBuffer.Z85.Internal (encodeWord, decodeWord, Z85Char (..), getZ85Char, Z85Chunk, z85Chars)

import Prelude
  ( div, bind, (<>), ($), (*), (-), (+), mod, (<$>), (/=), void, Unit, discard, not, show
  , otherwise, (==), pure, unit)
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Vec (toArray, fromArray) as Vec
import Data.UInt (UInt)
import Data.Array (snoc, length, foldM) as Array
import Data.Array ((..))
import Data.ArrayBuffer.ArrayBuffer (empty) as AB
import Data.ArrayBuffer.Types (Uint32Array, Uint32)
import Data.ArrayBuffer.Typed (buffer, whole) as TA
import Data.ArrayBuffer.DataView (whole, byteLength, get, set, DVProxy (..), BE) as DV
import Data.String.CodeUnits (toCharArray)
import Data.String.Yarn (fromChars)
import Data.String (length) as String
import Data.String (contains, Pattern (..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Ref (read, modify, new, write) as Ref
import Effect.Exception (throw)
import Partial.Unsafe (unsafePartial)



encodeZ85 :: Uint32Array -> Effect String
encodeZ85 xs' = do
  sRef <- Ref.new ""
  let go n' = do
        let n = n' * 4
        mX <- DV.get (DV.DVProxy :: DV.DVProxy Uint32 DV.BE) bytes n
        unsafePartial $ case mX of
          Just word ->
            let word' :: Array Char
                word' = getZ85Char <$> Vec.toArray (encodeWord word)
            in  void (Ref.modify (\acc -> acc <> fromChars word') sRef)
  traverse_ go ns
  Ref.read sRef
  where
    buffer = TA.buffer xs'
    bytes = DV.whole buffer
    len = DV.byteLength bytes
    len' = len `div` 4
    ns = 0 .. (len' - 1)


decodeZ85 :: String -> Effect Uint32Array
decodeZ85 s =
  if charsLen `mod` 5 /= 0
    then throw "Serialized string is not multiple of 5"
    else do
      byteOffsetRef <- Ref.new 0
      charsSoFarRef <- Ref.new []

      let go :: Char -> Effect Unit
          go c
            | not (contains (Pattern (fromChars [c])) z85Chars) =
              throw $ "Character not in z85 character set: " <> show c
            | otherwise = do
              charsSoFar <- Ref.modify (\cs' -> cs' `Array.snoc` Z85Char c) charsSoFarRef
              if Array.length charsSoFar == 5
                then do
                  let asVec :: Z85Chunk
                      asVec = unsafePartial $ case Vec.fromArray charsSoFar of
                        Just v -> v

                  byteOffset <- Ref.read byteOffsetRef
                  DV.set (DV.DVProxy :: DV.DVProxy Uint32 DV.BE) bytes (decodeWord asVec) byteOffset

                  Ref.write (byteOffset + 4) byteOffsetRef
                  Ref.write [] charsSoFarRef
                else pure unit

      traverse_ go cs
      pure (TA.whole buffer)
  where
    cs :: Array Char
    cs = toCharArray s

    buffer = AB.empty bytesLen
    bytes = DV.whole buffer

    charsLen = String.length s
    wordsLen = charsLen `div` 5
    bytesLen = wordsLen * 4
