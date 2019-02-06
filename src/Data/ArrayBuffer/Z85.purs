module Data.ArrayBuffer.Z85 where

import Data.ArrayBuffer.Z85.Internal (encodeWord, decodeWord, Z85Char (..), getZ85Char, Z85Chunk)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Vec (toArray, fromArray) as Vec
import Data.UInt (UInt)
import Data.Array (snoc, length, foldM) as Array
import Data.Array ((..))
import Data.ArrayBuffer.ArrayBuffer (empty) as AB
import Data.ArrayBuffer.Types (Uint32Array, Uint32)
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.DataView as DV
import Data.String.CodeUnits (toCharArray)
import Data.String.Yarn (fromChars)
import Data.String (length) as String
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Exception (throw)
import Partial.Unsafe (unsafePartial)



encodeZ85 :: Uint32Array -> Effect String
encodeZ85 xs' = do
  let bytes = TA.dataView xs'
      len = DV.byteLength bytes
      len' = len `div` 4
      ns = 0 .. (len' - 1)
  sRef <- Ref.new ""
  let go n' = do
        let n = n' * 4
        mX <- DV.getUint32be bytes n
        unsafePartial $ case mX of
          Just word ->
            let word' :: Array Char
                word' = getZ85Char <$> Vec.toArray (encodeWord word)
            in  void (Ref.modify (\acc -> acc <> fromChars word') sRef)
  traverse_ go ns
  Ref.read sRef

  --  if len == 0 then pure ""
  --     else
  -- let go :: String -> UInt -> TA.Offset -> String
  --     go acc word _ =
  -- in  TA.foldl go "" xs


decodeZ85 :: String -> Effect Uint32Array
decodeZ85 s =
  if charsLen `mod` 5 /= 0
    then throw "Serialized string is not multiple of 5"
    else do
      byteOffsetRef <- Ref.new 0
      charsSoFarRef <- Ref.new []

      buffer <- AB.empty bytesLen
      let bytes = DV.whole buffer

          go :: Char -> Effect Unit
          go c = do
            charsSoFar <- Ref.modify (\cs' -> cs' `Array.snoc` Z85Char c) charsSoFarRef
            if Array.length charsSoFar /= 5
              then pure unit
              else do
                let asVec :: Z85Chunk
                    asVec = unsafePartial $ case Vec.fromArray charsSoFar of
                      Just v -> v

                byteOffset <- Ref.read byteOffsetRef
                DV.setUint32be bytes (decodeWord asVec) byteOffset

                Ref.write (byteOffset + 4) byteOffsetRef
                Ref.write [] charsSoFarRef

      traverse_ go cs
      pure (TA.asUint32Array bytes)
  where
    cs :: Array Char
    cs = toCharArray s

    charsLen = String.length s
    wordsLen = charsLen `div` 5
    bytesLen = wordsLen * 4
