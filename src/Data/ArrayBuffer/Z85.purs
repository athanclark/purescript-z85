module Data.ArrayBuffer.Z85 where

import Data.ArrayBuffer.Z85.Internal (encodeWord, decodeWord, Z85Char (..), getZ85Char)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Vec (toArray, fromArray) as Vec
import Data.UInt (UInt)
import Data.Array (snoc, length, foldM) as Array
import Data.ArrayBuffer.ArrayBuffer (empty) as AB
import Data.ArrayBuffer.Types (Uint32Array, Uint32)
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.DataView as DV
import Data.String.CodeUnits (toCharArray)
import Data.String.Yarn (fromChars)
import Effect (Effect)
import Effect.Exception (throw)



encodeZ85 :: Uint32Array -> String
encodeZ85 xs =
  -- let xs = dataView xs'
  --     len = byteLength xs
  -- in  if len == 0 then pure ""
  --     else
  let go :: String -> UInt -> TA.Offset -> String
      go acc word _ =
        let word' :: Array Char
            word' = getZ85Char <$> Vec.toArray (encodeWord word)
        in  acc <> fromChars word'
  in  TA.foldl go "" xs


decodeZ85 :: String -> Effect Uint32Array
decodeZ85 s =
  if charsLen `mod` 5 /= 0
    then throw "Serialized string is not multiple of 5"
    else do
      let buffer = AB.empty bytesLen

          bytes = DV.whole buffer

          go :: Tuple Int (Array Z85Char) -> Char -> Effect (Tuple Int (Array Z85Char))
          go (Tuple byteOffset charsSoFar) c
            | Array.length charsSoFar /= 5 = pure $ Tuple byteOffset $ charsSoFar `Array.snoc` Z85Char c
            | otherwise = do
                asVec <- case Vec.fromArray charsSoFar of
                  Just cs -> pure cs
                  Nothing -> throw $ "Couldn't convert characters scanned so far to sized vector: "
                                <> show charsSoFar
                let word = decodeWord asVec
                DV.set (DV.DVProxy :: DV.DVProxy Uint32 DV.LE) bytes word byteOffset
                pure (Tuple (byteOffset + 4) [])

      void (Array.foldM go (Tuple 0 []) cs)
      pure (TA.whole buffer)
  where
    cs :: Array Char
    cs = toCharArray s

    charsLen = Array.length cs
    wordsLen = charsLen `div` 5
    bytesLen = wordsLen * 4
