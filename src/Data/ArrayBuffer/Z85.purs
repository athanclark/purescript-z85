module Data.ArrayBuffer.Z85 where

import Data.ArrayBuffer.Z85.Internal (encodeWord, decodeWord, Z85Char)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Vec (toArray, fromArray) as Vec
import Data.UInt (UInt)
import Data.Array ((..))
import Data.Array (snoc, length, foldM) as Array
import Data.ArrayBuffer.ArrayBuffer (create) as ArrayBuffer
import Data.ArrayBuffer.DataView (byteLength, getUint32le, setUint32le, whole)
import Data.ArrayBuffer.Types (Uint32Array)
import Data.ArrayBuffer.Typed (dataView, asUint32Array)
import Data.String.CodeUnits (toCharArray)
import Data.String.Yarn (fromChars)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throw)



encodeZ85 :: Uint32Array -> Effect String
encodeZ85 xs' =
  let xs = dataView xs'
      len = byteLength xs
  in  if len == 0 then pure ""
      else
        let ns :: Array Int
            ns =
              let q = len `div` 4
              in  0 .. (q - 1)
            go :: String -> Int -> Effect String
            go acc n = do
              (mWord :: Maybe UInt) <- getUint32le xs (n * 4)
              case mWord of
                Nothing -> throw $ "Can't extract word: " <> show (n * 4)
                Just word ->
                  let word' :: Array Char
                      word' = Vec.toArray (encodeWord word)
                  in  pure (acc <> fromChars word')
        in  Array.foldM go "" ns


decodeZ85 :: String -> Effect Uint32Array
decodeZ85 s =
  if charsLen `mod` 5 == 0
    then do
      let bytesLen = (charsLen `div` 5) * 4
      bytes <- whole <$> ArrayBuffer.create bytesLen

      let go :: Tuple Int (Array Z85Char) -> Char -> Effect (Tuple Int (Array Z85Char))
          go (Tuple byteOffset charsSoFar) c
            | Array.length charsSoFar /= 5 = pure $ Tuple byteOffset $ charsSoFar `Array.snoc` c
            | otherwise = do
                asVec <- case Vec.fromArray charsSoFar of
                  Just cs -> pure cs
                  Nothing -> throw $ "Couldn't convert characters scanned so far to sized vector: "
                                <> show charsSoFar
                case decodeWord asVec of
                  Nothing -> throw $ "Couldn't decode chunk to word: " <> show asVec
                  Just word -> do
                    setUint32le bytes word byteOffset
                    pure (Tuple (byteOffset + 4) [])

      void (Array.foldM go (Tuple 0 []) cs)
      pure (asUint32Array bytes)
    else throw "Serialized string is not multiple of 5"
  where
    cs :: Array Char
    cs = toCharArray s

    charsLen = Array.length cs
