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
import Data.Foldable (foldl)
import Control.Monad.Maybe.Trans (MaybeT (..), runMaybeT)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)



encodeZ85 :: Uint32Array -> String
encodeZ85 xs' =
  let xs = dataView xs'
      ns :: Array Int
      ns =
        let q = byteLength xs `div` 4
        in  0 .. (q - 1)
      go :: String -> Int -> String
      go acc n =
        let word :: UInt
            word = unsafePartial $ case unsafePerformEffect (getUint32le xs (n * 4)) of
              Just w -> w
            word' :: Array Char
            word' = Vec.toArray (encodeWord word)
        in  acc <> fromChars word'
  in  foldl go "" ns


decodeZ85 :: String -> Maybe Uint32Array
decodeZ85 s =
  if charsLen `mod` 5 == 0
    then
      let run :: MaybeT Effect Uint32Array
          run = do
            let bytesLen = (charsLen `div` 5) * 4
            bytes <- liftEffect (whole <$> ArrayBuffer.create bytesLen)

            let go :: Tuple Int (Array Z85Char) -> Char -> MaybeT Effect (Tuple Int (Array Z85Char))
                go (Tuple byteOffset charsSoFar) c
                  | Array.length charsSoFar /= 5 = pure $ Tuple byteOffset $ charsSoFar `Array.snoc` c
                  | otherwise =
                      let asVec = unsafePartial $ case Vec.fromArray charsSoFar of
                            Just cs -> cs
                      in  case decodeWord asVec of
                            Nothing -> MaybeT (pure Nothing)
                            Just word -> do
                              liftEffect (setUint32le bytes word byteOffset)
                              pure (Tuple (byteOffset + 4) [])

            void (Array.foldM go (Tuple 0 []) cs)
            pure (asUint32Array bytes)
      in  unsafePerformEffect (runMaybeT run)
    else Nothing
  where
    cs :: Array Char
    cs = toCharArray s

    charsLen = Array.length cs
