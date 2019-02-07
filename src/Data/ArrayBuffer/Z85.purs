module Data.ArrayBuffer.Z85 where

import Data.ArrayBuffer.Z85.Internal (encodeWord, decodeWord, Z85Char (..), getZ85Char, Z85Chunk)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Vec (toArray, fromArray) as Vec
import Data.UInt (UInt)
import Data.Array (cons) as Array
import Data.ArrayBuffer.ArrayBuffer (slice, byteLength) as AB
import Data.ArrayBuffer.Types (Uint32Array, Uint8Array)
import Data.ArrayBuffer.Typed as TA
import Data.String.CodeUnits (toCharArray)
import Data.String.Yarn (fromChars)
import Data.String (length) as String
import Data.String.CodeUnits (splitAt) as String
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Exception (throw)
-- import Partial.Unsafe (unsafePartial)
-- import Unsafe.Coerce (unsafeCoerce)
-- import Effect.Console (log)



encodeZ85 :: Uint32Array -> Effect String
encodeZ85 xs' = do
  (xs :: Uint32Array) <- do
    (tmp :: Uint32Array) <- TA.toArray xs' >>= TA.fromArray
    let bits = TA.buffer tmp
    (xs'' :: Uint8Array) <- TA.whole (AB.slice 0 (AB.byteLength bits) bits)
    TA.reverse xs''
    TA.whole (TA.buffer xs'')
  sRef <- Ref.new "" -- result string reference
  let go :: UInt -> Effect Unit -- takes word
      go word = do
        -- FIXME either reverse word, or reverse incoming array (somehow)
        -- log (unsafeCoerce word)
        let word' :: Array Char
            word' = getZ85Char <$> Vec.toArray (encodeWord word)
        void (Ref.modify (\acc -> fromChars word' <> acc) sRef)
  TA.traverse_ go xs
  Ref.read sRef



decodeZ85 :: String -> Effect Uint32Array
decodeZ85 s = do
  chunks <- asChunks s
  do  (x :: Uint32Array) <- TA.fromArray (decodeWord <$> chunks)
      TA.reverse x
      let buff = TA.buffer x
      (tmp :: Uint8Array) <- TA.whole buff
      TA.reverse tmp
      TA.whole (TA.buffer tmp)
      --TA.whole buff
  -- TA.reverse x
  -- pure x
  where
    asChunks :: String -> Effect (Array Z85Chunk)
    asChunks xs
      | String.length xs `mod` 5 /= 0 = throw "Characters not a modulo of five"
      | String.length xs == 0 = pure []
      | otherwise = do
        let {before,after} = String.splitAt 5 xs
        -- let chunk' = Array.take 5 xs
        chunk <- case Vec.fromArray (toCharArray before) of
          Nothing -> throw ("Can't turn array into chunk: " <> before)
          Just x -> pure (Z85Char <$> x)
        tail <- asChunks after
        pure (Array.cons chunk tail)
