module Data.ArrayBuffer.Z85 where

import Data.ArrayBuffer.Z85.Internal (encodeWord, decodeWord, Z85Char (..), getZ85Char, Z85Chunk)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Vec (toArray, fromArray) as Vec
import Data.UInt (UInt)
import Data.Array (snoc, length, foldM, take, drop, cons) as Array
import Data.Array ((..))
import Data.ArrayBuffer.ArrayBuffer (empty) as AB
import Data.ArrayBuffer.Types (Uint32Array, Uint32, DataView)
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.DataView as DV
import Data.String.CodeUnits (toCharArray)
import Data.String.Yarn (fromChars)
import Data.String (length) as String
import Data.String.CodeUnits (splitAt) as String
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Exception (throw)
-- import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Effect.Console (log)



encodeZ85 :: Uint32Array -> Effect String
encodeZ85 xs' = do
  sRef <- Ref.new "" -- result string reference
  let go :: UInt -> Effect Unit -- takes word
      go word = do
        -- FIXME either reverse word, or reverse incoming array (somehow)
        log (unsafeCoerce word)
        let word' :: Array Char
            word' = getZ85Char <$> Vec.toArray (encodeWord word)
        void (Ref.modify (\acc -> acc <> fromChars word') sRef)
  TA.traverse_ go xs
  Ref.read sRef



decodeZ85 :: String -> Effect Uint32Array
decodeZ85 s = do
  chunks <- asChunks s
  x <- TA.fromArray (decodeWord <$> chunks)
  TA.reverse x
  pure x
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
