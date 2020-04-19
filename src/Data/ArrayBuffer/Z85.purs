module Data.ArrayBuffer.Z85 where

import Data.ArrayBuffer.Z85.Internal (encodeWord, decodeWord, Z85Char (..), getZ85Char, Z85Chunk, inZ85Charset)

import Prelude
  ( bind, (<>), mod, (<$>), (/=), void, Unit, discard
  , otherwise, (==), pure, not)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Vec (toArray, fromArray) as Vec
import Data.UInt (UInt)
import Data.Array (cons) as Array
import Data.ArrayBuffer.Types (Uint32Array, Uint8Array)
import Data.ArrayBuffer.Typed as TA
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Data.String (length) as String
import Data.String.CodeUnits (splitAt) as String
import Effect (Effect)
import Effect.Ref (read, modify, new) as Ref
import Partial.Unsafe (unsafePartial)



encodeZ85 :: Uint32Array -> Effect String
encodeZ85 xs = do
  sRef <- Ref.new "" -- result string reference
  let go :: UInt -> Effect Unit -- takes word
      go word' = do
        word <- do
          (tmp :: Uint32Array) <- TA.fromArray [word']
          (tmp' :: Uint8Array) <- TA.whole (TA.buffer tmp)
          TA.reverse tmp' -- endian reversal
          unsafePartial (TA.unsafeAt tmp 0)
        let chunk :: Array Char
            chunk = getZ85Char <$> Vec.toArray (encodeWord word)
            go' acc = acc <> fromCharArray chunk -- content reversal
        void (Ref.modify go' sRef)
  TA.traverse_ go xs
  Ref.read sRef


decodeZ85 :: String -> Effect (Either String Uint32Array)
decodeZ85 s = do
  echunks <- asChunks s
  case echunks of
    Left e -> pure (Left e)
    Right chunks -> do
      (x :: Uint32Array) <- TA.fromArray (decodeWord <$> chunks)
      TA.reverse x -- content reversal
      (tmp :: Uint8Array) <- TA.whole (TA.buffer x)
      TA.reverse tmp -- endian reversal
      pure (Right x)
  where
    asChunks :: String -> Effect (Either String (Array Z85Chunk))
    asChunks xs
      | xs == "" = pure (Right [])
      | not (inZ85Charset xs) = pure (Left "Not in z85 character set")
      | String.length xs `mod` 5 /= 0 = pure (Left "Characters not a modulo of five")
      | otherwise = do
        let {before,after} = String.splitAt 5 xs
        case Vec.fromArray (toCharArray before) of
          Nothing -> pure (Left ("Can't turn array into chunk: " <> before))
          Just x -> do
            etail <- asChunks after
            case etail of
              Left e -> pure (Left e)
              Right tail -> pure (Right (Array.cons (Z85Char <$> x) tail))
