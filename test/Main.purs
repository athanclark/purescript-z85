module Test.Main where

import Data.ArrayBuffer.Z85 (encodeZ85, decodeZ85)
import Data.ArrayBuffer.Z85.Internal (encodeWord, decodeWord)

import Prelude
import Data.Maybe (Maybe (..))
import Data.UInt (UInt)
import Data.UInt as UInt
import Data.Int (rem, toNumber)
import Data.ArrayBuffer.ArrayBuffer (fromArray)
import Data.ArrayBuffer.Types (Uint32Array)
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Typed (asUint32Array)
import Data.Array (dropEnd, length) as Array
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Console (log)
import Test.QuickCheck (arbitrary, quickCheckGen)
import Test.QuickCheck.Gen (Gen, arrayOf)

main :: Effect Unit
main = do
  log "Generating test cases..."
  log "  - word serialization isomorphism"
  quickCheckGen wordTest
  log "  - sentence serialization isomorphism"
  quickCheckGen sentenceTest


wordTest :: Gen Boolean
wordTest = do
  x <- arbitraryUInt32
  pure (Just x == decodeWord (encodeWord x))

sentenceTest :: Gen Boolean
sentenceTest = do
  xs <- arbitraryUint32Array
  let round1 = unsafePerformEffect (encodeZ85 xs)
      round2 = unsafePerformEffect (decodeZ85 round1 >>= encodeZ85)
  pure (round1 == round2)


arbitraryUInt32 :: Gen UInt
arbitraryUInt32 = (\x -> UInt.fromNumber x `mod` UInt.fromInt 32) <$> arbitrary


arbitraryUint32Array :: Gen Uint32Array
arbitraryUint32Array = do
  let pruneRight xs =
        let len = Array.length xs
        in  Array.dropEnd (len `rem` 4)
  xs <- arrayOf ((\x -> toNumber (x `mod` 32)) <$> arbitrary)
  pure $ asUint32Array $ whole $ fromArray xs
