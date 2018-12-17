module Test.Main where

import Data.ArrayBuffer.Z85 (encodeZ85, decodeZ85)
import Data.ArrayBuffer.Z85.Internal
  (encodeWord, decodeWord, lookupBase85, allZ85Chars, lookupZ85Char, Z85Char)

import Prelude
import Data.Maybe (Maybe (..))
import Data.UInt (UInt)
import Data.UInt as UInt
import Data.Int (rem, toNumber)
import Data.ArrayBuffer.Types (Uint32Array)
import Data.ArrayBuffer.Typed (fromArray)
import Data.ArrayBuffer.Typed.Gen (genTypedArray, genUWord)
import Data.Array (dropEnd, length) as Array
import Data.Array ((..))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Console (log)
import Test.QuickCheck (arbitrary, quickCheck, quickCheckGen, Result, (===))
import Test.QuickCheck.Gen (Gen, arrayOf)
import Test.Assert (assertEqual)


main :: Effect Unit
main = do
  log "Static unit tests..."
  log "  - enumeration of Z85Chars and their value"
  assertEqual
    { actual: lookupBase85 <$> allZ85Chars
    , expected: UInt.fromInt <$> (0 .. 84)
    }
  log "Generating test cases..."
  log "  - char code / base85 mapping is isomorphic"
  quickCheck charTest
  log "  - word serialization isomorphism"
  quickCheckGen wordTest
  log "  - sentence serialization isomorphism"
  quickCheckGen sentenceTest


charTest :: Z85Char -> Result
charTest c = lookupZ85Char (lookupBase85 c) === c


wordTest :: Gen Result
wordTest = do
  x <- genUWord
  pure (x === decodeWord (encodeWord x))

sentenceTest :: Gen Result
sentenceTest = do
  xs <- genTypedArray 10 Nothing genUWord
  let round1 = unsafePerformEffect (encodeZ85 xs)
      round2 = unsafePerformEffect (encodeZ85 =<< decodeZ85 round1)
  pure (round1 === round2)
