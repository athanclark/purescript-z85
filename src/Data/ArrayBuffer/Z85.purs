module Data.ArrayBuffer.Z85 where

import Data.ArrayBuffer.Z85.Internal (Base256, lookupBase256)

-- import Data.ArrayBuffer.Types (Uint32Array)
-- import Data.String.CodeUnits (toCharArray)






-- encodeZ85 :: Uint32Array -> String
-- encodeZ85 xs = ...

decodeZ85 :: String -> Maybe Uint32Array
decodeZ85 s =
  if Array.length cs `mod` 5 == 0
    then Array.foldM go [] cs
    else Nothing
  where
    go :: Uint8Array -> Char -> Maybe Uint8Array
    go acc c = case lookupBase256 c of
      Nothing -> pure Nothing
      Just base256Value ->

    cs :: Array Char
    cs = toCharArray s
