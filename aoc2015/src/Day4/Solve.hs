module Day4.Solve (solve1, solve2) where
import Crypto.Hash
import Data.ByteString.Lazy.UTF8 (fromString)

getHash :: String -> String
getHash s = show digest
  where
    digest = hashlazy byteString :: Digest MD5
    byteString = fromString s

matches :: String -> Integer -> Int -> Bool
matches prefix num numZeros = take numZeros (getHash (prefix ++ show num)) == replicate numZeros '0'

solve :: Int -> String -> Integer
solve numZeros = go 1
  where
    go i s = if matches s i numZeros then i else go (i + 1) s

solve1 :: String -> Integer
solve1 = solve 5

solve2 :: String -> Integer
solve2 = solve 6
