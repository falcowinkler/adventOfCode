module Day5.Solve (solve1, solve2) where
import Data.List.Split
import Data.List (isInfixOf)

nice :: String -> Bool
nice s = hasVowels && hasEqualAdjacentPair && noNaughtyPairs
  where
    hasVowels = length (filter (`elem` "aeiou") s) >= 3
    hasEqualAdjacentPair = or [x == y | (x, y) <- zip s (tail s)]
    noNaughtyPairs = not $ or [naughtyPair `isInfixOf` s | naughtyPair <- ["ab", "cd", "pq", "xy"]]

solve1 :: String -> Int
solve1 input = sum $ map (fromEnum . nice) strings
  where
    strings = splitOn "\n" input

solve2 :: String -> Int
solve2 _ = 0
