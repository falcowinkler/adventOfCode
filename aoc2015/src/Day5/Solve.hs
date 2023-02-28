module Day5.Solve (solve1, solve2) where
import Data.List.Split ( splitOn )
import Data.List ( isInfixOf )

nice1 :: String -> Bool
nice1 s = hasVowels && hasEqualAdjacentPair && noNaughtyPairs
  where
    hasVowels = length (filter (`elem` "aeiou") s) >= 3
    hasEqualAdjacentPair = or [x == y | (x, y) <- zip s (tail s)]
    noNaughtyPairs = not $ or [naughtyPair `isInfixOf` s | naughtyPair <- ["ab", "cd", "pq", "xy"]]

nice2 :: String -> Bool
nice2 s = rule1 s && rule2
  where
    rule1 (x:y:xs) = isInfixOf [x,y] xs || rule1 (y:xs)
    rule1 _ = False
    rule2 = or [x == y | (x, _, y) <- zip3 s (tail s) (tail (tail s))]

sumNiceLines :: (String -> Bool) -> String -> Int
sumNiceLines rule input = sum $ map (fromEnum . rule) strings
  where
    strings = splitOn "\n" input

solve1 :: String -> Int
solve1 = sumNiceLines nice1

solve2 :: String -> Int
solve2 = sumNiceLines nice2
