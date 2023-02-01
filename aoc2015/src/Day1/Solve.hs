module Day1.Solve (solve1, solve2) where

solve1 :: String -> Int
solve1 ('(':xs) = 1 + solve1 xs
solve1 (')':xs) = solve1 xs - 1
solve1 [] = 0
solve1 _ = error "Invalid input"

solve2 :: String -> Int
solve2 = go 0 0
  where
    go :: Int -> Int -> String -> Int
    go (-1) n _ = n
    go d n ('(':xs) = go (d + 1) (n + 1) xs
    go d n (')':xs) = go (d - 1) (n + 1) xs
    go _ _ _ = error "Invalid input"
