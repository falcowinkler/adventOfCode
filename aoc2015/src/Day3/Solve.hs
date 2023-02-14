module Day3.Solve where
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (partition)

data State = State { visited :: Set (Int, Int), px :: Int, py :: Int }

update :: State -> Char -> State
update state char = State {
  visited = updatedVisited,
  px = nx,
  py = ny
                          }
  where
    x = px state
    y = py state
    nx = case char of
      '>' -> x+1
      '<' -> x-1
      _ -> x
    ny = case char of
      '^' -> y+1
      'v' -> y-1
      _ -> y
    updatedVisited = Set.insert (nx, ny) (visited state)

computeVisited :: String -> Set (Int, Int)
computeVisited = visited . foldl update (State { visited = Set.singleton (0, 0), px = 0, py = 0 })

solve1 :: String -> Int
solve1 = length . computeVisited

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

solve2 :: String -> Int
solve2 = length
  . uncurry Set.union
  . mapTuple computeVisited
  . mapTuple (map snd)
  . partition (even . fst)
  . zip ([0..] :: [Int])
