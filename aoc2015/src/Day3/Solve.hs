module Day3.Solve (solve1, solve2) where
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (partition, dropWhileEnd)
import Data.Char (isSpace)

type Coordinate = (Int, Int)

data FlightLog = FlightLog { visited :: Set Coordinate, currentPosition :: Coordinate }

move :: Coordinate -> Char -> Coordinate
move (x, y) '>' = (x+1, y)
move (x, y) '<' = (x-1, y)
move (x, y) '^' = (x, y-1)
move (x, y) 'v' = (x, y+1)
move _ instruction = error $ "invalid instruction: " ++ [instruction]

updateFlightLog :: FlightLog -> Char -> FlightLog
updateFlightLog flightLog instruction = FlightLog { visited = updatedVisited, currentPosition = nextPosition }
  where
    nextPosition = move (currentPosition flightLog) instruction
    updatedVisited = Set.insert nextPosition (visited flightLog)

computeVisitedHouses :: String -> Set Coordinate
computeVisitedHouses = visited . foldl updateFlightLog (FlightLog { visited = Set.singleton (0, 0), currentPosition = (0, 0) })

--- utility

trim = dropWhileEnd isSpace . dropWhile isSpace

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

--- part 1

solve1 :: String -> Int
solve1 = length . computeVisitedHouses . trim

--- part 2

solve2 :: String -> Int
solve2 = length
  . uncurry Set.union
  . mapTuple computeVisitedHouses
  . mapTuple (map snd)
  . partition (even . fst)
  . zip ([0..] :: [Int])
  . trim
