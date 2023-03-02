module Day6.Solve (solve1, solve2) where
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec
import Text.ParserCombinators.Parsec ( Parser )
import Data.Functor

type Coordinate = (Integer, Integer)
type LightsMap = Map Coordinate Int

data Command = TurnOn | TurnOff | Toggle deriving (Eq,Enum,Show)

data Instruction = Instruction {
  command :: Command,
  corner1 :: Coordinate,
  corner2 :: Coordinate
  } deriving (Show, Eq)

parseCommand :: Parser Command
parseCommand = choice [
    try (string "toggle") $> Toggle,
    try (string "turn on") $> TurnOn,
    string "turn off" $> TurnOff
  ]

parseCoordinate :: Parser Coordinate
parseCoordinate =  number >>= \x -> char ',' *> number >>= \y -> return (x, y)
  where number = read <$> many digit

instructions = endBy line eol
line = parseCommand >>= \command ->
  char ' ' *>
  parseCoordinate >>= \point1 ->
  string " through " *>
  parseCoordinate >>= \point2 ->
  return Instruction { command = command, corner1 = point1, corner2 = point2 }
eol = char '\n'

parseInstructions :: String -> [Instruction]
parseInstructions input = fatalParseError $ parse instructions "(unknown)" input
  where
    fatalParseError (Left e) = error ("Invalid input" ++ show e)
    fatalParseError (Right result) = result

turnOn :: LightsMap -> Coordinate -> LightsMap
turnOn lightsMap coord = Map.insert coord 1 lightsMap

turnOff :: LightsMap -> Coordinate -> LightsMap
turnOff lightsMap coord = Map.insert coord 0 lightsMap

toggle :: LightsMap -> Coordinate -> LightsMap
toggle lightsMap coord = Map.insert coord (if currentValue == 0 then 1 else 0) lightsMap
  where currentValue = Map.findWithDefault 0 coord lightsMap

allCoords :: Coordinate -> Coordinate -> [Coordinate]
allCoords (x1, y1) (x2, y2) = [(x, y) | x <- [x1..x2], y <- [y1..y2]]

updateLights :: LightsMap -> Instruction -> LightsMap
updateLights lights (Instruction TurnOn c1 c2) = foldl turnOn lights (allCoords c1 c2)
updateLights lights (Instruction TurnOff c1 c2) = foldl turnOff lights (allCoords c1 c2)
updateLights lights (Instruction Toggle c1 c2) = foldl toggle lights (allCoords c1 c2)

solve1 :: String -> Int
solve1 s = totalLightValue
  where
    totalLightValue = Map.foldl (+) 0 updatedLightsMap
    updatedLightsMap = foldl updateLights Map.empty allInstructions
    allInstructions = parseInstructions s

solve2 :: String -> Int
solve2 _ = 0
