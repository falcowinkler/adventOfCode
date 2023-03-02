module Day6.Solve (solve1, solve2) where
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec
import Text.ParserCombinators.Parsec ( Parser )
import Data.Functor

type Coordinate = (Integer, Integer)
type LightsMap = Map Coordinate Integer

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

allCoords :: Coordinate -> Coordinate -> [Coordinate]
allCoords (x1, y1) (x2, y2) = [(x, y) | x <- [x1..x2], y <- [y1..y2]]

currentValue :: Coordinate -> LightsMap -> Integer
currentValue = Map.findWithDefault 0

turnOn1 :: LightsMap -> Coordinate -> LightsMap
turnOn1 lightsMap coord = Map.insert coord 1 lightsMap

turnOff1 :: LightsMap -> Coordinate -> LightsMap
turnOff1 lightsMap coord = Map.insert coord 0 lightsMap

toggle1 :: LightsMap -> Coordinate -> LightsMap
toggle1 lightsMap coord = Map.insert coord (if currentValue coord lightsMap == 0 then 1 else 0) lightsMap

updateLights1 :: LightsMap -> Instruction -> LightsMap
updateLights1 lights (Instruction TurnOn c1 c2) = foldl turnOn1 lights (allCoords c1 c2)
updateLights1 lights (Instruction TurnOff c1 c2) = foldl turnOff1 lights (allCoords c1 c2)
updateLights1 lights (Instruction Toggle c1 c2) = foldl toggle1 lights (allCoords c1 c2)

solve :: (LightsMap -> Instruction -> LightsMap) -> String -> Integer
solve foldFn s = totalLightValue
  where
    totalLightValue = Map.foldl (+) 0 updatedLightsMap
    updatedLightsMap = foldl foldFn Map.empty allInstructions
    allInstructions = parseInstructions s

solve1 :: String -> Integer
solve1 = solve updateLights1

turnOn2 :: LightsMap -> Coordinate -> LightsMap
turnOn2 lightsMap coord = Map.insert coord (currentValue coord lightsMap + 1) lightsMap

turnOff2 :: LightsMap -> Coordinate -> LightsMap
turnOff2 lightsMap coord = Map.insert coord (max (currentValue coord lightsMap - 1) 0) lightsMap

toggle2 :: LightsMap -> Coordinate -> LightsMap
toggle2 lightsMap coord = Map.insert coord (currentValue coord lightsMap + 2) lightsMap

updateLights2 :: LightsMap -> Instruction -> LightsMap
updateLights2 lights (Instruction TurnOn c1 c2) = foldl turnOn2 lights (allCoords c1 c2)
updateLights2 lights (Instruction TurnOff c1 c2) = foldl turnOff2 lights (allCoords c1 c2)
updateLights2 lights (Instruction Toggle c1 c2) = foldl toggle2 lights (allCoords c1 c2)

solve2 :: String -> Integer
solve2 = solve updateLights2
