module Day2.Solve where
import Text.Parsec

giftSizes = endBy line eol
line = do
  length <- cell
  char ('x')
  width <- cell
  char ('x')
  height <- cell
  return $ Dimensions { l = length, w = width, h = height }
cell = read <$> many digit
eol = char '\n'

data Dimensions = Dimensions { l :: Int, w :: Int, h :: Int } deriving (Show, Eq)

parseGiftSizes :: String -> Either ParseError [Dimensions]
parseGiftSizes input = parse giftSizes "(unknown)" input

solve1 :: String -> Int
solve1 _ = 1

solve2 :: String -> Int
solve2 _ = 1
