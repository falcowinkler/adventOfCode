module Day2.Solve where
import Text.Parsec
import Data.List (sort)

giftSizes = endBy line eol
line = cell >>= \parcelLength ->
       char 'x' *>
       cell >>= \parcelWidth ->
       char 'x' *>
       cell >>= \parcelHeight ->
       return Dimensions { l = parcelLength, w = parcelWidth, h = parcelHeight }
cell = read <$> many digit
eol = char '\n'

data Dimensions = Dimensions { l :: Int, w :: Int, h :: Int } deriving (Show, Eq)

parseGiftSizes :: String -> [Dimensions]
parseGiftSizes input = fatalParseError $ parse giftSizes "(unknown)" input
  where
    fatalParseError (Left _) = error "Invalid input"
    fatalParseError (Right result) = result

calculateWrappingPaper :: Dimensions -> Int
calculateWrappingPaper d = (2 * a) + (2 * b) + (2 * c) + minimum [a, b, c]
  where
    a = l d * w d
    b = w d * h d
    c = h d * l d

solve1 :: String -> Int
solve1 = sum . map calculateWrappingPaper . parseGiftSizes

calculateRibbonLength :: Dimensions -> Int
calculateRibbonLength d = 2 * x + 2 * y + a * b * c
  where
    a = l d
    b = w d
    c = h d
    (x:y:_) = sort [a, b, c]

solve2 :: String -> Int
solve2 = sum . map calculateRibbonLength . parseGiftSizes
