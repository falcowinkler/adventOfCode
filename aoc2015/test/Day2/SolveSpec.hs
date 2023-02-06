module Day2.SolveSpec where


import Test.Hspec
import System.IO
import Day2.Solve

spec :: Spec
spec = do
  describe "ParseGiftSizes" $do
    it "parses correctly" $ do
      parseGiftSizes "12x4x13\n" `shouldBe` (Right $ [Dimensions { l = 12, w = 4, h = 13 }])
  describe "Calculate the correct floor for santa to arrive in" $ do
    referenceInput <- runIO $ openFile "test/Day1/testinput.txt" ReadMode >>= hGetContents
    it "works for reference input" $ do
      solve1 referenceInput `shouldBe` 74
  describe "Calculates the instruction position that causes santa to enter the basement" $ do
    referenceInput <- runIO $ openFile "test/Day1/testinput.txt" ReadMode >>= hGetContents
    it "works for reference input" $ do
      solve2 referenceInput `shouldBe` 74
