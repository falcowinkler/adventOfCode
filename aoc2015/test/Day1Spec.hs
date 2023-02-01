module Day1Spec (spec) where

import Test.Hspec
import System.IO
import Day1

spec :: Spec
spec = do
  describe "Calculate the correct floor for santa to arrive in" $ do
    referenceInput <- runIO $ openFile "test/day1full.txt" ReadMode >>= hGetContents
    it "works for (())" $ do
      solve1 "(())" `shouldBe` 0
    it "works for (((" $ do
      solve1 "(((" `shouldBe` 3
    it "works for ()()" $ do
      solve1 "()()" `shouldBe` 0
    it "works for (()(()(" $ do
      solve1 "(()(()(" `shouldBe` 3
    it "works for )(" $ do
      solve1 ")(" `shouldBe` 0
    it "works for )))" $ do
      solve1 ")))" `shouldBe` -3
    it "works for )())())" $ do
      solve1 ")())())" `shouldBe` -3
    it "works for the reference input" $ do
      solve1 referenceInput `shouldBe` 74
  describe "Calculates the instruction position that causes santa to enter the basement" $ do
    referenceInput <- runIO $ openFile "test/day1full.txt" ReadMode >>= hGetContents
    it "works for )" $ do
      solve2 ")" `shouldBe` 1
    it "works for ()())" $ do
      solve2 "()())" `shouldBe` 5
    it "works for the referenceInput" $ do
      solve2 referenceInput `shouldBe` 1795
