module Day3.SolveSpec where

import Test.Hspec
import System.IO
import Day3.Solve

spec :: Spec
spec = do
  describe "Computes all houses visited at least once" $ do
    referenceInput <- runIO $ openFile "test/Day3/testinput.txt" ReadMode >>= hGetContents
    it "works for reference input" $ do
      solve1 referenceInput `shouldBe` 2572
    it "works for examples" $ do
      solve1 ">" `shouldBe` 2
      solve1 "^>v<" `shouldBe` 4
      solve1 "^v^v^v^v^v" `shouldBe` 2
  describe "Computes all houses visited once by robo or real santa" $ do
    referenceInput <- runIO $ openFile "test/Day3/testinput.txt" ReadMode >>= hGetContents
    it "works for reference input" $ do
     solve2 referenceInput `shouldBe` 2631
    it "works for examples" $ do
      solve2 "^v" `shouldBe` 3
      solve2 "^>v<" `shouldBe` 3
      solve2 "^v^v^v^v^v" `shouldBe` 11
