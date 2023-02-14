module Day2.SolveSpec where


import Test.Hspec
import System.IO
import Day2.Solve
import GHC.Real (FractionalExponentBase(Base10))

spec :: Spec
spec = do
  describe "Calculate needed wrapping paper correctly" $ do
    referenceInput <- runIO $ openFile "test/Day2/testinput.txt" ReadMode >>= hGetContents
    it "works for reference input" $ do
      solve1 referenceInput `shouldBe` 1598415
    it "works for examples" $ do
      solve1 "2x3x4\n" `shouldBe` 58
      solve1 "1x1x10\n" `shouldBe` 43
      solve1 "2x3x4\n1x1x10\n" `shouldBe` 58 + 43
  describe "Calculates needed ribbon" $ do
    referenceInput <- runIO $ openFile "test/Day2/testinput.txt" ReadMode >>= hGetContents
    it "works for reference input" $ do
      solve2 referenceInput `shouldBe` 3812909
    it "works for examples" $ do
      solve2 "2x3x4\n" `shouldBe` 34
      solve2 "1x1x10\n" `shouldBe` 14
