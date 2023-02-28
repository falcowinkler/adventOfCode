module Day5.SolveSpec where


import Test.Hspec
import Day5.Solve
import System.IO

spec :: Spec
spec = do
  describe "Seperate naughty and nice strings1" $ do
    referenceInput <- runIO $ openFile "test/Day5/testinput.txt" ReadMode >>= hGetContents
    it "works for some examples" $ do
      solve1 "ugknbfddgicrmopn\n" `shouldBe` 1
      solve1 "aaa\n" `shouldBe` 1
      solve1 "jchzalrnumimnmhp\n" `shouldBe` 0
      solve1 "haegwjzuvuyypxyu\n" `shouldBe` 0
      solve1 "dvszwmarrgswjxmb\n" `shouldBe` 0
    it "works for reference input" $ do
      solve1 referenceInput `shouldBe` 236
  describe "Seperate naughty and nice strings" $ do
    referenceInput <- runIO $ openFile "test/Day5/testinput.txt" ReadMode >>= hGetContents
    it "works for some examples" $ do
      solve2 "qjhvhtzxzqqjkmpb\n" `shouldBe` 1
      solve2 "xxyxx\n" `shouldBe` 1
      solve2 "uurcxstgmygtbstg\n" `shouldBe` 0
      solve2 "ieodomkazucvgmuy\n" `shouldBe` 0
    it "works for reference input" $ do
      solve2 referenceInput `shouldBe` 51
