module Day6.SolveSpec where


import Test.Hspec
import Day6.Solve
import System.IO

spec :: Spec
spec = do
  describe "Figures out the number of switched-on lightbulbs" $ do
    referenceInput <- runIO $ openFile "test/Day6/testinput.txt" ReadMode >>= hGetContents
    it "works for some examples" $ do
      solve1 "toggle 0,0 through 0,0\n" `shouldBe` 1
      solve1 "toggle 0,0 through 1,1\n" `shouldBe` 4
      solve1 "toggle 0,0 through 1,1\ntoggle 0,0 through 1,1\n" `shouldBe` 0
      solve1 "turn on 0,0 through 0,0\n" `shouldBe` 1
      solve1 "turn on 0,0 through 1,1\n" `shouldBe` 4
      solve1 "turn on 0,0 through 1,1\nturn off 0,0 through 1,1\n" `shouldBe` 0
    --it "works for reference input" $ do
      --solve1 referenceInput `shouldBe` 543903
  describe "Figures out the light value of all dimmed lightbulbs" $ do
    referenceInput <- runIO $ openFile "test/Day6/testinput.txt" ReadMode >>= hGetContents
    it "works for some examples" $ do
      solve2 "toggle 0,0 through 0,0\n" `shouldBe` 2
      solve2 "toggle 0,0 through 1,1\n" `shouldBe` 8
    --it "works for reference input" $ do
      --solve2 referenceInput `shouldBe` 14687245
