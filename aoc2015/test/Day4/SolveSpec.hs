module Day4.SolveSpec where


import Test.Hspec
import Day4.Solve

spec :: Spec
spec = do
  describe "Computes correct AdventCoin hash input" $ do
    it "works for 5 zeros" $ do
      solve1 "bgvyzdsv" `shouldBe` 254575
    it "works for 6 zeros" $ do
      solve2 "bgvyzdsv" `shouldBe` 1038736
