module Main (main) where

import Test.Hspec
import HangulQuiz.Data (romanize)
import HangulQuiz.Quiz (randomPair, allPairs)

main :: IO ()
main = hspec $ do
  describe "allPairs" $ do
    it "contains 11172 entries" $
      length allPairs `shouldBe` 11172
  describe "randomPair" $ do
    it "returns a pair from allPairs" $ do
      p <- randomPair
      p `shouldSatisfy` (`elem` allPairs)
  describe "romanize" $ do
    it "romanizes first syllable" $
      romanize 0 `shouldBe` "ga"
    it "romanizes second syllable" $
      romanize 1 `shouldBe` "gak"
    it "romanizes last syllable" $
      romanize 11171 `shouldBe` "hit"
