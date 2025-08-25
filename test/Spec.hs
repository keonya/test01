module Main (main) where

import Test.Hspec
import Test.QuickCheck
import System.Random (mkStdGen)
import HangulQuiz.Data
import HangulQuiz.Quiz

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "checkAnswer" $ do
    let pair = Pair "가" "ga"
    it "accepts correct transcription" $
      checkAnswer (AskTranscription pair) "ga" `shouldBe` True
    it "rejects wrong transcription" $
      checkAnswer (AskTranscription pair) "na" `shouldBe` False
    it "accepts correct syllable" $
      checkAnswer (AskSyllable pair) "가" `shouldBe` True
  describe "randomPair" $ do
    it "produit un élément de la liste" $
      fst (randomPair (mkStdGen 0)) `shouldSatisfy` (`elem` pairs)
  describe "randomQuestion" $ do
    let p = head pairs
    it "asks transcription when random is 0" $
      fst (randomQuestion (mkStdGen 0) p) `shouldBe` AskTranscription p
    it "asks syllable when random is 1" $
      fst (randomQuestion (mkStdGen 1) p) `shouldBe` AskSyllable p
  describe "prompt and correctAnswer" $ do
    let p = Pair "가" "ga"
    it "shows syllable when asking transcription" $
      prompt (AskTranscription p) `shouldBe` "Transcription for: 가"
    it "shows transcription when asking syllable" $
      prompt (AskSyllable p) `shouldBe` "Syllable for: ga"
    it "provides correct answer" $
      correctAnswer (AskTranscription p) `shouldBe` "ga"
  describe "checkAnswer . correctAnswer" $ do
    it "always succeeds" $
      property $ \(NonNegative n) b ->
        let p = pairs !! (n `mod` length pairs)
            q = if b then AskTranscription p else AskSyllable p
        in checkAnswer q (correctAnswer q)
  describe "pairs dataset" $ do
    it "contains 11172 entries" $
      length pairs `shouldBe` 11172
  describe "romanize" $ do
    it "romanizes first syllable" $
      romanize 0 `shouldBe` "ga"
    it "romanizes second syllable" $
      romanize 1 `shouldBe` "gak"
    it "romanizes last syllable" $
      romanize 11171 `shouldBe` "hit"
