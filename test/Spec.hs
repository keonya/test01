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
    it "picks first element with seed 0" $
      fst (randomPair (mkStdGen 0)) `shouldBe` head pairs
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
