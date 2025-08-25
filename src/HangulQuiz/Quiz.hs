module HangulQuiz.Quiz
  ( Question(..)
  , randomPair
  , randomQuestion
  , checkAnswer
  , prompt
  , correctAnswer
  ) where

import System.Random (StdGen, randomR)
import HangulQuiz.Data

-- | A quiz question either asks for the transcription or the syllable.
data Question
  = AskTranscription Pair -- ^ Show a syllable and ask for its transcription.
  | AskSyllable Pair      -- ^ Show a transcription and ask for the syllable.
  deriving (Eq, Show)

-- | Pick a random syllable/transcription pair from the list.
randomPair :: StdGen -> (Pair, StdGen)
randomPair gen =
  let (idx, gen') = randomR (0, length pairs - 1) gen
  in (pairs !! idx, gen')

-- | Pick randomly whether to ask for the transcription or the syllable.
randomQuestion :: StdGen -> Pair -> (Question, StdGen)
randomQuestion gen p =
  let (b, gen') = randomR (0, 1 :: Int) gen
  in (if b == 0 then AskTranscription p else AskSyllable p, gen')

-- | Check if the user's answer is correct for the given question.
checkAnswer :: Question -> String -> Bool
checkAnswer (AskTranscription p) ans = transcription p == ans
checkAnswer (AskSyllable p) ans      = syllable p == ans

-- | Render the prompt for a question.
prompt :: Question -> String
prompt (AskTranscription p) = "Transcription for: " ++ syllable p
prompt (AskSyllable p)      = "Syllable for: " ++ transcription p

-- | Provide the correct answer to a question.
correctAnswer :: Question -> String
correctAnswer (AskTranscription p) = transcription p
correctAnswer (AskSyllable p)      = syllable p
