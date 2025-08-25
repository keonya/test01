module HangulQuiz.Data
  ( Pair(..)
  , pairs
  ) where

-- | Represents a Hangul syllable and its transcription.
data Pair = Pair
  { syllable     :: String
  , transcription :: String
  } deriving (Eq, Show)

-- | Sample list of Hangul syllables with Latin transcriptions.
pairs :: [Pair]
pairs =
  [ Pair "가" "ga"
  , Pair "나" "na"
  , Pair "다" "da"
  , Pair "라" "ra"
  , Pair "마" "ma"
  ]
