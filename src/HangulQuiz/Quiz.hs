module HangulQuiz.Quiz (randomPair, allPairs) where

import HangulQuiz.Data (pairs)
import System.Random (randomRIO)

allPairs :: [(String,String)]
allPairs = pairs

randomPair :: IO (String,String)
randomPair = do
  i <- randomRIO (0, length pairs - 1)
  return (pairs !! i)
