module HangulQuiz.Quiz (randomPair, allPairs) where

import HangulQuiz.Data (romanize)
import Data.Char (chr)
import System.Random (randomRIO)

base :: Int
base = 0xAC00

syllableCount :: Int
syllableCount = 11172

pairAt :: Int -> (String, String)
pairAt off = ([chr (base + off)], romanize off)

allPairs :: [(String,String)]
allPairs = map pairAt [0 .. syllableCount - 1]

randomPair :: IO (String,String)
randomPair = do
  i <- randomRIO (0, syllableCount - 1)
  return (pairAt i)
