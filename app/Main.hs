module Main where

import System.Random (getStdGen, StdGen)
import HangulQuiz.Quiz

-- | Run a short quiz of three questions.
main :: IO ()
main = do
  putStrLn "Hangul Quiz!"
  gen <- getStdGen
  (score, _) <- runQuiz 3 gen 0
  putStrLn ("Score: " ++ show score ++ "/3")

-- | Ask N questions accumulating the score.
runQuiz :: Int -> StdGen -> Int -> IO (Int, StdGen)
runQuiz 0 gen score = pure (score, gen)
runQuiz n gen score = do
  let (p, gen1) = randomPair gen
      (q, gen2) = randomQuestion gen1 p
  putStrLn (prompt q)
  ans <- getLine
  let correct = checkAnswer q ans
  putStrLn (if correct
              then "Correct!"
              else "Wrong. Correct answer: " ++ correctAnswer q)
  runQuiz (n-1) gen2 (if correct then score + 1 else score)
