module Main where

import System.Environment (getArgs)
import Text.Read (readMaybe)
import System.Random (getStdGen, StdGen)
import HangulQuiz.Quiz

-- | Run the quiz according to command line options.
main :: IO ()
main = do
  putStrLn "Hangul Quiz! (type :q to quit)"
  args <- getArgs
  let (mCount, dir) = parseArgs args
  gen <- getStdGen
  (score, asked, _) <- runQuiz mCount dir gen 0 0
  putStrLn ("Score: " ++ show score ++ "/" ++ show asked)

-- | Ask questions accumulating the score.
--   The first parameter is 'Nothing' for infinite quizzes.
runQuiz :: Maybe Int -> Direction -> StdGen -> Int -> Int -> IO (Int, Int, StdGen)
runQuiz (Just 0) _ gen score asked = pure (score, asked, gen)
runQuiz count dir gen score asked = do
  let (p, gen1) = randomPair gen
      (q, gen2) = randomQuestion gen1 p dir
  putStrLn (prompt q)
  ans <- getLine
  if ans == ":q"
    then pure (score, asked, gen2)
    else do
      let correct = checkAnswer q ans
      putStrLn (if correct
                  then "Correct!"
                  else "Wrong. Correct answer: " ++ correctAnswer q)
      runQuiz (fmap (\n -> n - 1) count) dir gen2 (if correct then score + 1 else score) (asked + 1)

-- | Parse command line arguments into (number of questions, direction).
parseArgs :: [String] -> (Maybe Int, Direction)
parseArgs = go (Just 3) Both
  where
    go n d [] = (n, d)
    go n d ("-n":x:xs) = go (parseCount x n) d xs
    go n d ("--tests":x:xs) = go (parseCount x n) d xs
    go n d ("-d":x:xs) = go n (parseDir x d) xs
    go n d ("--direction":x:xs) = go n (parseDir x d) xs
    go n d (_:xs) = go n d xs

    parseCount s def
      | s `elem` ["inf", "infinite"] = Nothing
      | otherwise = case readMaybe s of
          Just v  -> Just v
          Nothing -> def

    parseDir s _
      | s `elem` ["st", "syllable"] = ToTranscription
      | s `elem` ["ts", "transcription"] = ToSyllable
      | s == "both" = Both
    parseDir _ def = def
