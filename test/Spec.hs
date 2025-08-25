module Main (main) where

import HangulQuiz.Data (pairs)
import Data.List (lookup)
import System.Exit (exitFailure)

main :: IO ()
main = do
  let count = length pairs
  if count /= 11172
    then do
      putStrLn ("Expected 11172 syllables, got " ++ show count)
      exitFailure
    else return ()
  check "가" "ga"
  check "힣" "hit"
  putStrLn "All tests passed."
  where
    check s r =
      case lookup s pairs of
        Just v | v == r -> return ()
        _ -> do
          putStrLn ("Incorrect romanization for " ++ s)
          exitFailure
