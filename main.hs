{- HLINT ignore "Use zipWith" -}

import Feedback (Feedback (..), GuessResult (..), checkGuess)
import Validate (getGuess)

main :: IO ()
main = do
  playGame 5

playGame :: Int -> IO ()
playGame 0 = putStrLn "YOU LOST :("
playGame tries = do
  guess <- getGuess
  case checkGuess guess "lemon" of -- TODO: use API for getting 5 letter wordle words
    Won -> putStrLn "YOU WON!!!!!"
    Continue feedback -> do
      print feedback
      playGame (tries - 1)
