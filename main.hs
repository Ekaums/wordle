{- HLINT ignore "Use zipWith" -}

import Data.Char (isAlpha)
import Data.List (mapAccumL)
import Data.Map.Strict qualified as M

main :: IO ()
main = do
  playGame 5

playGame 0 = do
  putStrLn "YOU LOST :("
playGame tries = do
  guess <- getGuess
  case checkGuess guess "lemon" of -- TODO: use API for getting 5 letter wordle words
    Won -> putStrLn "YOU WON!!!!!"
    Continue feedback -> do
      print feedback
      playGame (tries - 1)

data GuessResult = Won | Continue [Feedback]

-- Feedback on each character of guess
data Feedback = Correct | Misplaced | Absent
  deriving (Show, Eq)

checkGuess :: String -> String -> GuessResult
checkGuess guess soln =
  if guess == soln
    then Won
    else Continue (computeFeedback guess soln)

{-
Algo for computing feedback:
  - Construct a mapping b/w each char in soln and # of occurences
  - Two passes to compute feedback
  First pass:
    For each char in guess:
      if exact match:
        dec count and mark correct
      else:
        mark absent

  Prioritize exact matches first to handle edge case where multiple of same char in guess

  Second pass:
    For each char in guess:
      if marked absent and count > 0:
         mark misplaced (not correct, but still present)
         dec count
-}
computeFeedback :: String -> String -> [Feedback]
computeFeedback guess soln = snd $ mapAccumL pass2Fn pass1Map (zip guess pass1Feedback)
  where
    table = M.fromListWith (+) [(c, 1) | c <- soln]
    (pass1Map, pass1Feedback) = mapAccumL pass1Fn table (zip guess soln)

    pass1Fn table (guess_c, soln_c)
      | guess_c == soln_c = (M.adjust (subtract 1) guess_c table, Correct)
      | otherwise = (table, Absent)

    pass2Fn table (guess, feedback)
      | feedback == Correct = (table, feedback)
      | M.findWithDefault 0 guess table > 0 = (M.adjust (subtract 1) guess table, Misplaced)
      | otherwise = (table, feedback)

getGuess :: IO String
getGuess = do
  putStr "Guess: "
  val <- getLine
  case validateInput val of
    Left WrongLen -> do
      putStrLn "Must be 5 chars"
      getGuess
    Left InvalidWord -> do
      putStrLn "Must be English letters"
      getGuess
    Right guess -> return guess

data Error = WrongLen | InvalidWord

-- TODO: validate its an actual word
validateInput :: String -> Either Error String
validateInput x
  | length x /= 5 = Left WrongLen
  | not (all isAlpha x) = Left InvalidWord
  | otherwise = Right x