{- HLINT ignore "Use zipWith" -}

import Data.Char (isAlpha)
import Data.List (mapAccumL)
import Data.Map.Strict qualified as M

main :: IO ()
main = do
  playGame

playGame = do
  guess <- getGuess
  case checkGuess guess "lemon" of -- TODO: use API for getting 5 letter wordle words
    Won -> putStrLn "YOU WON!!!!!"
    Continue feedback -> do
      print feedback
      playGame

data GuessResult = Won | Continue [Feedback]

-- Feedback on each character of guess
data Feedback = Correct | Misplaced | Absent
  deriving (Show, Eq)

checkGuess :: String -> String -> GuessResult
checkGuess guess soln =
  if guess == soln
    then Won
    else Continue (computeFeedback guess soln)

-- How to correctly mark misplaced chars?
-- 1. construct a mapping b/w each char -> count
-- 2. First pass: For each char, if exact match then dec count and mark correct, else absent. Prioritize exact matches first to handle edge cases (two of same char guessed)
-- 3. Second pass: For each other char, if absent:
--        if count > 0 then
--          misplaced (still some left)
--          dec count

computeFeedback :: String -> String -> [Feedback]
computeFeedback guess soln = final
  where
    charCounts str = M.fromListWith (+) [(c, 1) | c <- str]
    table = charCounts soln
    (pass1Map, pass1Feedback) = mapAccumL pass1Fn table (zip guess soln)
    (_, final) = mapAccumL pass2Fn pass1Map (zip guess pass1Feedback)

pass1Fn table (guess_c, soln_c) = if guess_c == soln_c then (M.adjust (subtract 1) guess_c table, Correct) else (table, Absent)

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