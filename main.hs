{- HLINT ignore "Use zipWith" -}
import Data.Char (isAlpha)

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
  deriving (Show)

checkGuess :: String -> String -> GuessResult
checkGuess guess soln =
  if guess == soln
    then Won
    else Continue (computeFeedback guess soln)

computeFeedback :: String -> String -> [Feedback]
computeFeedback guess soln = map (\(x, y) -> if x == y then Correct else Absent) (zip guess soln)

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

validateInput :: String -> Either Error String
validateInput x
  | length x /= 5 = Left WrongLen
  | not (all isAlpha x) = Left InvalidWord
  | otherwise = Right x