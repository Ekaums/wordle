import Data.Char (isAlpha)

main :: IO ()
main = do
  playGame

playGame = do
  guess <- getGuess
  case checkGuess guess "lemon" of -- TODO: use API for getting 5 letter wordle words
    Won -> putStrLn "YOU WON!!!!!"
    Continue hint -> putStrLn hint

data GuessResult = Won | Continue String

checkGuess :: String -> String -> GuessResult
checkGuess guess answer = if guess == answer then Won else Continue "too bad next time"

-- TODO check input

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