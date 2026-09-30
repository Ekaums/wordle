import Data.Char (isAlpha)

main :: IO ()
main = do
  guess <- getGuess
  return ()

getGuess :: IO String
getGuess = do
  putStr "Guess: "
  val <- getLine
  case validateInput val of
    Left err -> do
      putStrLn err
      getGuess
    Right guess -> do
      putStrLn "nice thats good"
      return guess

validateInput :: String -> Either String String
validateInput x
  | length x /= 5 = Left "Must be 5 characters"
  | not (all isAlpha x) = Left "Must be letters only"
  | otherwise = Right x

-- TODO: add case for not a word