module Validate (getGuess) where

import Data.Char (isAlpha)

data Error = WrongLen | InvalidWord

validateInput :: String -> Either Error String
validateInput x
  | length x /= 5 = Left WrongLen
  | not (all isAlpha x) = Left InvalidWord
  | otherwise = Right x

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
