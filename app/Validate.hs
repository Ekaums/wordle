{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Validate (getGuess) where

import Data.Aeson (FromJSON, object, (.=))
import GHC.Generics (Generic)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest_, setRequestBodyJSON)

data Error = WrongLen | InvalidWord

newtype ValidWord = ValidWord {validWord :: Bool} deriving (Generic)
instance FromJSON ValidWord

getGuess :: IO String
getGuess = do
  putStr "Guess: "
  val <- getLine
  result <- validateInput val
  case result of
    Left WrongLen -> do
      putStrLn "Guess must be 5 chars"
      getGuess
    Left InvalidWord -> do
      putStrLn "Not a valid English word"
      getGuess
    Right guess -> return guess

validateInput :: String -> IO (Either Error String)
validateInput x
  | length x /= 5 = return (Left WrongLen)
  | otherwise = do
      isValid <- validateWord x
      if isValid
        then return (Right x)
        else return (Left InvalidWord)

validateWord :: String -> IO Bool
validateWord guess = do
  response <- httpJSON request
  return (validWord (getResponseBody response))
  where
    request =
      setRequestBodyJSON (object ["word" .= guess]) $
        parseRequest_ "POST https://words.dev-apis.com/validate-word"