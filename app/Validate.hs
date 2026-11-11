{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Validate (getGuess) where

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON, object, (.=))
import GHC.Generics (Generic)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest_, setRequestBodyJSON)
import System.Exit (exitFailure)

data Error = WrongLen | InvalidWord

newtype ValidWord = ValidWord {validWord :: Bool} deriving (Generic)

instance FromJSON ValidWord

getGuess :: IO String
getGuess = do
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
      result <- validateWord x
      case result of
        Left _ -> do
          putStrLn "API for validating input failed!"
          exitFailure
        Right isValid ->
          if isValid
            then return (Right x)
            else return (Left InvalidWord)

validateWord :: String -> IO (Either SomeException Bool)
validateWord guess = try $ do
  response <- httpJSON request
  return (validWord (getResponseBody response))
  where
    request =
      setRequestBodyJSON (object ["word" .= guess]) $
        parseRequest_ "POST https://words.dev-apis.com/validate-word"