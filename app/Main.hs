{-# LANGUAGE DeriveGeneric #-}

{- HLINT ignore "Use zipWith" -}

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON)
import Feedback (GuessResult (..), checkGuess, printFeedback)
import GHC.Generics (Generic)
import Network.HTTP.Simple (Response, getResponseBody, httpJSON, parseRequest)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Validate (getGuess)

newtype WordResponse = WordResponse {word :: String} deriving (Generic)

instance FromJSON WordResponse

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  soln <- fetchRandomWord
  case soln of
    Left _ -> putStrLn "API for retreiving word failed!"
    Right s -> playGame s 5

playGame :: String -> Int -> IO ()
playGame soln 0 = putStrLn ("YOU LOST :(\nThe word was " ++ soln)
playGame soln tries = do
  putStr ("Guess #" ++ show (6 - tries) ++ ": ")
  guess <- getGuess
  case checkGuess guess soln of
    Won -> putStrLn "YOU WON!!!!!"
    Continue feedback -> do
      printFeedback feedback
      playGame soln (tries - 1)

fetchRandomWord :: IO (Either SomeException String)
fetchRandomWord = try $ do
  request <- parseRequest "https://words.dev-apis.com/word-of-the-day?random=1"
  response <- httpJSON request :: IO (Response WordResponse) -- Assert that return json can be converted to this type. In other words, it has the fields needed to construct this type
  return (word (getResponseBody response)) -- Request returns json in form {word : "word"}. We just have to make sure our data type matches the fields we want
