{-# LANGUAGE DeriveGeneric #-}

{- HLINT ignore "Use zipWith" -}

import Data.Aeson (FromJSON)
import Feedback (GuessResult (..), checkGuess)
import GHC.Generics (Generic)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Validate (getGuess)

data WordResponse = WordResponse {word :: String} deriving (Generic)

instance FromJSON WordResponse

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  soln <- fetchRandomWord
  playGame soln 5

fetchRandomWord :: IO String
fetchRandomWord = do
  request <- parseRequest "https://words.dev-apis.com/word-of-the-day?random=1"
  response <- httpJSON request
  return (word (getResponseBody response))

playGame :: String -> Int -> IO ()
playGame soln 0 = putStrLn ("YOU LOST :( The word was " ++ soln)
playGame soln tries = do
  guess <- getGuess
  case checkGuess guess soln of
    Won -> putStrLn "YOU WON!!!!!"
    Continue feedback -> do
      print feedback
      playGame soln (tries - 1)
