import Data.Text.Internal.Encoding.Utf32 (validate)
main :: IO ()
main = do 
    putStr "Guess: "
    val <- getLine
    case validateInput val of
        Left err -> putStrLn err
        Right guess -> putStrLn "nice thats good"

validateInput :: String -> Either String String
validateInput x =  if length x /= 5 then 
    Left "Must be 5 characters" 
    else Right x
-- add case for not a word