module Main where
import System.IO
import Data.List.Split
import Data.Char
import Parser

main :: IO ()
main = do
    putStrLn "Please enter a classification level, followed by a query. This prompt will loop until you exit with ^C."
    loop ""

loop :: String -> IO ()
loop text = do
    char <- getChar
    case char of
        ';' -> do
            putStrLn $ show $ getResult text
            loop ""
        character ->
            loop (text ++ [character])
    
getResult query =
    case parseQuery ( map toLower query) of
        Nothing -> "Error: Query was invalid. Please enter another query."
        Just result -> show result