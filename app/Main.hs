module Main where
import System.Environment
import Data.Char
import FileParser
import Parser
import Executor

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

main :: IO ()
main = do
  args <- getArgs
  if null args then
    putStrLn "Please enter more than one argument."
  else do
    tables <- getTables args
    print $ show tables
    putStrLn "Please enter a classification level, followed by a query. This prompt will loop until you exit with ^C."
    loop tables ""

getTables :: [String] -> IO [Table]
getTables args = do
  files <- mapM getFile args
  let results = parseFiles files
  return results

getFile :: String -> IO File
getFile f = do
  result <- readFile f
  return ( File { fileName = f
                , fileContent = result
                }
         )

loop :: [Table] -> String -> IO ()
loop t text = do
  c <- getChar
  case c of
    ';' -> do
      print $ show $ length $ getResult t text
      loop t ""
    character ->
      loop t (text ++ [character])

getResult :: [Table] -> String -> [[String]]
getResult tables query =
  let
    result = parseQuery ( map toLower query )
    errorText = "Error: Query was invalid. Please enter another query."
  in
  case result of
    Nothing -> [[errorText]]
    Just q ->
      case getCrossProduct q tables of
        Left err -> [[err]]
        Right r -> r
