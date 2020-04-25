module Main where
import System.Environment
import Data.Char
import FileParser
import Parser

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
    loop ""

getTables :: [String] -> IO [TableType]
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

loop :: String -> IO ()
loop text = do
  c <- getChar
  case c of
    ';' -> do
      print $ show $ getResult text
      loop ""
    character ->
      loop (text ++ [character])

getResult :: String -> String
getResult query =
  let
    result = parseQuery ( map toLower query )
    errorText = "Error: Query was invalid. Please enter another query."
  in
  maybe errorText (show . selectList) result
