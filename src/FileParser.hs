module FileParser(parseTableName, parseFiles, Table(..), File(..)) where
import Text.ParserCombinators.ReadP
import Data.Char

data File = File { fileName :: String
                 , fileContent :: String
                 }

data Table = Table { tableName :: String
                   , tableData :: [[String]]
                   } deriving (Show)

parseTableName :: String -> String
parseTableName [] = []
parseTableName ('.' : _) = []
parseTableName (h : t) = toLower h : parseTableName t
  
whitespace :: ReadP Char
whitespace = satisfy (\c -> c == '\t' || c == ' ')

newline :: ReadP Char
newline = satisfy (\c -> c == '\n' || c == '\r')

keyword :: ReadP String
keyword = do
  result <- munch1 (\c -> isAlpha c || isDigit c)
  let lower = map toLower result
  return lower

line :: ReadP [String]
line = sepBy1 keyword (many1 whitespace)

file :: ReadP [[String]]
file = sepBy line (many1 newline)

parseFile :: File -> Table
parseFile f =
  let
    name = parseTableName $ fileName f
    parsedFile = fst $ last $ readP_to_S file $ fileContent f
  in
  Table { tableName = name, tableData = parsedFile }

parseFiles :: [File] -> [Table]
parseFiles = map parseFile
