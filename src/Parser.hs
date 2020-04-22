module Parser (parseQuery) where
import Text.ParserCombinators.ReadP
import Data.Char

whitespace :: ReadP Char
whitespace = do
    satisfy (\char -> any (char ==) "\n ")

separator :: ReadP String
separator = do
    optional $ many whitespace
    key <- satisfy (\char -> char == ',')
    optional $ many whitespace
    return [key]

selectKeyword :: ReadP String
selectKeyword = do
    select <- string "select"
    many1 whitespace
    return select

keyword :: ReadP String
keyword = do
    munch1(\char -> (isAlpha char) || (isDigit char))

keywordList :: ReadP [String]
keywordList = do
    sepBy1 keyword separator

wildCard :: ReadP [String]
wildCard = do
    key <- satisfy(\char -> char == '*')
    many1 whitespace
    return [[key]]

selectStatement :: ReadP [String]
selectStatement = do
    many whitespace
    selectKeyword
    result <- choice [wildCard, keywordList]
    many whitespace
    return result

fromKeyword :: ReadP String
fromKeyword = do
    from <- string "from"
    many1 whitespace
    return from

fromStatement :: ReadP [String]
fromStatement = do
    fromKeyword
    result <- keywordList
    many whitespace
    return result

query :: ReadP ([String], [String])
query = do
    select <- selectStatement
    from <- fromStatement
    return (select, from)

parseQuery :: String -> Maybe ([String], [String])
parseQuery input =
    case readP_to_S query input of
        [] -> Nothing
        list -> Just $ fst $ last list