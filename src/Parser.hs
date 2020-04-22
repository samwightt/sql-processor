module Parser (parseQuery, clauseList, QueryType) where
import Text.ParserCombinators.ReadP
import Data.Char
import Data.List

type QueryType = (String, [String], [String], [(String, String)])

{- Common functions -}
whitespace :: ReadP Char
whitespace = do
    satisfy (\char -> any (char ==) "\n ")

separator :: ReadP String
separator = do
    many whitespace
    key <- satisfy (\char -> char == ',')
    many whitespace
    return [key]

operator :: ReadP String
operator = do
    many whitespace
    key <- satisfy (\char -> char == '=')
    many whitespace
    return [key]

accessLevel :: ReadP String
accessLevel = do
    many whitespace
    val <- munch1(\char -> isDigit char)
    many whitespace
    return val

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

clause :: ReadP (String, String)
clause = do
    first <- keyword
    operator
    second <- keyword
    many whitespace
    return (first, second)

andKeyword :: ReadP String
andKeyword = do
    key <- string "and"
    many1 whitespace
    return key

clauseList :: ReadP [(String, String)]
clauseList = do
    sepBy1 clause andKeyword

{- Keywords and statements functions -}
selectKeyword :: ReadP String
selectKeyword = do
    select <- string "select"
    many1 whitespace
    return select

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

whereKeyword :: ReadP String
whereKeyword = do
    key <- string "where"
    many1 whitespace
    return key

whereStatement :: ReadP [(String, String)]
whereStatement = do
    whereKeyword
    result <- clauseList
    many whitespace
    return result

{- Putting it all together... -}
query :: ReadP QueryType
query = do
    access <- accessLevel
    select <- selectStatement
    from <- fromStatement
    whr <- option [] whereStatement
    return (access, select, from, whr)

isReservedWord :: String -> Bool
isReservedWord string =
    string /= "from" && string /= "select" && string /= "where" && string /= "kc"

removeReserved :: QueryType -> QueryType
removeReserved (a, b, c, d) =
    let second = filter isReservedWord b
        third = filter isReservedWord c
    in
    (a, second, third, d)

parseQuery :: String -> Maybe QueryType
parseQuery input =
    case readP_to_S query input of
        [] -> Nothing
        list -> case removeReserved $ fst $ last list of
            (a, b, c, d) ->
                if (length b) == 0 || (length c) == 0 then
                    Nothing
                else
                    Just (a, b, c, d)