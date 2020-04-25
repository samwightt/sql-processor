module Parser (parseQuery, clauseList, QueryType(..), ComparisonOp(..)) where
import Text.ParserCombinators.ReadP
import Data.Char

data ComparisonOp = Equality String String

data QueryType = QueryType { accessLevel :: String
                           , selectList :: [String]
                           , fromList :: [String]
                           , whereList :: [ComparisonOp]}

{- Common functions -}
whitespace :: ReadP Char
whitespace = satisfy (\c -> c `elem` "\n ")

separator :: ReadP String
separator = do
  _ <- many whitespace
  key <- satisfy (== ',')
  _ <- many whitespace
  return [key]

operator :: ReadP String
operator = do
  _ <- many whitespace
  key <- satisfy (== '=')
  _ <- many whitespace
  return [key]

accessL :: ReadP String
accessL = do
  _ <- many whitespace
  val <- munch1 isDigit
  _ <- many whitespace
  return val

keyword :: ReadP String
keyword = munch1 (\c -> isAlpha c || isDigit c)

keywordList :: ReadP [String]
keywordList = sepBy1 keyword separator

wildCard :: ReadP [String]
wildCard = do
  key <- satisfy (== '*')
  _ <- many1 whitespace
  return [[key]]

clause :: ReadP ComparisonOp
clause = do
  first <- keyword
  _ <- operator
  second <- keyword
  _ <- many whitespace
  return (Equality first second)

andKeyword :: ReadP String
andKeyword = do
  key <- string "and"
  _ <- many1 whitespace
  return key

clauseList :: ReadP [ComparisonOp]
clauseList = sepBy1 clause andKeyword

{- Keywords and statements functions -}
selectKeyword :: ReadP String
selectKeyword = do
  select <- string "select"
  _ <- many1 whitespace
  return select

selectStatement :: ReadP [String]
selectStatement = do
  _ <- many whitespace
  _ <- selectKeyword
  result <- choice [wildCard, keywordList]
  _ <- many whitespace
  return result

fromKeyword :: ReadP String
fromKeyword = do
  from <- string "from"
  _ <- many1 whitespace
  return from

fromStatement :: ReadP [String]
fromStatement = do
  _ <- fromKeyword
  result <- keywordList
  _ <- many whitespace
  return result

whereKeyword :: ReadP String
whereKeyword = do
  key <- string "where"
  _ <- many1 whitespace
  return key

whereStatement :: ReadP [ComparisonOp]
whereStatement = do
  _ <- whereKeyword
  result <- clauseList
  _ <- many whitespace
  return result

{- Putting it all together... -}
query :: ReadP QueryType
query = do
  access <- accessL
  select <- selectStatement
  from <- fromStatement
  whr <- option [] whereStatement
  return QueryType { accessLevel = access
                   , selectList = select
                   , fromList = from
                   , whereList = whr}

isReservedWord :: String -> Bool
isReservedWord str =
  str /= "from" && str /= "select" && str /= "where" && str /= "kc"

removeReserved :: QueryType -> QueryType
removeReserved q =
  let
    second = filter isReservedWord $  selectList q
    third = filter isReservedWord $ fromList q
  in
  q { selectList = second, fromList = third}

parseQuery :: String -> Maybe QueryType
parseQuery input =
  case readP_to_S query input of
    [] -> Nothing
    list ->
      let
        result = removeReserved $ fst $ last list
        s = selectList result
        f = fromList result
      in
        if null s || null f then
          Nothing
        else
          Just result
