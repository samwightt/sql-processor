module Executor where
import Parser(QueryType(..))
import FileParser(Table(..))

validCols :: [Table] -> [String] -> Bool
validCols t c =
  let
    tableNames = map tableName t
  in
  foldl (\acc x -> acc && x `elem` tableNames) True c

filterTables :: [Table] -> [String] -> [Table]
filterTables t c =
  filter (\a -> tableName a `elem` c) t

splitHead :: [[String]] -> ([String], [[String]])
splitHead [[]] = ([], [[]])
splitHead [] = ([], [[]])
splitHead (h : t) =
  (h, t)


mergeTables :: [[String]] -> Table -> [[String]]
mergeTables acc table =
  let
    c = tableData table
    (nHead, nTail) = splitHead c
    (oHead, oTail) = splitHead acc
  in
    (oHead ++ nHead) : [i ++ j | i <- nTail, j <- oTail]

getCrossProduct :: QueryType -> [Table] -> Either String [[String]]
getCrossProduct query tables =
  if not $ validCols tables (fromList query) then
    Left "Error: Table or tables do not exist."
  else
    Right $ foldl mergeTables [[]] $ filterTables tables $ fromList query
