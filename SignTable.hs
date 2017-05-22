module SignTable(SignTable,
                 mkTable,
                 numRows, numCols) where

import Data.List

import Polynomial

data Val = Var String | Inf | NInf deriving (Eq, Ord, Show)

data Interval =
  Point Val |
  Range Val Val deriving (Eq, Ord, Show)

data SignTable = SignTable [Polynomial] [Interval] deriving (Eq, Ord, Show)

mkTable polys =
  if all (\p -> isCon p) polys
     then constantSignTable polys
  else recursiveSignTable polys

constantSignTable polys =
  SignTable polys [Range NInf Inf]

inferTableFor p ps rs pTable rTable = SignTable [p] []

splitSignTable toExtract table = (table, table)
  

-- Note: Assumes x is the only variable since for now we are doing the univariate
-- case
recursiveSignTable polys =
  let degPolys = sortBy (\p q -> compare (deg "x" p) (deg "x" q)) polys
      p = head degPolys
      ps = (derivative "x" p):(tail degPolys)
      rs = map (\pi -> snd $ divide lexOrder p [pi]) ps

      nextS = mkTable (ps ++ rs) -- Recursively build new sign table
      (pTable, rTable) = splitSignTable rs nextS in
   inferTableFor p ps rs pTable rTable

numRows (SignTable _ intervals) = length intervals
numCols (SignTable polys _) = length polys


