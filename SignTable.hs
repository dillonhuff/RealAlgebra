module SignTable(SignTable,
                 mkTable,
                 numRows, numCols) where

import Data.List as L

import Polynomial

data Val = Var String | Inf | NInf deriving (Eq, Ord, Show)

data Interval =
  Point Val |
  Range Val Val deriving (Eq, Ord, Show)

data Sign = Neg | Zero | Pos deriving (Eq, Ord, Show)

data SignTable = SignTable [Polynomial] [Interval] [[Sign]] deriving (Eq, Ord, Show)

tablePolys (SignTable ps _ _) = ps

deleteColumn :: Polynomial -> SignTable -> SignTable
deleteColumn p st =
  st

deleteColumns :: [Polynomial] -> SignTable -> SignTable
deleteColumns ps table =
  L.foldr deleteColumn table ps

mkTable polys =
  if all (\p -> isCon p) polys
     then constantSignTable polys
  else recursiveSignTable polys

univariateSign :: Polynomial -> Sign
univariateSign p = if (getCon p) > 0 then Pos else if (getCon p) < 0 then Neg else Zero

signList :: Int -> Polynomial -> [Sign]
signList i p = replicate i (univariateSign p)

constantRows :: [Polynomial] -> [[Sign]]
constantRows ps = [L.map univariateSign ps]

constantSignTable polys =
  SignTable polys [Range NInf Inf] $ constantRows polys

inferTableFor p ps rs pTable rTable = SignTable [p] [] []

splitSignTable toExtract table =
  (deleteColumns toExtract table,
   deleteColumns ((tablePolys table) \\ toExtract) table)
  
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

numRows (SignTable _ intervals _) = length intervals
numCols (SignTable polys _ _) = length polys


