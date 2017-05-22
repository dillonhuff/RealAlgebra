module SignTable(SignTable,
                 mkTable,
                 numRows, numCols) where

import Control.Exception.Base
import Data.List as L
import Data.Maybe

import Polynomial

data Val = Var String | Inf | NInf deriving (Eq, Ord, Show)

data Interval =
  Point Val |
  Range Val Val deriving (Eq, Ord, Show)

data Sign = Neg | Zero | Pos deriving (Eq, Ord, Show)

data SignTable = SignTable [Polynomial] [Interval] [[Sign]] deriving (Eq, Ord, Show)

intervals (SignTable _ its _) = its

tablePolys (SignTable ps _ _) = ps

deleteN :: Int -> [a] -> [a]
deleteN _ []     = []
deleteN i (a:as)
   | i == 0    = as
   | otherwise = a : deleteN (i-1) as

deleteColumn :: Polynomial -> SignTable -> SignTable
deleteColumn p (SignTable ps is signRows) =
  let pInd = fromJust $ elemIndex p ps
      newRows = L.map (deleteN pInd) signRows in
   SignTable (deleteN pInd ps) is newRows

deleteColumns :: [Polynomial] -> SignTable -> SignTable
deleteColumns ps table =
  L.foldr deleteColumn table ps

mkTable polys =
  if all (\p -> isCon p) polys
     then constantSignTable polys
  else recursiveSignTable polys

univariateSign :: Polynomial -> Sign
univariateSign p =
  if (getCon p) > 0 then Pos else if (getCon p) < 0 then Neg else Zero

signList :: Int -> Polynomial -> [Sign]
signList i p = replicate i (univariateSign p)

constantRows :: [Polynomial] -> [[Sign]]
constantRows ps = [L.map univariateSign ps]

constantSignTable polys =
  SignTable polys [Range NInf Inf] $ constantRows polys

insertCol p signs (SignTable ps intervals rows) =
  SignTable (ps ++ [p]) intervals (L.zipWith (\row sign -> row ++ [sign]) rows signs)

rootBetween :: Sign -> Sign -> Bool
rootBetween Pos Neg = True
rootBetween Neg Pos = True
rootBetween _ _ = False

signAt :: Polynomial -> Interval -> SignTable -> Sign
signAt p i st@(SignTable ps its sgs) =
  let pInd = fromJust $ elemIndex p ps
      iInd = fromJust $ elemIndex i its in
   error $ show $ st--(sgs !! iInd) !! pInd
  
splitInterval :: Polynomial -> Interval -> SignTable -> SignTable
splitInterval p (Point _) st = st
splitInterval p (Range x0 x1) st =
  if rootBetween (signAt p (Point x0) st) (signAt p (Point x1) st) then st else st

splitIntervals :: Polynomial -> Polynomial -> SignTable -> SignTable
splitIntervals p pd st =
  L.foldr (splitInterval p) st (intervals st)

inferCol p ps rs pTable rTable =
  let signs = L.replicate (numRows pTable) Neg in --colSigns p ps rs pTable rTable in
   splitIntervals p (head ps) $ insertCol p signs pTable

inferTableFor p ps rs pTable rTable =
  let newSt = inferCol p ps rs pTable rTable in
   newSt

splitSignTable toExtract table =
  (deleteColumns toExtract table,
   deleteColumns ((tablePolys table) \\ toExtract) table)
  
-- Note: Assumes x is the only variable since for now we are doing the univariate
-- case
recursiveSignTable polys =
  let degPolys = sortBy (\p q -> compare (deg "x" p) (deg "x" q)) polys
      p = assert (not $ isCon $ head degPolys) (head degPolys)
      ps = (derivative "x" p):(tail degPolys)
      rs = map (\pi -> snd $ divide lexOrder p [pi]) ps

      nextS = mkTable (ps ++ rs) -- Recursively build new sign table
      (pTable, rTable) = splitSignTable rs nextS in
   inferTableFor p ps rs pTable rTable

numRows (SignTable _ intervals _) = length intervals
numCols (SignTable polys _ _) = length polys
