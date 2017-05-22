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
   (sgs !! iInd) !! pInd

updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix m x (r,c) =
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m

updateList m x c =
  (take c m) ++ x ++ (drop (c + 1) m)
  
setValue :: Int -> Int -> a -> [[a]] -> [[a]]
setValue col row newVal old =
  updateMatrix old newVal (row, col)

replaceInterval :: Polynomial -> Interval -> [Interval] -> SignTable -> SignTable
replaceInterval p i newInts st@(SignTable polys ints signs) =
  let row = fromJust $ elemIndex i ints
      col = fromJust $ elemIndex p polys in
   SignTable polys (updateList ints newInts row) signs -- fix signs too!

setSignAt :: Polynomial -> Interval -> Sign -> SignTable -> SignTable
setSignAt p i sgn st@(SignTable ps its sgs) =
  let pInd = fromJust $ elemIndex p ps
      iInd = fromJust $ elemIndex i its in
   SignTable ps its $ setValue pInd iInd sgn sgs
   
splitInterval :: Polynomial -> Interval -> SignTable -> SignTable
splitInterval p (Point _) st = st
splitInterval p i@(Range x0 x1) st =
  if rootBetween (signAt p (Point x0) st) (signAt p (Point x1) st)
  then
    let fresh = Var "x_fresh"
        r0 = Range x0 fresh
        root = Point fresh
        r1 = Range fresh x1 in
     replaceInterval p i [r0, root, r1] st
        
  else st

setPosInf p pd st =
  if (signAt pd (Point Inf) st) == Pos
     then setSignAt p (Point Inf) Pos st
  else setSignAt p (Point Inf) Neg st

setNegInf p pd st =
  if (signAt pd (Point NInf) st) == Pos
     then setSignAt p (Point NInf) Neg st
  else setSignAt p (Point NInf) Pos st

takeMiddle l = take ((length l) - 2) (drop 1 l)

removeInfinityPoints :: SignTable -> SignTable
removeInfinityPoints (SignTable polys ints signs) =
  SignTable polys (takeMiddle ints) $ takeMiddle signs 

insertInfinityPoints :: Polynomial -> Polynomial -> SignTable -> SignTable
insertInfinityPoints p pd st@(SignTable polys ints signs) =
  let atNegInfRow = head $ signs
      atPosInfRow = head $ reverse signs in
   setPosInf p pd $ setNegInf p pd $ SignTable polys ([Point NInf] ++ ints ++ [Point Inf]) ([atNegInfRow] ++ signs ++ [atPosInfRow])

splitIntervals :: Polynomial -> Polynomial -> SignTable -> SignTable
splitIntervals p pd stInit =
  let st = insertInfinityPoints p pd stInit in
   removeInfinityPoints $ L.foldr (splitInterval p) st (intervals st)

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
  let degPolys = reverse $ sortBy (\p q -> compare (deg "x" p) (deg "x" q)) polys
      p = assert (not $ isCon $ head degPolys) (head degPolys)
      ps = (derivative "x" p):(tail degPolys)
      rs = map (\pi -> snd $ divide lexOrder p [pi]) ps

      nextS = mkTable (ps ++ rs) -- Recursively build new sign table
      (pTable, rTable) = splitSignTable rs nextS in
   inferTableFor p ps rs pTable rTable

numRows (SignTable _ intervals _) = length intervals
numCols (SignTable polys _ _) = length polys
