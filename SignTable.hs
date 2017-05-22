module SignTable(SignTable,
                 mkTable,
                 numRows, numCols) where

import Polynomial

data Interval =
  Point String |
  Range String String deriving (Eq, Ord, Show)

data SignTable = SignTable [Polynomial] [Interval] deriving (Eq, Ord, Show)

mkTable polys =
  if all (\p -> isCon p) polys
     then constantSignTable polys
  else recursiveSignTable polys

constantSignTable polys = SignTable polys []

recursiveSignTable polys = SignTable polys []

numRows (SignTable _ intervals) = length intervals
numCols (SignTable polys _) = length polys


