module Univariate(Monomial,
                  Polynomial,
                  mkMono, mkPoly) where

import Data.List as L

import PrettyPrint

data Monomial = Monomial Rational Integer deriving (Eq, Ord)

instance Show Monomial where
  show (Monomial c i) = "(" ++ (show c) ++ ")" ++ "x^" ++ (show i)

mkMono r i = Monomial r i

monoDegree (Monomial r i) = i

data Polynomial = Polynomial [Monomial] deriving (Eq, Ord)

instance Show Polynomial where
  show (Polynomial mList) =
    if length mList == 0
    then "0"
    else sumList "+" mList
  
mkPoly monoList = Polynomial $ sumDegrees $ sortBy (\a b -> compare (monoDegree a) (monoDegree b)) monoList

degree (Polynomial []) = 0
degree (Polynomial monos) = monoDegree $ head $ sortBy (\a b -> compare (monoDegree a) (monoDegree b)) monos

sumMonosDegree :: [Monomial] -> Monomial
sumMonosDegree ms = mkMono (L.foldr (\(Monomial r i) sum -> r + sum) 0 ms) $ (monoDegree $ head ms)

sumDegrees :: [Monomial] -> [Monomial]
sumDegrees sortedMonos =
  let g = groupBy (\a b -> (monoDegree a) == (monoDegree b)) sortedMonos
      summed = map sumMonosDegree g in
   summed
