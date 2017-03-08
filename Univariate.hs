module Univariate(Monomial,
                  Polynomial,
                  mkMono, mkPoly,
                  degree, plus, minus) where

import Data.List as L

import PrettyPrint

data Monomial = Monomial Rational Integer deriving (Eq, Ord)

instance Show Monomial where
  show (Monomial c i) = "(" ++ (show c) ++ ")" ++ "x^" ++ (show i)

mkMono r i = Monomial r i

monoDegree (Monomial r i) = i

isZeroMonomial (Monomial r _) = r == 0
isNonZeroMonomial (Monomial r _) = r /= 0

data Polynomial = Polynomial [Monomial] deriving (Eq, Ord)

instance Show Polynomial where
  show (Polynomial mList) =
    if length mList == 0
    then "0"
    else sumList "+" mList
  
mkPoly monoList = Polynomial $ filter isNonZeroMonomial $  sumDegrees $ sortBy (\a b -> compare (monoDegree a) (monoDegree b)) monoList

degree (Polynomial []) = 0
degree (Polynomial monos) = monoDegree $ head $ sortBy (\a b -> compare (monoDegree a) (monoDegree b)) monos

sumMonosDegree :: [Monomial] -> Monomial
sumMonosDegree ms = mkMono (L.foldr (\(Monomial r i) sum -> r + sum) 0 ms) $ (monoDegree $ head ms)

sumDegrees :: [Monomial] -> [Monomial]
sumDegrees sortedMonos =
  let g = groupBy (\a b -> (monoDegree a) == (monoDegree b)) sortedMonos
      summed = map sumMonosDegree g in
   summed

scalarTimes c (Polynomial a) = mkPoly $ L.map (\(Monomial r i) -> mkMono (c*r) i) a

plus (Polynomial a) (Polynomial b) = mkPoly (a ++ b)

minus a b =
  plus a (scalarTimes (-1) b)
