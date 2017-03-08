module Polynomial(Polynomial,
                  Monomial,
                  mkCon, mkPoly, mkMono, one, zero,
                  isCon, getCon, isZero,
                  deg, tryMonicize,
                  lcof, isPos,
                  deleteLcof, monoQuotient,
                  plus, times, divide,
                  lexOrder, revLexOrder) where

import Control.Exception.Base
import Data.List as L
import Data.Map as M
import Data.Maybe as Mb
import Data.Set as S

import PrettyPrint

data Monomial = Monomial Rational (Map String Integer)
                deriving (Eq, Ord)

mkMono :: Rational -> [(String, Integer)] -> Monomial
mkMono i vars = Monomial i (M.fromList $ L.filter (\(_, c) -> c /= 0) vars)

lexOrderStrings :: [(String, Integer)] -> [(String, Integer)] -> Ordering
lexOrderStrings [] [] = EQ
lexOrderStrings [] _ = LT
lexOrderStrings _ [] = GT
lexOrderStrings ((a, an):as) ((b, bn):bs) =
  if a == b then
    if an == bn then
      lexOrderStrings as bs
    else compare an bn
  else compare a b

lexOrder :: Monomial -> Monomial -> Ordering
lexOrder (Monomial _ a) (Monomial _ b) =
  let aVals = sortBy (\(a, _) (b, _) -> compare a b) $ M.toList a
      bVals = sortBy (\(a, _) (b, _) -> compare a b) $ M.toList b in
   lexOrderStrings aVals bVals

revLexOrder :: Monomial -> Monomial -> Ordering
revLexOrder (Monomial _ a) (Monomial _ b) =
  let aVals = sortBy (\(a, _) (b, _) -> compare b a) $ M.toList a
      bVals = sortBy (\(a, _) (b, _) -> compare b a) $ M.toList b in
   lexOrderStrings aVals bVals

mkMonoMap i vars = Monomial i (M.filter (\c -> c /= 0) vars)

monoCoeff (Monomial c _) = c

evenPowers (Monomial _ m) = L.all (\(_, i) -> even i) $ M.toList m

vars :: Monomial -> Set String
vars (Monomial _ vs) = M.keysSet vs

monomialTimes :: Rational -> Monomial -> Monomial
monomialTimes i (Monomial c ms) = mkMonoMap (i*c) ms

monomialProd :: Monomial -> Monomial -> Monomial
monomialProd (Monomial c1 m1) (Monomial c2 m2) =
  mkMonoMap (c1*c2) $ M.unionWith (\i j -> i + j) m1 m2

deleteVar :: String -> Monomial -> Monomial
deleteVar x (Monomial c vars) = mkMonoMap c $ M.delete x vars

setMonoDegree var d (Monomial c vars) =
  mkMonoMap c $ M.insert var d vars

monoDegree :: String -> Monomial -> Integer
monoDegree var (Monomial _ vars) =
  case M.lookup var vars of
   Just d -> d
   Nothing -> 0

instance Show Monomial where
  show (Monomial c vars) =
    let varList = M.toList vars in
     if c /= 1 && (length varList) /= 0
     then show c ++ "*" ++ (printVars $ M.toList vars)
     else if length varList == 0
          then show c
          else (printVars $ M.toList vars)

printVars :: [(String, Integer)] -> String
printVars [] = ""
printVars ((x, p):[]) = showExp x p
printVars ((x, p):(y, q):rest) =
  (showExp x p) ++ "*" ++
  (showExp y q) ++ (printVars rest)

showExp x 1 = x
showExp x p = x ++ "^" ++ show p

data Polynomial = Polynomial (Set Monomial)
                  deriving (Eq, Ord)

instance Show Polynomial where
  show (Polynomial rs) =
    let mList = S.toList rs in
     if length mList == 0
     then "0"
     else sumList "+" $ S.toList rs

isCon p = (S.size $ varSet p) == 0

one = mkPoly [mkMono 1 []]
zero = mkPoly [mkMono 0 []]

mkPoly :: [Monomial] -> Polynomial
mkPoly monomials = Polynomial $ S.fromList $ L.filter (\m -> monoCoeff m /= 0) monomials

mkCon :: Rational -> Polynomial
mkCon i = mkPoly [mkMono i []]

mkPolyS :: Set Monomial -> Polynomial
mkPolyS monomials = Polynomial $ S.filter (\m -> monoCoeff m /= 0) monomials

deleteLcof var p@(Polynomial ms) =
  Polynomial $ S.filter (\m -> monoDegree var m < deg var p) ms

timesInt :: Rational -> Polynomial -> Polynomial
timesInt i (Polynomial ms) = Polynomial (S.map (\m -> monomialTimes i m) ms)

times :: Polynomial -> Polynomial -> Polynomial
times (Polynomial ps) (Polynomial qs) =
  let p = S.toList ps
      q = S.toList qs in
   mkPoly $ simplify $ L.concatMap (\m -> L.map (\t -> monomialProd m t) q) p

plus :: Polynomial -> Polynomial -> Polynomial
plus (Polynomial m1) (Polynomial m2) = mkPoly $ simplify ((S.toList m1) ++ (S.toList m2))

minus :: Polynomial -> Polynomial -> Polynomial
minus f g = plus f (timesInt (-1) g)

simplify :: [Monomial] -> [Monomial]
simplify ms =
  let sorted = sortBy (\(Monomial _ m1) (Monomial _ m2) -> compare m1 m2) ms
      grouped = groupBy (\(Monomial _ m1) (Monomial _ m2) -> m1 == m2) sorted
      mPlus = (\(Monomial c1 m1) (Monomial c2 _) -> Monomial (c1 + c2) m1)
      results = L.map (\terms -> L.foldr mPlus (L.head terms) (L.tail terms)) grouped in
   results

pow :: Polynomial -> Integer -> Polynomial
pow p 0 = one
pow p 1 = p
pow p n = pow (times p p) (n-1)

lcof :: String -> Polynomial -> Polynomial
lcof var p@(Polynomial ts) = Polynomial $ S.map (deleteVar var) $ S.filter (\m -> (monoDegree var m) == (deg var p)) ts

deg :: String -> Polynomial -> Integer
deg var (Polynomial ts) =
  if length ts == 0
  then 0
  else L.maximum $ S.toList $ S.map (\m -> monoDegree var m) ts

varSet (Polynomial ts) = S.unions $ L.map vars $ S.toList ts

nextVar f g =
  let vars = S.union (varSet f) (varSet g) in
  case S.size vars of
   0 -> Nothing
   _ -> Just $ S.findMax vars

getCon :: Polynomial -> Rational
getCon p@(Polynomial m) =
  if S.size m == 0
  then 0
  else monoCoeff $ S.findMax m

isZero p@(Polynomial s) = ((length s) == 0) || ((isCon p) && ((monoCoeff $ L.head $ S.toList s) == 0))

isPos :: Polynomial -> Bool
isPos (Polynomial monos) = L.all (\m -> (monoCoeff m) > 0 && (evenPowers m)) monos

-- Very weak monicization for the simplifier
tryMonicize p@(Polynomial m) =
  case S.toList m of
   [Monomial c m] -> mkPoly [mkMonoMap 1 m]
   m -> p
  
divide :: (Monomial -> Monomial -> Ordering) ->
          Polynomial ->
          [Polynomial] ->
          ([Polynomial], Polynomial)
divide monomialOrder g fs =
  let as = replicate (length fs) (mkCon 0) in
   rDivide monomialOrder (DivState g as fs)

lt :: (Monomial -> Monomial -> Ordering) -> Polynomial -> Monomial
lt monomialOrder (Polynomial s) =
  let monos = S.toList s in
   assert ((length monos) > 0) (head $ sortBy monomialOrder monos)

data DivState = DivState Polynomial [Polynomial] [Polynomial]
                deriving (Eq, Ord, Show)

appendNextVar :: Monomial ->
                 Monomial ->
                 String ->
                 Maybe [(String, Integer)] ->
                 Maybe [(String, Integer)]
appendNextVar _ _ _ Nothing = Nothing
appendNextVar divisor dividend var (Just varList) =
  let aDeg = monoDegree var divisor
      bDeg = monoDegree var dividend in
   if aDeg > bDeg then Nothing
   else Just $ (var, bDeg - aDeg):varList

quotientPowers :: Monomial -> Monomial -> Maybe [(String, Integer)]
quotientPowers divisor dividend =
  let allVars = S.toList $ S.union (vars divisor) (vars dividend) in
   L.foldr (appendNextVar divisor dividend) (Just []) allVars

monoQuotient :: Monomial -> Monomial -> Maybe Monomial
monoQuotient divisor dividend =
  let ac = monoCoeff divisor
      bc = monoCoeff dividend
      c = bc / ac
      varPowsM = quotientPowers divisor dividend in
   case varPowsM of
    Just varPows -> Just $ mkMono c varPows
    Nothing -> Nothing

type MonomialOrder = Monomial -> Monomial -> Ordering

dropUnreducible :: MonomialOrder -> DivState -> Maybe Int
dropUnreducible monomialOrder (DivState r _ fs) =
  if isZero r then Nothing
  else
    let reduces = \f -> ((monoQuotient (lt monomialOrder f) (lt monomialOrder r))) /= Nothing
        reduceInd = L.findIndex reduces fs in
     reduceInd

rDivide :: (Monomial -> Monomial -> Ordering) ->
           DivState ->
           ([Polynomial], Polynomial)
rDivide monomialOrder ds@(DivState r as fs) =
  case dropUnreducible monomialOrder ds of
   Nothing -> (as, r)
   Just i ->
     let ak = head $ drop i as
         fk = head $ drop i fs
         q = fromJust $ monoQuotient (lt monomialOrder fk) (lt monomialOrder r)
         newA = plus ak (mkPoly [q])
         newR = minus r (times (mkPoly [q]) fk)
         lastA = drop (i + 1) as
         oldA = take i as in
      (oldA ++ [newA] ++ lastA, newR)

   
x = mkPoly $ [mkMono 1 [("x", 1)]]
x2 = mkPoly $ [mkMono 1 [("x", 2)]]
