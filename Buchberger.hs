module Buchberger() where

import Data.Maybe as Mb

import Polynomial

buchberger :: MonomialOrder ->
              [Polynomial] ->
              [Polynomial]
buchberger monomialOrder fs =
  let g = fs
      pairs = [(fi, fj) | fi <- fs, fj <- fs, fi /= fj] in
   recursiveBuchberger monomialOrder  g pairs

sPolynomial monomialOrder f g =
  let x = monoLCM (lt monomialOrder f) (lt monomialOrder g)
      ft = mkPoly [fromJust $ monoQuotient (lt monomialOrder f) x]
      gt = mkPoly [fromJust $ monoQuotient (lt monomialOrder g) x]
      fs = times ft f
      gs = times gt g in
   minus fs gs

recursiveBuchberger :: MonomialOrder ->
                       [Polynomial] ->
                       [(Polynomial, Polynomial)] ->
                       [Polynomial]
recursiveBuchberger monomialOrder basis [] = basis
recursiveBuchberger monomialOrder basis ((f, g):gs) =
  let sfg = sPolynomial monomialOrder f g
      sfgl = snd $ divide monomialOrder sfg basis in
   case isZero sfgl of
    True -> recursiveBuchberger monomialOrder basis gs
    False -> recursiveBuchberger monomialOrder (sfgl:basis) (gs ++ [(fi, sfgl) | fi <- basis])

f1 = mkPoly [mkMono 1 [("y", 2)], mkMono 1 [("x", 1), ("y", 1)], mkMono 1 [("x", 2)]]
f2 = mkPoly [mkMono 1 [("y", 1)], mkMono 1 [("x", 1)]]
f3 = mkPoly [mkMono 1 [("y", 1)]]
