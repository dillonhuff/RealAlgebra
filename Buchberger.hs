module Buchberger() where

import Polynomial

buchberger :: MonomialOrder ->
              [Polynomial] ->
              [Polynomial]
buchberger monomialOrder fs =
  let g = fs
      pairs = [(fi, fj) | fi <- fs, fj <- fs, fi /= fj] in
   recursiveBuchberger monomialOrder  g pairs

sPolynomial monomialOrder f g = f
--  let x = lcm f g in 

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
