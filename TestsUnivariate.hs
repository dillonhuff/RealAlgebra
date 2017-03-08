module TestsUnivariate(main) where

import Test.Hspec

import Univariate

main :: IO ()
main = hspec $ do
  describe "Univariate polynomials" $ do

    it "x^3 has degree 3" $ do
      (degree $ mkPoly [mkMono 1 3]) `shouldBe` 3

    it "x^1 + 3x^3 + 3*x^1 is 4x^1 + 3x^3" $ do
      let correct = mkPoly [mkMono 4 1, mkMono 3 3]
          result = mkPoly [mkMono 1 1, mkMono 3 3, mkMono 3 1] in
       result `shouldBe` correct

    it "(2*x^4 + 3*x^5) - (2*x^4 + 5*x^7) is (3*x^5 - 5*x^7)" $ do
      let a = mkPoly [mkMono 2 4, mkMono 3 5]
          b = mkPoly [mkMono 2 4, mkMono 5 7]
          correct = mkPoly [mkMono 3 5, mkMono (-5) 7] in
       (minus a b) `shouldBe` correct
