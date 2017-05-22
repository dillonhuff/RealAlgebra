module Tests(main) where

import Test.Hspec

import Polynomial
import SignTable

main :: IO ()
main = hspec $ do
  describe "Polynomial addition" $ do

    it "add multivariate polynomials" $ do
      (plus
       (mkPoly [mkMono 2 [("x", 3), ("y", 4)]])
       (mkPoly [mkMono 5 [("x", 3)]])) `shouldBe`
        (mkPoly [mkMono 2 [("x", 3), ("y", 4)], mkMono 5 [("x", 3)]])

    it "add multivariate polynomials with simplification" $ do
      (plus
       (mkPoly [mkMono 2 [("x", 3), ("y", 4)]])
       (mkPoly [mkMono 5 [("x", 3), ("y", 4)]])) `shouldBe`
        (mkPoly [mkMono 7 [("x", 3), ("y", 4)]])

  describe "Lexicographic order" $ do
    it "x less than y" $ do
      let a = mkMono 2 [("x", 3)]
          b = mkMono 2 [("y", 4)] in
       lexOrder a b `shouldBe` LT

    it "y greater than x" $ do
      let a = mkMono 5 [("x", 3)]
          b = mkMono (-2) [("y", 4)] in
       lexOrder b a `shouldBe` GT

    it "x equal to x" $ do
      let a = mkMono 5 [("x", 2)]
          b = mkMono 2 [("x", 2)] in
       lexOrder a b `shouldBe` EQ

    it "d^3 > d^2" $ do
      let a = mkMono 3 [("l", 6), ("d", 3)]
          b = mkMono 2 [("l", 7), ("d", 2)] in
       lexOrder a b `shouldBe` GT

    it "2 < d^3" $ do
      let a = mkMono 3 [("l", 6), ("d", 3)]
          b = mkMono 2 [] in
       lexOrder b a `shouldBe` LT

  describe "isZero" $ do
    it "x^2 is not zero" $ do
      isZero (mkPoly [mkMono 1 [("x", 2)]]) `shouldBe` False

  describe "Monomial division" $ do
    it "2x | 6x is 3" $ do
      monoQuotient (mkMono 2 [("x", 1)]) (mkMono 6 [("x", 1)]) `shouldBe` (Just $ mkMono 3 [])

    it "3*x^2 does not divide 3*x^1" $ do
      monoQuotient (mkMono 3 [("x", 2)]) (mkMono 3 [("x", 1)]) `shouldBe` Nothing

    it "4 * a^3 * b^1 * d^4 * z^8 divides 3 * a^5 * b^2 * c^3 * d^4 * z^12" $ do
      monoQuotient (mkMono 4 [("a", 3), ("b", 1), ("d", 4), ("z", 8)]) (mkMono 3 [("a", 5), ("b", 2), ("c", 3), ("d", 4), ("z", 12)]) `shouldBe` (Just $ mkMono (3 / 4) [("a", 2), ("b", 1), ("c", 3), ("z", 4)])
      
  describe "Multipolynomial division" $ do
    it "x divides x^2" $ do
      let x = mkPoly $ [mkMono 1 [("x", 1)]]
          x2 = mkPoly $ [mkMono 1 [("x", 2)]] in
       divide lexOrder x2 [x] `shouldBe` ([x], mkCon 0)

    it "x divides 2 * x^2 * y" $ do
      let x = mkPoly $ [mkMono 1 [("x", 1)]]
          x2 = mkPoly $ [mkMono 2 [("x", 2), ("y", 1)]] in
       divide lexOrder x2 [x] `shouldBe` ([mkPoly [mkMono 2 [("x", 1), ("y", 1)] ] ], mkCon 0)

    it "x + y divided by [x, x + y] is ([1, 0], y)" $ do
      let xpy = mkPoly [mkMono 1 [("x", 1)], mkMono 1 [("y", 1)]]
          x = mkPoly [mkMono 1 [("x", 1)]]
          y = mkPoly [mkMono 1 [("y", 1)]] in
       divide revLexOrder xpy [x, xpy] `shouldBe` ([mkCon 1, mkCon 0], y)

    it "x + y divided by [x + y, 0] is ([1, 0], 0)" $ do
      let xpy = mkPoly [mkMono 1 [("x", 1)], mkMono 1 [("y", 1)]]
          x = mkPoly [mkMono 1 [("x", 1)]]
          y = mkPoly [mkMono 1 [("y", 1)]] in
       divide revLexOrder xpy [xpy, x] `shouldBe` ([mkCon 1, mkCon 0], mkCon 0)

  describe "Sign table tests" $ do
    it "Table for 3 table" $ do
      let tbl = mkTable [mkCon 3] in
       (numRows tbl) `shouldBe` 1

    it "Table for 3 and 4, # cols should be 2" $ do
      let tbl = mkTable [mkCon 3, mkCon 2] in
       (numCols tbl) `shouldBe` 2

    it "Table for -3x + 1" $ do
      let tbl = mkTable [ mkPoly $ [mkMono 3 [("x", 1)]] ] in
       (numRows tbl) `shouldBe` 3

-- TODO: Add some randomized tests with QuickCheck
