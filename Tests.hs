module Tests(main) where

import Test.Hspec

import Polynomial

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
  describe "Leading coefficient" $ do
    it "Univariate, one term" $ do
      (lcof "x" $ mkPoly [mkMono 3 [("x", 4)]]) `shouldBe` mkPoly [mkMono 3 []]

    it "Multivariate, a few terms" $ do
    (lcof "x" $ mkPoly [mkMono 3 [("x", 4), ("z", 2)],
                        mkMono (-4) [("x", 2), ("y", 3)]])
      `shouldBe`
      (mkPoly [mkMono 3 [("z", 2)]])

  describe "List division" $ do
    it "Example from Holdender" $ do
      1 `shouldBe` 1

  -- describe "Division" $ do
  --   it "Two univariate polynomials" $ do
  --     divide "x" (mkPoly [mkMono 4 [("x", 2)]]) (mkPoly [mkMono 1 [("x", 1)]])
  --     `shouldBe`
  --     Just (mkPoly [mkMono 4 [("x", 1)]], zero)

  -- describe "Pseudo division" $ do

  --   it "deg(f) > deg(g)" $ do
  --     let f = mkPoly [mkMono 1 [("x", 2)], mkMono 1 [("y", 2)]]
  --         g = mkPoly [mkMono 1 [("x", 7)], mkMono (-1) [("y", 1)]] in
  --      pseudoDivide "x" f g `shouldBe` (one, zero, f)

  --   it "Multivariate, b != 1, no remainder" $ do
  --     let f = mkPoly [mkMono 9 [("z", 5), ("x", 2), ("y", 2), ("w", 4)]]
  --         g = mkPoly [mkMono 2 [("z", 1), ("x", 2), ("y", 1)]]
  --         (b, q, r) = pseudoDivide "z" f g in
  --      (times b f) `shouldBe` (plus (times q g) r)

  --   it "with remainder" $ do
  --     let f = mkPoly [mkMono 1 [("x", 2)], mkMono 1 [("y", 2)]]
  --         g = mkPoly [mkMono 1 [("x", 1)], mkMono (-1) [("y", 1)]]
  --         q = mkPoly [mkMono 1 [("x", 1)], mkMono 1 [("y", 1)]]
  --         r = mkPoly [mkMono 2 [("y", 2)]] in
  --      pseudoDivide "x" f g `shouldBe` (one, q, r)

  -- describe "Derivative" $ do

  --   it "Multivariate polynomial" $ do
  --     let f = mkPoly [mkMono 9 [("z", 5), ("x", 2), ("y", 2), ("w", 4)],
  --                     mkMono 2 [("z", 1), ("x", 2), ("y", 1)]]
  --         fp = mkPoly [mkMono 36 [("z", 5), ("x", 2), ("y", 2), ("w", 3)]] in
  --      derivative "w" f `shouldBe` fp
