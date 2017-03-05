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

    it "d^3 > d^2" $ do
      let a = mkMono 3 [("l", 6), ("d", 3)]
          b = mkMono 2 [("l", 7), ("d", 2)] in
       lexOrder a b `shouldBe` GT

    it "2 < d^3" $ do
      let a = mkMono 3 [("l", 6), ("d", 3)]
          b = mkMono 2 [] in
       lexOrder b a `shouldBe` LT

  describe "Multipolynomial division" $ do
    -- it "x divides x^2" $ do
    --   let x = mkPoly $ [mkMono 1 [("x", 1)]]
    --       x2 = mkPoly $ [mkMono 1 [("x", 2)]] in
    --    divide lexOrder x2 [x] `shouldBe` ([x], mkCon 0)

    -- it "x divides 2 * x^2 * y" $ do
    --   let x = mkPoly $ [mkMono 1 [("x", 1)]]
    --       x2 = mkPoly $ [mkMono 2 [("x", 2), ("y", 1)]] in
    --    divide lexOrder x2 [x] `shouldBe` ([mkPoly [mkMono 2 [("x", 1), ("y", 1)] ] ], mkCon 0)

  describe "Monomial division" $ do
    it "2x | 6x is 3" $ do
      monoQuotient (mkMono 2 [("x", 1)]) (mkMono 6 [("x", 1)]) `shouldBe` (Just $ mkMono 3 [])

    it "3*x^2 does not divide 3*x^1" $ do
      monoQuotient (mkMono 3 [("x", 2)]) (mkMono 3 [("x", 1)]) `shouldBe` Nothing

  -- describe "Leading coefficient" $ do
  --   it "Univariate, one term" $ do
  --     (lcof "x" $ mkPoly [mkMono 3 [("x", 4)]]) `shouldBe` mkPoly [mkMono 3 []]

  --   it "Multivariate, a few terms" $ do
  --   (lcof "x" $ mkPoly [mkMono 3 [("x", 4), ("z", 2)],
  --                       mkMono (-4) [("x", 2), ("y", 3)]])
  --     `shouldBe`
  --     (mkPoly [mkMono 3 [("z", 2)]])

