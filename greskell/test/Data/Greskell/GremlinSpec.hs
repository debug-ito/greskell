{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.GremlinSpec (main,spec) where

import Test.Hspec

import Data.Greskell.Gremlin
  ( pBetween, pAnd, pOr, pNegate, pWithin, pGte, pTest
  )
import Data.Greskell.Greskell (toGremlin, Greskell)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "P and Predicate" $ do
    specify "P and Predicate methods" $ do
      let expr = (pBetween (10 :: Greskell Int) 50 `pAnd` ((pNegate (pWithin [5, 15, 25, 35])) `pOr` pGte 20)) `pTest` 50
      toGremlin expr `shouldBe`
        "((P.between(10,50)).and(((P.within(5,15,25,35)).negate()).or(P.gte(20)))).test(50)"
