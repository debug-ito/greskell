{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.GremlinSpec
    ( main
    , spec
    ) where

import           Test.Hspec

import           Control.Monad          (forM_)
import           Data.Greskell.Gremlin  (P, pAnd, pBetween, pGte, pNegate, pOr, pTest, pWithin)
import           Data.Greskell.Greskell (Greskell, toGremlin)
import           Data.Text              (unpack)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "P and Predicate" $ do
    specify "P and Predicate methods" $ do
      let pr :: Greskell (P Int)
          pr = pBetween 10 50 `pAnd` ((pNegate (pWithin [5, 15, 25, 35])) `pOr` pGte 20)
          expr = pr `pTest` 50
      toGremlin expr `shouldBe`
        "((P.between(10,50)).and(((P.within(5,15,25,35)).negate()).or(P.gte(20)))).test(50)"
