{-# OPTIONS_GHC -fdefer-type-errors #-}
module Main (main,spec) where

import Data.Proxy (Proxy(..))
import Test.Hspec
import Test.ShouldNotTypecheck (shouldNotTypecheck)

import Data.Greskell.GTraversal
  ( Walk, WalkType, Filter, Transform, SideEffect, Split,
    showSplit, showWalkType
  )

main :: IO ()
main = hspec spec

pFilter :: Proxy Filter
pFilter =  Proxy

pTransform :: Proxy Transform
pTransform = Proxy

pSideEffect :: Proxy SideEffect
pSideEffect = Proxy

spec :: Spec
spec = do
  describe "Split typeclass" $ do
    let c = checkSplit
    checkSplit pFilter pFilter True
    checkSplit pSideEffect pFilter False
    -- specify "SideEffect -> Filter" $ do
    --   shouldNotTypecheck $ showSplit (Proxy :: Proxy SideEffect) (Proxy :: Proxy Filter)

checkSplit :: (WalkType c, WalkType p) => Proxy c -> Proxy p -> Bool -> Spec
checkSplit c p expOk = specify label doCheck
  where
    label = showWalkType c ++ " -> " ++ showWalkType p
    doCheck = if expOk
              then length (showSplit c p) `shouldSatisfy` (> 0)
              else shouldNotTypecheck (showSplit c p)
