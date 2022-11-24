{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
module Main
    ( main
    , spec
    ) where

import           Data.Proxy               (Proxy (..))
import           Test.Hspec
import           Test.ShouldNotTypecheck  (shouldNotTypecheck)

import           Data.Greskell.GTraversal (Filter, SideEffect, Split, Transform, Walk, WalkType,
                                           showLift, showSplit, showWalkType)

main :: IO ()
main = hspec spec

pF :: Proxy Filter
pF =  Proxy

pT :: Proxy Transform
pT = Proxy

pS :: Proxy SideEffect
pS = Proxy

spec :: Spec
spec = do
  describe "Split typeclass" $ do
    specify (label pF pF) $ shouldTypecheck (showSplit pF pF)
    specify (label pF pT) $ shouldTypecheck (showSplit pF pT)
    specify (label pF pS) $ shouldTypecheck (showSplit pF pS)
    specify (label pT pF) $ shouldTypecheck (showSplit pT pF)
    specify (label pT pT) $ shouldTypecheck (showSplit pT pT)
    specify (label pT pS) $ shouldTypecheck (showSplit pT pS)
    specify (label pS pF) $ shouldNotTypecheck (showSplit pS pF)
    specify (label pS pT) $ shouldNotTypecheck (showSplit pS pT)
    specify (label pS pS) $ shouldTypecheck (showSplit pS pS)
  describe "Lift typeclass" $ do
    specify (label pF pF) $ shouldTypecheck (showLift pF pF)
    specify (label pF pT) $ shouldTypecheck (showLift pF pT)
    specify (label pF pS) $ shouldTypecheck (showLift pF pS)
    specify (label pT pF) $ shouldNotTypecheck (showLift pT pF)
    specify (label pT pT) $ shouldTypecheck (showLift pT pT)
    specify (label pT pS) $ shouldTypecheck (showLift pT pS)
    specify (label pS pF) $ shouldNotTypecheck (showLift pS pF)
    specify (label pS pT) $ shouldNotTypecheck (showLift pS pT)
    specify (label pS pS) $ shouldTypecheck (showLift pS pS)


label :: (WalkType a, WalkType b) => Proxy a -> Proxy b -> String
label a b = showWalkType a ++ " -> " ++ showWalkType b

shouldTypecheck :: String -> Expectation
shouldTypecheck s = length s `shouldSatisfy` (> 0)
