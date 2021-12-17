{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Main (main,spec) where

import Data.Proxy (Proxy(..))
import Test.Hspec
import Test.ShouldNotTypecheck (shouldNotTypecheck)

import Data.Greskell.GTraversal (Walk, WalkType, Filter, Transform, SideEffect, Split, showSplit)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Split typeclass" $ do
    specify "SideEffect -> Filter" $ do
      shouldNotTypecheck $ showSplit (Proxy :: Proxy SideEffect) (Proxy :: Proxy Filter)
