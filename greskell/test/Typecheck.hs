{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main,spec) where

import Test.Hspec
import Test.ShouldNotTypecheck (shouldNotTypecheck)

import Data.Greskell.GTraversal (Walk, WalkType, Filter, Transform, SideEffect, gFilter, unsafeWalk)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Split typeclass" $ do
    specify "SideEffect -> Filter" $ do
      putStrLn $ show ((gFilter :: Walk SideEffect Int Int -> Walk Filter Int Int) anyWalk)


anyWalk :: WalkType c => Walk c Int Int
anyWalk = unsafeWalk "foo" []
