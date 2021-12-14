{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Main (main,spec) where

import Test.Hspec
import Test.ShouldNotTypecheck (shouldNotTypecheck)

import Data.Greskell.GTraversal (Walk, WalkType, Filter, Transform, SideEffect, Split, gFilter, unsafeWalk, walkTypeDescription)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Split typeclass" $ do
    specify "SideEffect -> Filter" $ do
      -- putStrLn $ show (gFilter wSideEffect :: Walk Filter Int Int)
      -- putStrLn $ splitToF wSideEffect
      putStrLn $ walkTypeDescription (gFilter wSideEffect :: Walk Filter Int Int)

wSideEffect :: Walk SideEffect Int Int
wSideEffect = unsafeWalk "wSideEffect" []

splitToF :: Split c Filter => Walk c Int Int -> String
splitToF w = show w
