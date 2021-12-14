{-# OPTIONS_GHC -fdefer-type-errors #-}
module Main (main,spec) where

import Test.Hspec
import Test.ShouldNotTypecheck (shouldNotTypecheck)

import Data.Greskell.GTraversal (Walk)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Split typeclass" $ do
    True `shouldBe` False
    
