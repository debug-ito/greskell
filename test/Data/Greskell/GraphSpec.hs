{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.GraphSpec (main,spec) where

import Data.Monoid (Monoid(..))
import Test.Hspec

import Data.Greskell.Graph
  ( SimpleProperty(..), PropertyMapSingle, PropertyMapList,
    PropertyMap(..)
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_PropertyMap

spec_PropertyMap :: Spec
spec_PropertyMap = do
  describe "PropertyMapSingle" $ do
    let pm :: PropertyMapSingle SimpleProperty Int
        pm = putProperty (SimpleProperty "buzz" 300)
             $ putProperty (SimpleProperty "bar" 200)
             $ putProperty (SimpleProperty "foo" 100) mempty
    specify "lookupOne existing" $ do
      lookupOne "foo" pm `shouldBe` (Just $ SimpleProperty "foo" 100)
    specify "lookupOne non-existing" $ do
      lookupOne "HOGE" pm `shouldBe` Nothing
    specify "lookupList existing" $ do
      lookupList "bar" pm `shouldBe` [SimpleProperty "bar" 200]
    specify "lookupList non-existing" $ do
      lookupList "HOGE" pm `shouldBe` []
    specify "putProperty overrides" $ do
      let pm2 = putProperty (SimpleProperty "foo" 500) pm
      lookupOne "foo" pm2 `shouldBe` Just (SimpleProperty "foo" 500)
      lookupList "foo" pm2 `shouldBe` [SimpleProperty "foo" 500]
    specify "removeProperty" $ do
      let pm2 = removeProperty "HOGE" $ removeProperty "bar" pm
      lookupList "bar" pm2 `shouldBe` []
  describe "PropertyMapList" $ do
    let pm :: PropertyMapList SimpleProperty Int
        pm = putProperty (SimpleProperty "foo" 100)
             $ putProperty (SimpleProperty "foo" 200)
             $ putProperty (SimpleProperty "bar" 300)
             $ putProperty (SimpleProperty "foo" 400) mempty
    specify "lookupOne existing" $ do
      lookupOne "foo" pm `shouldBe` Just (SimpleProperty "foo" 100)
    specify "lookupOne non-existing" $ do
      lookupOne "HOGE" pm `shouldBe` Nothing
    specify "lookupList existing" $ do
      lookupList "foo" pm `shouldBe` map (SimpleProperty "foo") [100,200,400]
    specify "lookupList non-existing" $ do
      lookupList "HOGE" pm `shouldBe` []
    specify "putProperty appends" $ do
      let pm2 = putProperty (SimpleProperty "bar" 500) pm
      lookupOne "bar" pm2 `shouldBe` Just (SimpleProperty "bar" 500)
      lookupList "bar" pm2 `shouldBe` map (SimpleProperty "bar") [500,300]
    specify "removeProperty" $ do
      let pm2 = removeProperty "foo" $ removeProperty "HOGE" pm
      lookupOne "foo" pm2 `shouldBe` Nothing
      lookupList "foo" pm2 `shouldBe` []

  
