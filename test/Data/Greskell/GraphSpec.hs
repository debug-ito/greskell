{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.GraphSpec (main,spec) where

import Data.Monoid (Monoid(..))
import Test.Hspec

import Data.Greskell.Graph
  ( AesonProperty(..), PropertyMapSingle, PropertyMapList,
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
    let pm :: PropertyMapSingle AesonProperty Int
        pm = putProperty (AesonProperty "buzz" 300)
             $ putProperty (AesonProperty "bar" 200)
             $ putProperty (AesonProperty "foo" 100) mempty
    specify "lookupOne existing" $ do
      lookupOne "foo" pm `shouldBe` (Just $ AesonProperty "foo" 100)
    specify "lookupOne non-existing" $ do
      lookupOne "HOGE" pm `shouldBe` Nothing
    specify "lookupList existing" $ do
      lookupList "bar" pm `shouldBe` [AesonProperty "bar" 200]
    specify "lookupList non-existing" $ do
      lookupList "HOGE" pm `shouldBe` []
    specify "putProperty overrides" $ do
      let pm2 = putProperty (AesonProperty "foo" 500) pm
      lookupOne "foo" pm2 `shouldBe` Just (AesonProperty "foo" 500)
      lookupList "foo" pm2 `shouldBe` [AesonProperty "foo" 500]
    specify "removeProperty" $ do
      let pm2 = removeProperty "HOGE" $ removeProperty "bar" pm
      lookupList "bar" pm2 `shouldBe` []
  describe "PropertyMapList" $ do
    let pm :: PropertyMapList AesonProperty Int
        pm = putProperty (AesonProperty "foo" 100)
             $ putProperty (AesonProperty "foo" 200)
             $ putProperty (AesonProperty "bar" 300)
             $ putProperty (AesonProperty "foo" 400) mempty
    specify "lookupOne existing" $ do
      lookupOne "foo" pm `shouldBe` Just (AesonProperty "foo" 100)
    specify "lookupOne non-existing" $ do
      lookupOne "HOGE" pm `shouldBe` Nothing
    specify "lookupList existing" $ do
      lookupList "foo" pm `shouldBe` map (AesonProperty "foo") [100,200,400]
    specify "lookupList non-existing" $ do
      lookupList "HOGE" pm `shouldBe` []
    specify "putProperty appends" $ do
      let pm2 = putProperty (AesonProperty "bar" 500) pm
      lookupOne "bar" pm2 `shouldBe` Just (AesonProperty "bar" 500)
      lookupList "bar" pm2 `shouldBe` map (AesonProperty "bar") [500,300]
    specify "removeProperty" $ do
      let pm2 = removeProperty "foo" $ removeProperty "HOGE" pm
      lookupOne "foo" pm2 `shouldBe` Nothing
      lookupList "foo" pm2 `shouldBe` []

  
