{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.Graph.PropertyMapSpec
    ( main
    , spec
    ) where

import           Data.Monoid                     (mempty, (<>))
import           Test.Hspec

import           Data.Greskell.Graph.PropertyMap (AProperty (..), PropertyMap (..), PropertyMapList,
                                                  PropertyMapSingle)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "PropertyMapSingle" $ do
    let pm :: PropertyMapSingle AProperty Int
        pm = putProperty (AProperty "buzz" 300)
             $ putProperty (AProperty "bar" 200)
             $ putProperty (AProperty "foo" 100) mempty
    specify "allProperties" $ do
      allProperties pm `shouldMatchList`
        [ AProperty "buzz" 300,
          AProperty "bar" 200,
          AProperty "foo" 100
        ]
    specify "lookupOne existing" $ do
      lookupOne "foo" pm `shouldBe` (Just $ AProperty "foo" 100)
    specify "lookupOne non-existing" $ do
      lookupOne "HOGE" pm `shouldBe` Nothing
    specify "lookupList existing" $ do
      lookupList "bar" pm `shouldBe` [AProperty "bar" 200]
    specify "lookupList non-existing" $ do
      lookupList "HOGE" pm `shouldBe` []
    specify "putProperty overrides" $ do
      let pm2 = putProperty (AProperty "foo" 500) pm
      lookupOne "foo" pm2 `shouldBe` Just (AProperty "foo" 500)
      lookupList "foo" pm2 `shouldBe` [AProperty "foo" 500]
    specify "removeProperty" $ do
      let pm2 = removeProperty "HOGE" $ removeProperty "bar" pm
      lookupList "bar" pm2 `shouldBe` []
    specify "mappend prefers the left" $ do
      let pm2 :: PropertyMapSingle AProperty Int
          pm2 = putProperty (AProperty "hoge" 600)
                $ putProperty (AProperty "bar" 500) mempty
          pm3 = pm <> pm2
      allProperties pm3 `shouldMatchList`
        [ AProperty "buzz" 300,
          AProperty "bar" 200,
          AProperty "foo" 100,
          AProperty "hoge" 600
        ]
  describe "PropertyMapList" $ do
    let pm :: PropertyMapList AProperty Int
        pm = putProperty (AProperty "foo" 100)
             $ putProperty (AProperty "foo" 200)
             $ putProperty (AProperty "bar" 300)
             $ putProperty (AProperty "foo" 400) mempty
    specify "allProperties" $ do
      allProperties pm `shouldMatchList`
        [ AProperty "foo" 100,
          AProperty "foo" 200,
          AProperty "bar" 300,
          AProperty "foo" 400
        ]
    specify "lookupOne existing" $ do
      lookupOne "foo" pm `shouldBe` Just (AProperty "foo" 100)
    specify "lookupOne non-existing" $ do
      lookupOne "HOGE" pm `shouldBe` Nothing
    specify "lookupList existing" $ do
      lookupList "foo" pm `shouldBe` map (AProperty "foo") [100,200,400]
    specify "lookupList non-existing" $ do
      lookupList "HOGE" pm `shouldBe` []
    specify "putProperty appends" $ do
      let pm2 = putProperty (AProperty "bar" 500) pm
      lookupOne "bar" pm2 `shouldBe` Just (AProperty "bar" 500)
      lookupList "bar" pm2 `shouldBe` map (AProperty "bar") [500,300]
    specify "removeProperty" $ do
      let pm2 = removeProperty "foo" $ removeProperty "HOGE" pm
      lookupOne "foo" pm2 `shouldBe` Nothing
      lookupList "foo" pm2 `shouldBe` []
    specify "mappend appends" $ do
      let pm2 :: PropertyMapList AProperty Int
          pm2 = putProperty (AProperty "bar" 500)
                $ putProperty (AProperty "buzz" 600)
                $ putProperty (AProperty "foo" 700) mempty
          pm3 = pm <> pm2
      lookupList "foo" pm3 `shouldBe` map (AProperty "foo") [100,200,400,700]
      lookupList "bar" pm3 `shouldBe` map (AProperty "bar") [300,500]
      allProperties pm3 `shouldMatchList`
        [ AProperty "foo" 100,
          AProperty "foo" 200,
          AProperty "foo" 400,
          AProperty "foo" 700,
          AProperty "bar" 300,
          AProperty "bar" 500,
          AProperty "buzz" 600
        ]

