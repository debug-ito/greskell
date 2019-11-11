{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.PMapSpec (main,spec) where

import Prelude hiding (lookup)

import Data.Text (Text)
import Test.Hspec

import Data.Greskell.PMap
  ( PMap, Single, Multi,
    pMapFromList, pMapToList, pMapInsert, pMapDelete,
    lookup, lookupList
  )

main :: IO ()
main = hspec spec

-- the tests are based on PropertyMapSpec.

spec :: Spec
spec = do
  describe "PMap Single" $ do
    let pm :: PMap Single Int
        pm = pMapFromList
             [ ("buzz", 300),
               ("bar", 200),
               ("foo", 100)
             ]
    specify "pMapToList" $ do
      pMapToList pm `shouldMatchList`
        [ ("buzz", 300),
          ("bar", 200),
          ("foo", 100)
        ]
    specify "lookup existing" $ do
      lookup ("foo" :: Text) pm `shouldBe` Just 100
    specify "lookup non-existing" $ do
      lookup ("HOGE" :: Text) pm `shouldBe` Nothing
    specify "lookupList existing" $ do
      lookupList ("foo" :: Text) pm `shouldBe` [100]
    specify "lookupList non-existing" $ do
      lookupList ("HOGE" :: Text) pm `shouldBe` []
    specify "pMapInsert overrides" $ do
      let pm2 = pMapInsert "foo" 500 pm
      pMapToList pm2 `shouldMatchList`
        [ ("buzz", 300),
          ("bar", 200),
          ("foo", 500)
        ]
    specify "pMapFromList prefers the first value" $ do
      let pm2 :: PMap Single Int
          pm2 = pMapFromList
                [("foo", 100), ("foo", 200), ("foo", 300), ("bar", 400)]
      pMapToList pm2 `shouldMatchList` [("foo", 100), ("bar", 400)]
    specify "pMapDelete" $ do
      let pm2 = pMapDelete "HOGE" $ pMapDelete "bar" $ pm
      pMapToList pm2 `shouldMatchList`
        [ ("buzz", 300),
          ("foo", 100)
        ]
    specify "<> prefers the left" $ do
      let pm2 = pMapFromList [("hoge", 600), ("bar", 500)]
          pm3 = pm <> pm2
      pMapToList pm3 `shouldMatchList`
        [ ("buzz", 300),
          ("bar", 200),
          ("foo", 100),
          ("hoge", 600)
        ]
  describe "PMap Multi" $ do
    let pm :: PMap Multi Int
        pm = pMapFromList
             [ ("foo", 100),
               ("foo", 200),
               ("bar", 300),
               ("foo", 400)
             ]
    specify "pMapToList" $ do
      pMapToList pm `shouldMatchList`
        [ ("foo", 100),
          ("foo", 200),
          ("bar", 300),
          ("foo", 400)
        ]
    specify "lookup existing" $ do
      lookup ("foo" :: Text) pm `shouldBe` Just 100
    specify "lookup non-existing" $ do
      lookup ("HOGE" :: Text) pm `shouldBe` Nothing
    specify "lookupList existing" $ do
      lookupList ("foo" :: Text) pm `shouldBe` [100,200,400]
    specify "lookupList non-existing" $ do
      lookupList ("HOGE" :: Text) pm `shouldBe` []
    specify "pMapInsert prepends" $ do
      let pm2 = pMapInsert "bar" 500 pm
      pMapToList pm2 `shouldMatchList`
        [ ("bar", 500),
          ("foo", 100),
          ("foo", 200),
          ("bar", 300),
          ("foo", 400)
        ]
      lookup ("bar" :: Text) pm2 `shouldBe` Just 500
      lookupList ("bar" :: Text) pm2 `shouldBe` [500, 300]
    specify "pMapDelete" $ do
      let pm2 = pMapDelete "HOGE" $ pMapDelete "foo" pm
      pMapToList pm2 `shouldMatchList` [("bar", 300)]
      let pm3 = pMapDelete "bar" pm2
      pMapToList pm3 `shouldMatchList` []
    specify "<> appends" $ do
      let pm2 = pMapFromList
                [ ("bar", 500),
                  ("buzz", 600),
                  ("foo", 700)
                ]
          pm3 = pm <> pm2
      lookupList ("foo" :: Text) pm3 `shouldBe` [100, 200, 400, 700]
      lookupList ("bar" :: Text) pm3 `shouldBe` [300, 500]
      pMapToList pm3 `shouldMatchList`
        [ ("foo", 100),
          ("foo", 200),
          ("bar", 300),
          ("foo", 400),
          ("bar", 500),
          ("buzz", 600),
          ("foo", 700)
        ]
    
