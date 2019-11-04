{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.GraphSpec (main,spec) where

import Data.Aeson (toJSON, FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid (Monoid(..), (<>))
import Data.Text (Text)
import Test.Hspec

import Data.Greskell.Graph
  ( AProperty(..),
    -- PropertyMapSingle, PropertyMapList,
    AEdge(..), AVertexProperty(..), AVertex(..),
    ElementID(..)
  )
import Data.Greskell.GraphSON
  ( nonTypedGraphSON, typedGraphSON, typedGraphSON',
    nonTypedGValue, typedGValue', GValueBody(..)
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- spec_PropertyMap
  spec_AEdge
  spec_AProperty
  spec_AVertexProperty
  spec_AVertex

---- spec_PropertyMap :: Spec
---- spec_PropertyMap = do
----   describe "PropertyMapSingle" $ do
----     let pm :: PropertyMapSingle AProperty Int
----         pm = putProperty (AProperty "buzz" 300)
----              $ putProperty (AProperty "bar" 200)
----              $ putProperty (AProperty "foo" 100) mempty
----     specify "allProperties" $ do
----       allProperties pm `shouldMatchList`
----         [ AProperty "buzz" 300,
----           AProperty "bar" 200,
----           AProperty "foo" 100
----         ]
----     specify "lookupOne existing" $ do
----       lookupOne "foo" pm `shouldBe` (Just $ AProperty "foo" 100)
----     specify "lookupOne non-existing" $ do
----       lookupOne "HOGE" pm `shouldBe` Nothing
----     specify "lookupList existing" $ do
----       lookupList "bar" pm `shouldBe` [AProperty "bar" 200]
----     specify "lookupList non-existing" $ do
----       lookupList "HOGE" pm `shouldBe` []
----     specify "putProperty overrides" $ do
----       let pm2 = putProperty (AProperty "foo" 500) pm
----       lookupOne "foo" pm2 `shouldBe` Just (AProperty "foo" 500)
----       lookupList "foo" pm2 `shouldBe` [AProperty "foo" 500]
----     specify "removeProperty" $ do
----       let pm2 = removeProperty "HOGE" $ removeProperty "bar" pm
----       lookupList "bar" pm2 `shouldBe` []
----     specify "mappend prefers the left" $ do
----       let pm2 :: PropertyMapSingle AProperty Int
----           pm2 = putProperty (AProperty "hoge" 600)
----                 $ putProperty (AProperty "bar" 500) mempty
----           pm3 = pm <> pm2
----       allProperties pm3 `shouldMatchList`
----         [ AProperty "buzz" 300,
----           AProperty "bar" 200,
----           AProperty "foo" 100,
----           AProperty "hoge" 600
----         ]
----   describe "PropertyMapList" $ do
----     let pm :: PropertyMapList AProperty Int
----         pm = putProperty (AProperty "foo" 100)
----              $ putProperty (AProperty "foo" 200)
----              $ putProperty (AProperty "bar" 300)
----              $ putProperty (AProperty "foo" 400) mempty
----     specify "allProperties" $ do
----       allProperties pm `shouldMatchList`
----         [ AProperty "foo" 100,
----           AProperty "foo" 200,
----           AProperty "bar" 300,
----           AProperty "foo" 400
----         ]
----     specify "lookupOne existing" $ do
----       lookupOne "foo" pm `shouldBe` Just (AProperty "foo" 100)
----     specify "lookupOne non-existing" $ do
----       lookupOne "HOGE" pm `shouldBe` Nothing
----     specify "lookupList existing" $ do
----       lookupList "foo" pm `shouldBe` map (AProperty "foo") [100,200,400]
----     specify "lookupList non-existing" $ do
----       lookupList "HOGE" pm `shouldBe` []
----     specify "putProperty appends" $ do
----       let pm2 = putProperty (AProperty "bar" 500) pm
----       lookupOne "bar" pm2 `shouldBe` Just (AProperty "bar" 500)
----       lookupList "bar" pm2 `shouldBe` map (AProperty "bar") [500,300]
----     specify "removeProperty" $ do
----       let pm2 = removeProperty "foo" $ removeProperty "HOGE" pm
----       lookupOne "foo" pm2 `shouldBe` Nothing
----       lookupList "foo" pm2 `shouldBe` []
----     specify "mappend appends" $ do
----       let pm2 :: PropertyMapList AProperty Int
----           pm2 = putProperty (AProperty "bar" 500)
----                 $ putProperty (AProperty "buzz" 600)
----                 $ putProperty (AProperty "foo" 700) mempty
----           pm3 = pm <> pm2
----       lookupList "foo" pm3 `shouldBe` map (AProperty "foo") [100,200,400,700]
----       lookupList "bar" pm3 `shouldBe` map (AProperty "bar") [300,500]
----       allProperties pm3 `shouldMatchList`
----         [ AProperty "foo" 100,
----           AProperty "foo" 200,
----           AProperty "foo" 400,
----           AProperty "foo" 700,
----           AProperty "bar" 300,
----           AProperty "bar" 500,
----           AProperty "buzz" 600
----         ]


loadGraphSON :: FromJSON a => FilePath -> IO (Either String a)
loadGraphSON filename = fmap Aeson.eitherDecode $ BSL.readFile ("test/graphson/" ++ filename)

spec_AEdge :: Spec
spec_AEdge = describe "AEdge" $ do
  it "should parse GraphSON v1" $ do
    let expected = nonTypedGraphSON
                   $ AEdge
                   { aeId = ElementID $ nonTypedGValue $ GNumber 13,
                     aeLabel = "develops"
                   }
    loadGraphSON "edge_v1.json" `shouldReturn` Right expected
  let expected_v23 = typedGraphSON
                     $ AEdge
                     { aeId = ElementID $ typedGValue' "g:Int32" $ GNumber 13,
                       aeLabel = "develops"
                     }
  it "should parse GraphSON v2" $ do
    loadGraphSON "edge_v2.json" `shouldReturn` Right expected_v23
  it "should parse GraphSON v3" $ do
    loadGraphSON "edge_v3.json" `shouldReturn` Right expected_v23

spec_AProperty :: Spec
spec_AProperty = describe "AProperty" $ do
  it "should parse GraphSON v1 (GValue)" $ do
    let ex = nonTypedGraphSON $ AProperty "since" $ nonTypedGValue $ GNumber 2009
    loadGraphSON "property_v1.json" `shouldReturn` Right ex
  it "should parse GraphSON v1 (bare Int)" $ do
    let ex = nonTypedGraphSON $ AProperty "since" $ (2009 :: Int)
    loadGraphSON "property_v1.json" `shouldReturn` Right ex
  let ex23 = typedGraphSON $ AProperty "since" $ typedGValue' "g:Int32" $ GNumber 2009
      ex23_nowrap = typedGraphSON $ AProperty "since" $ (2009 :: Int)
  it "should parse GraphSON v2 (GValue)" $ do
    loadGraphSON "property_v2.json" `shouldReturn` Right ex23
  it "should parse GraphSON v2 (bare Int)" $ do
    loadGraphSON "property_v2.json" `shouldReturn` Right ex23_nowrap
  it "should parse GraphSON v3 (GValue)" $ do
    loadGraphSON "property_v3.json" `shouldReturn` Right ex23
  it "should parse GraphSON v3 (bare Int)" $ do
    loadGraphSON "property_v3.json" `shouldReturn` Right ex23_nowrap

spec_AVertexProperty :: Spec
spec_AVertexProperty = describe "AVertexProperty" $ do
  it "should parse GraphSON v1" $ do
    let ex = nonTypedGraphSON $
             AVertexProperty
             { avpId = ElementID $ nonTypedGValue $ GNumber 0,
               avpLabel = "name",
               avpValue = nonTypedGValue $ GString "marko"
             }
    loadGraphSON "vertex_property_v1.json" `shouldReturn` Right ex
  let ex23 = typedGraphSON $
             AVertexProperty
             { avpId = ElementID $ typedGValue' "g:Int64" $ GNumber 0,
               avpLabel = "name",
               avpValue = nonTypedGValue $ GString "marko"
             }
  it "should parse GraphSON v2" $ do
    loadGraphSON "vertex_property_v2.json" `shouldReturn` Right ex23
  it "should parse GraphSON v3" $ do
    loadGraphSON "vertex_property_v3.json" `shouldReturn` Right ex23

spec_AVertex :: Spec
spec_AVertex = describe "AVertex" $ do
  it "should parse GraphSON v1" $ do
    let ex = nonTypedGraphSON $
             AVertex
             { avId = ElementID $ nonTypedGValue $ GNumber 1,
               avLabel = "person"
             }
    loadGraphSON "vertex_v1.json" `shouldReturn` Right ex
  let ex23 = typedGraphSON $
             AVertex
             { avId = ElementID $ typedGValue' "g:Int32" $ GNumber 1,
               avLabel = "person"
             }
  it "should parse GraphSON v2" $ do
    loadGraphSON "vertex_v2.json" `shouldReturn` Right ex23
  it "should parse GraphSON v3" $ do
    loadGraphSON "vertex_v3.json" `shouldReturn` Right ex23
