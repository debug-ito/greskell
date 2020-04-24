{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.GraphSpec (main,spec) where

import Data.Aeson (toJSON, FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashSet as HS
import Data.Monoid (Monoid(..), (<>))
import Data.Text (Text)
import Test.Hspec

import Data.Greskell.Graph
  ( AProperty(..),
    -- PropertyMapSingle, PropertyMapList,
    AEdge(..), AVertexProperty(..), AVertex(..),
    ElementID(..),
    Path(..), PathEntry(..)
  )
import Data.Greskell.GraphSON
  ( nonTypedGraphSON, typedGraphSON, typedGraphSON',
    nonTypedGValue, typedGValue', GValueBody(..)
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_AEdge
  spec_AProperty
  spec_AVertexProperty
  spec_AVertex
  spec_Path

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

mkEID :: Maybe Text -> GValueBody -> ElementID a
mkEID mtype vb =
  case mtype of
    Nothing -> ElementID $ nonTypedGValue vb
    Just t -> ElementID $ typedGValue' t vb

spec_Path :: Spec
spec_Path = describe "Path" $ do
  let exp_path_v1 =
        Path
        [ PathEntry (HS.fromList ["a"]) $ AVertex (mkEID Nothing $ GNumber 1) "person",
          PathEntry (HS.fromList ["b", "c"]) $ AVertex (mkEID Nothing $ GNumber 10) "software",
          PathEntry HS.empty $ AVertex (mkEID Nothing $ GNumber 11) "software"
        ]
      exp_path_v2 =
        Path
        [ PathEntry (HS.fromList ["a"]) $ AVertex (mkEID (Just "g:Int32") $ GNumber 1) "person",
          PathEntry (HS.fromList ["b", "c"]) $ AVertex (mkEID (Just "g:Int32") $ GNumber 10) "software",
          PathEntry HS.empty $ AVertex (mkEID (Just "g:Int32") $ GNumber 11) "software"
        ]
      exp_path_v3 = exp_path_v2
  it "should parse GraphSON v1" $ do
    loadGraphSON "path_v1.json" `shouldReturn` Right exp_path_v1
  it "should parse GraphSON v2" $ do
    loadGraphSON "path_v2.json" `shouldReturn` Right exp_path_v2
  it "should parse GraphSON v3" $ do
    loadGraphSON "path_v3.json" `shouldReturn` Right exp_path_v3
