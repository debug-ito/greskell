{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.GraphSpec (main,spec) where

import Data.Aeson (toJSON, FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Monoid (Monoid(..), (<>))
import Data.Text (Text)
import Test.Hspec

import Data.Greskell.AsLabel (AsLabel(..))
import Data.Greskell.Graph
  ( AProperty(..),
    -- PropertyMapSingle, PropertyMapList,
    AEdge(..), AVertexProperty(..), AVertex(..),
    ElementID(..),
    Path(..), PathEntry(..), pathToPMap
  )
import Data.Greskell.GraphSON
  ( nonTypedGraphSON, typedGraphSON, typedGraphSON',
    nonTypedGValue, typedGValue', GValueBody(..)
  )
import Data.Greskell.PMap (pMapFromList, lookupList)

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

mkLabels :: [Text] -> HashSet (AsLabel a)
mkLabels = HS.fromList . map AsLabel

mkPE :: [Text] -> a -> PathEntry a
mkPE labels obj = PathEntry (mkLabels labels) obj

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
  describe "pathToPMap" $ do
    specify "empty path" $ do
      let p :: Path Int
          p = mempty
      (pathToPMap p) `shouldBe` mempty
    specify "objects without labels" $ do
      let p :: Path Int
          p = Path [ PathEntry mempty 10,
                     PathEntry mempty 20,
                     PathEntry mempty 30
                   ]
      (pathToPMap p) `shouldBe` mempty
    specify "objects with unique single label" $ do
      let p :: Path Int
          p = Path
              [ mkPE ["a"] 10,
                mkPE ["b"] 20,
                mkPE [] 30,
                mkPE ["c"] 40
              ]
          expected = pMapFromList
                     [ ("a", 10), ("b", 20), ("c", 40)
                     ]
      (pathToPMap p) `shouldBe` expected
    specify "object with multiple labels" $ do
      let p :: Path Int
          p = Path
              [ mkPE ["a", "b", "c"] 10,
                mkPE ["d", "e"] 20,
                mkPE [] 30
              ]
          expected = pMapFromList
                     [ ("a", 10), ("b", 10), ("c", 10), ("d", 20), ("e", 20)
                     ]
      (pathToPMap p) `shouldBe` expected
    specify "object with shared labels" $ do
      let p :: Path Int
          p = Path
              [ mkPE ["a", "b", "c"] 10,
                mkPE ["b", "c", "d"] 20,
                mkPE ["d"] 30,
                mkPE [] 40,
                mkPE ["b"] 50
              ]
          got = pathToPMap p
          expected = pMapFromList
                     [ ("a", 10), ("b", 10), ("c", 10),
                       ("b", 20), ("c", 20), ("d", 20),
                       ("d", 30),
                       ("b", 50)
                     ]
      got `shouldBe` expected
      (lookupList ("a" :: AsLabel Int) got) `shouldBe` [10]
      (lookupList ("b" :: AsLabel Int) got) `shouldBe` [10, 20, 50]
      (lookupList ("c" :: AsLabel Int) got) `shouldBe` [10, 20]
      (lookupList ("d" :: AsLabel Int) got) `shouldBe` [20, 30]
      (lookupList ("e" :: AsLabel Int) got) `shouldBe` []
