{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.GraphSpec (main,spec) where

import Data.Aeson (toJSON, FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid (Monoid(..), (<>))
import Data.Text (Text)
import Test.Hspec

import Data.Greskell.Graph
  ( SimpleProperty(..), PropertyMapSingle, PropertyMapList,
    PropertyMap(..),
    AEdge(..), AVertexProperty(..), AVertex(..)
  )
import Data.Greskell.GraphSON
  ( nonTypedGraphSON, typedGraphSON, typedGraphSON'
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_PropertyMap
  spec_AEdge
  spec_SimpleProperty
  spec_AVertexProperty
  spec_AVertex

spec_PropertyMap :: Spec
spec_PropertyMap = do
  describe "PropertyMapSingle" $ do
    let pm :: PropertyMapSingle SimpleProperty Int
        pm = putProperty (SimpleProperty "buzz" 300)
             $ putProperty (SimpleProperty "bar" 200)
             $ putProperty (SimpleProperty "foo" 100) mempty
    specify "allProperties" $ do
      allProperties pm `shouldMatchList`
        [ SimpleProperty "buzz" 300,
          SimpleProperty "bar" 200,
          SimpleProperty "foo" 100
        ]
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
    specify "mappend prefers the left" $ do
      let pm2 :: PropertyMapSingle SimpleProperty Int
          pm2 = putProperty (SimpleProperty "hoge" 600)
                $ putProperty (SimpleProperty "bar" 500) mempty
          pm3 = pm <> pm2
      allProperties pm3 `shouldMatchList`
        [ SimpleProperty "buzz" 300,
          SimpleProperty "bar" 200,
          SimpleProperty "foo" 100,
          SimpleProperty "hoge" 600
        ]
  describe "PropertyMapList" $ do
    let pm :: PropertyMapList SimpleProperty Int
        pm = putProperty (SimpleProperty "foo" 100)
             $ putProperty (SimpleProperty "foo" 200)
             $ putProperty (SimpleProperty "bar" 300)
             $ putProperty (SimpleProperty "foo" 400) mempty
    specify "allProperties" $ do
      allProperties pm `shouldMatchList`
        [ SimpleProperty "foo" 100,
          SimpleProperty "foo" 200,
          SimpleProperty "bar" 300,
          SimpleProperty "foo" 400
        ]
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
    specify "mappend appends" $ do
      let pm2 :: PropertyMapList SimpleProperty Int
          pm2 = putProperty (SimpleProperty "bar" 500)
                $ putProperty (SimpleProperty "buzz" 600)
                $ putProperty (SimpleProperty "foo" 700) mempty
          pm3 = pm <> pm2
      lookupList "foo" pm3 `shouldBe` map (SimpleProperty "foo") [100,200,400,700]
      lookupList "bar" pm3 `shouldBe` map (SimpleProperty "bar") [300,500]
      allProperties pm3 `shouldMatchList`
        [ SimpleProperty "foo" 100,
          SimpleProperty "foo" 200,
          SimpleProperty "foo" 400,
          SimpleProperty "foo" 700,
          SimpleProperty "bar" 300,
          SimpleProperty "bar" 500,
          SimpleProperty "buzz" 600
        ]


loadGraphSON :: FromJSON a => FilePath -> IO (Either String a)
loadGraphSON filename = fmap Aeson.eitherDecode $ BSL.readFile ("test/graphson/" ++ filename)

spec_AEdge :: Spec
spec_AEdge = describe "AEdge" $ do
  it "should parse GraphSON v1" $ do
    let expected = nonTypedGraphSON
                   $ AEdge
                   { aeId = nonTypedGraphSON $ toJSON (13 :: Int),
                     aeLabel = "develops",
                     aeInVLabel = "software",
                     aeOutVLabel = "person",
                     aeInV = nonTypedGraphSON $ toJSON (10 :: Int),
                     aeOutV = nonTypedGraphSON $ toJSON (1 :: Int),
                     aeProperties = putProperty
                                    (SimpleProperty "since" $ nonTypedGraphSON $ toJSON (2009 :: Int))
                                    $ mempty
                   }
    loadGraphSON "edge.v1.json" `shouldReturn` Right expected
  let expected_v23 = typedGraphSON
                     $ AEdge
                     { aeId = typedGraphSON' "g:Int32" $ toJSON (13 :: Int),
                       aeLabel = "develops",
                       aeInVLabel = "software",
                       aeOutVLabel = "person",
                       aeInV = typedGraphSON' "g:Int32" $ toJSON (10 :: Int),
                       aeOutV = typedGraphSON' "g:Int32" $ toJSON (1 :: Int),
                       aeProperties = putProperty
                                      (SimpleProperty "since" $ typedGraphSON' "g:Int32" $ toJSON (2009 :: Int))
                                      $ mempty
                     }
  it "should parse GraphSON v2" $ do
    loadGraphSON "edge.v2.json" `shouldReturn` Right expected_v23
  it "should parse GraphSON v3" $ do
    loadGraphSON "edge.v3.json" `shouldReturn` Right expected_v23

spec_SimpleProperty :: Spec
spec_SimpleProperty = describe "SimpleProperty" $ do
  it "should parse GraphSON v1" $ do
    let ex = nonTypedGraphSON $ SimpleProperty "since" $ nonTypedGraphSON (2009 :: Int)
    loadGraphSON "property.v1.json" `shouldReturn` Right ex
  let ex23 = typedGraphSON $ SimpleProperty "since" $ typedGraphSON' "g:Int32" (2009 :: Int)
  it "should parse GraphSON v2" $ do
    loadGraphSON "property.v2.json" `shouldReturn` Right ex23
  it "should parse GraphSON v3" $ do
    loadGraphSON "property.v3.json" `shouldReturn` Right ex23

spec_AVertexProperty :: Spec
spec_AVertexProperty = describe "AVertexProperty" $ do
  it "should parse GraphSON v1" $ do
    let ex = nonTypedGraphSON $
             AVertexProperty
             { avpId = nonTypedGraphSON $ toJSON (0 :: Int),
               avpLabel = "name",
               avpValue = nonTypedGraphSON $ toJSON ("marko" :: Text),
               avpProperties = mempty
             }
    loadGraphSON "vertex_property.v1.json" `shouldReturn` Right ex
  let ex23 = typedGraphSON $
             AVertexProperty
             { avpId = typedGraphSON' "g:Int64" $ toJSON (0 :: Int),
               avpLabel = "name",
               avpValue = nonTypedGraphSON $ toJSON ("marko" :: Text),
               avpProperties = mempty
             }
  it "should parse GraphSON v2" $ do
    loadGraphSON "vertex_property.v2.json" `shouldReturn` Right ex23
  it "should parse GraphSON v3" $ do
    loadGraphSON "vertex_property.v3.json" `shouldReturn` Right ex23

spec_AVertex :: Spec
spec_AVertex = describe "AVertex" $ do
  it "should parse GraphSON v1" $ do
    let ex = nonTypedGraphSON $
             AVertex
             { avId = nonTypedGraphSON $ toJSON (1 :: Int),
               avLabel = "person",
               avProperties = foldr putProperty mempty
                              [ AVertexProperty
                                { avpId = nonTypedGraphSON $ toJSON (0 :: Int),
                                  avpLabel = "name",
                                  avpValue = nonTypedGraphSON $ toJSON ("marko" :: Text),
                                  avpProperties = mempty
                                },
                                AVertexProperty
                                { avpId = nonTypedGraphSON $ toJSON (6 :: Int),
                                  avpLabel = "location",
                                  avpValue = nonTypedGraphSON $ toJSON ("san diego" :: Text),
                                  avpProperties = foldr putProperty mempty
                                                  [ SimpleProperty "startTime" $ nonTypedGraphSON $ toJSON (1997 :: Int),
                                                    SimpleProperty "endTime" $ nonTypedGraphSON $ toJSON (2001 :: Int)
                                                  ]
                                },
                                AVertexProperty
                                { avpId = nonTypedGraphSON $ toJSON (7 :: Int),
                                  avpLabel = "location",
                                  avpValue = nonTypedGraphSON $ toJSON ("santa cruz" :: Text),
                                  avpProperties = foldr putProperty mempty
                                                  [ SimpleProperty "startTime" $ nonTypedGraphSON $ toJSON (2001 :: Int),
                                                    SimpleProperty "endTime" $ nonTypedGraphSON $ toJSON (2004 :: Int)
                                                  ]
                                },
                                AVertexProperty
                                { avpId = nonTypedGraphSON $ toJSON (8 :: Int),
                                  avpLabel = "location",
                                  avpValue = nonTypedGraphSON $ toJSON ("brussels" :: Text),
                                  avpProperties = foldr putProperty mempty
                                                  [ SimpleProperty "startTime" $ nonTypedGraphSON $ toJSON (2004 :: Int),
                                                    SimpleProperty "endTime" $ nonTypedGraphSON $ toJSON (2005 :: Int)
                                                  ]
                                },
                                AVertexProperty
                                { avpId = nonTypedGraphSON $ toJSON (9 :: Int),
                                  avpLabel = "location",
                                  avpValue = nonTypedGraphSON $ toJSON ("santa fe" :: Text),
                                  avpProperties = foldr putProperty mempty
                                                  [ SimpleProperty "startTime" $ nonTypedGraphSON $ toJSON (2005 :: Int)
                                                  ]
                                }
                              ]
             }
    loadGraphSON "vertex.v1.json" `shouldReturn` Right ex
  let ex23 = typedGraphSON $
             AVertex
             { avId = typedGraphSON' "g:Int32" $ toJSON (1 :: Int),
               avLabel = "person",
               avProperties = foldr putProperty mempty
                              [ AVertexProperty
                                { avpId = typedGraphSON' "g:Int64" $ toJSON (0 :: Int),
                                  avpLabel = "name",
                                  avpValue = nonTypedGraphSON $ toJSON ("marko" :: Text),
                                  avpProperties = mempty
                                },
                                AVertexProperty
                                { avpId = typedGraphSON' "g:Int64" $ toJSON (6 :: Int),
                                  avpLabel = "location",
                                  avpValue = nonTypedGraphSON $ toJSON ("san diego" :: Text),
                                  avpProperties = foldr putProperty mempty
                                                  [ SimpleProperty "startTime" $ typedGraphSON' "g:Int32" $ toJSON (1997 :: Int),
                                                    SimpleProperty "endTime" $ typedGraphSON' "g:Int32" $ toJSON (2001 :: Int)
                                                  ]
                                },
                                AVertexProperty
                                { avpId = typedGraphSON' "g:Int64" $ toJSON (7 :: Int),
                                  avpLabel = "location",
                                  avpValue = nonTypedGraphSON $ toJSON ("santa cruz" :: Text),
                                  avpProperties = foldr putProperty mempty
                                                  [ SimpleProperty "startTime" $ typedGraphSON' "g:Int32" $ toJSON (2001 :: Int),
                                                    SimpleProperty "endTime" $ typedGraphSON' "g:Int32" $ toJSON (2004 :: Int)
                                                  ]
                                },
                                AVertexProperty
                                { avpId = typedGraphSON' "g:Int64" $ toJSON (8 :: Int),
                                  avpLabel = "location",
                                  avpValue = nonTypedGraphSON $ toJSON ("brussels" :: Text),
                                  avpProperties = foldr putProperty mempty
                                                  [ SimpleProperty "startTime" $ typedGraphSON' "g:Int32" $ toJSON (2004 :: Int),
                                                    SimpleProperty "endTime" $ typedGraphSON' "g:Int32" $ toJSON (2005 :: Int)
                                                  ]
                                },
                                AVertexProperty
                                { avpId = typedGraphSON' "g:Int64" $ toJSON (9 :: Int),
                                  avpLabel = "location",
                                  avpValue = nonTypedGraphSON $ toJSON ("santa fe" :: Text),
                                  avpProperties = foldr putProperty mempty
                                                  [ SimpleProperty "startTime" $ typedGraphSON' "g:Int32" $ toJSON (2005 :: Int)
                                                  ]
                                }
                              ]
             }
  it "should parse GraphSON v2" $ do
    loadGraphSON "vertex.v2.json" `shouldReturn` Right ex23
  it "should parse GraphSON v3" $ do
    loadGraphSON "vertex.v3.json" `shouldReturn` Right ex23
