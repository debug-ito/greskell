{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.GMapSpec (main,spec) where

import Data.Aeson (eitherDecode, object, (.=), toJSON, Value(..))
import qualified Data.HashMap.Strict as HM
import Data.Vector ((!), Vector)
import qualified Data.Vector as Vec
import Test.Hspec

import Data.Greskell.GraphSON (GraphSON(..), typedGraphSON, nonTypedGraphSON)
import Data.Greskell.GMap (GMap(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "GraphSON GMap" $ do
  describe "non-flat" $ do
    let val :: GraphSON (GMap String Int)
        val = nonTypedGraphSON $ GMap False $ HM.fromList [("foo", 3), ("bar", 5), ("a", 1)]
    specify "FromJSON" $ do
      let input = "{\"foo\":3, \"a\": 1, \"bar\": 5}"
      eitherDecode input `shouldBe` Right val
    specify "ToJSON" $ do
      let expected = object [ "foo" .= Number 3,
                              "bar" .= Number 5,
                              "a"   .= Number 1
                            ]
      toJSON val `shouldBe` expected
  describe "flat" $ do
    let val :: GraphSON (GMap String Int)
        val = typedGraphSON $ GMap True $ HM.fromList [("foo", 3), ("bar", 5), ("a", 1)]
    specify "FromJSON" $ do
      let input = "{\"@type\": \"g:Map\", \"@value\": [\"a\", 1, \"bar\", 5, \"foo\", 3]}"
      eitherDecode input `shouldBe` Right val
    specify "ToJSON" $ do
      let exp_flat = Vec.fromList [ String "foo", Number 3,
                                    String "bar", Number 5,
                                    String "a",   Number 1
                                  ]
          (Object got) = toJSON val
      HM.lookup "@type" got `shouldBe` (Just $ String "g:Map")
      let (Just (Array got_flat)) = HM.lookup "@value" got
      pairList got_flat `shouldMatchList` pairList exp_flat

pairList :: Vector Value -> [(Value,Value)]
pairList a = map toPair $ [0 .. imax]
  where
    imax = (Vec.length a `div` 2) - 1
    toPair i = (a ! (i*2), a ! (i*2 + 1))
