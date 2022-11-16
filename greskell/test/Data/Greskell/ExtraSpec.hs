{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.ExtraSpec
    ( main
    , spec
    ) where

import qualified Data.Aeson.KeyMap        as KM
import           Data.Monoid              (mempty, (<>))
import           Data.Text                (Text)
import           Test.Hspec

import           Data.Aeson               (Value (..))
import           Data.Greskell.Binder     (Binder, Binding, runBinder)
import           Data.Greskell.Extra      (writeKeyValues, writePropertyKeyValues)
import           Data.Greskell.Graph      (AVertex, Key, KeyValue, (=:))
import           Data.Greskell.Greskell   (toGremlin)
import           Data.Greskell.GTraversal (Walk, WalkType)

main :: IO ()
main = hspec spec

runBoundWalk :: WalkType c => Binder (Walk c AVertex AVertex) -> (Text, Binding)
runBoundWalk = doFst . runBinder
  where
    doFst (w, b) = (toGremlin w, b)

spec :: Spec
spec = do
  describe "writePropertyKeyValues" $ do
    specify "empty" $ do
      let input :: [(Text, ())]
          input = []
      (runBoundWalk $ writePropertyKeyValues input) `shouldBe` ("__.identity()", mempty)
    specify "one prop" $ do
      let input :: [(Text, Int)]
          input = [("age", 24)]
      (runBoundWalk $ writePropertyKeyValues input)
        `shouldBe` ( "__.property(\"age\",__v0).identity()",
                     KM.fromList [("__v0", Number 24)]
                   )
    specify "multiple props" $ do
      let input :: [(Text, Value)]
          input = [("age", Number 24), ("name", String "Toshio"), ("foo", String "bar")]
      (runBoundWalk $ writePropertyKeyValues input)
        `shouldBe` ( "__.property(\"age\",__v0).property(\"name\",__v1)"
                     <> ".property(\"foo\",__v2).identity()",
                     KM.fromList [ ("__v0", Number 24),
                                   ("__v1", String "Toshio"),
                                   ("__v2", String "bar")
                                 ]
                   )
  describe "writeKeyValues" $ do
    specify "empty" $ do
      (toGremlin $ writeKeyValues ([] :: [KeyValue AVertex])) `shouldBe` "__.identity()"
    specify "key-values" $ do
      let name :: Key AVertex Text
          name = "name"
          age :: Key AVertex Int
          age = "age"
          input = writeKeyValues [name =: "toshio", age =: 30]
      (toGremlin input) `shouldBe` "__.property(\"name\",\"toshio\").property(\"age\",30).identity()"
