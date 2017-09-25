{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Greskell.GreskellSpec (main,spec) where

import Data.String (fromString)
import Data.Text (Text, pack)
import Test.Hspec
import Test.QuickCheck (property, Arbitrary(..))

import Data.Greskell.Greskell
  ( raw, rawS, runGreskell, string,
    placeHolder, toPlaceHolderVariable,
    funCall, methodCall
  )

-- TODO: move this into a single support module.
instance Arbitrary Text where
  arbitrary = fmap pack arbitrary

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "raw" $ it "should be just a raw script text" $ property $ \t ->
    (runGreskell $ raw t) `shouldBe` t
  describe "rawS" $ do
    specify "integer" $ do
      let x :: Int
          x = 123
      (runGreskell $ rawS x) `shouldBe` "123"
    specify "negative integer" $ do
      let x :: Int
          x = -56
      (runGreskell $ rawS x) `shouldBe` "-56"
    specify "floating" $ do
      let x :: Double
          x = 92.12
      (runGreskell $ rawS x) `shouldBe` "92.12"
  describe "placeHolder" $ it "should create a placeholder variable" $ property $ \i ->
    (runGreskell $ placeHolder i) `shouldBe` toPlaceHolderVariable i
  describe "gLateral and fromString" $ do
    specify "empty" $ checkLiteral "" "\"\""
    specify "words" $ checkLiteral "hoge foo bar"  "\"hoge foo bar\""
    specify "escaped" $ checkLiteral "foo 'aaa \n \t \\ \"bar\"" "\"foo 'aaa \\n \\t \\\\ \\\"bar\\\"\""
  describe "funCall" $ do
    it "should make function call" $ do
      (runGreskell $ funCall "fun" ["foo", raw "bar"]) `shouldBe` "fun(\"foo\",bar)"
  describe "methodCall" $ do
    it "should make method call" $ do
      (runGreskell $ methodCall "meth" [raw "hoge", raw "foo", "bar"]) `shouldBe` ".meth(hoge,foo,\"bar\")"


checkLiteral :: String -> Text -> Expectation
checkLiteral input expected = do
  (runGreskell $ fromString input) `shouldBe` expected
  (runGreskell $ string $ pack input) `shouldBe` expected
