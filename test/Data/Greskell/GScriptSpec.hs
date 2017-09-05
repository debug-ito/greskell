{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Greskell.GScriptSpec (main,spec) where

import Data.String (fromString)
import Data.Text (Text, pack)
import Test.Hspec
import Test.QuickCheck (property, Arbitrary(..))

import Data.Greskell.GScript
  ( gRaw, getGScript, gLiteral,
    gPlaceHolder, toPlaceHolderVariable,
    gFunCall, gMethodCall
  )

-- TODO: move this into a single support module.
instance Arbitrary Text where
  arbitrary = fmap pack arbitrary

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "gRaw" $ it "should be just a raw script text" $ property $ \t ->
    (getGScript $ gRaw t) `shouldBe` t
  describe "gPlaceHolder" $ it "should create a placeholder variable" $ property $ \i ->
    (getGScript $ gPlaceHolder i) `shouldBe` toPlaceHolderVariable i
  describe "gLateral and fromString" $ do
    specify "empty" $ checkLiteral "" "\"\""
    specify "words" $ checkLiteral "hoge foo bar"  "\"hoge foo bar\""
    specify "escaped" $ checkLiteral "foo 'aaa \n \t \\ \"bar\"" "\"foo 'aaa \\n \\t \\\\ \\\"bar\\\"\""
  describe "gFunCall" $ do
    it "should make function call" $ do
      (getGScript $ gFunCall "fun" ["foo", gRaw "bar"]) `shouldBe` "fun(\"foo\", bar)"
  describe "gMethodCall" $ do
    it "should make method call" $ do
      (getGScript $ gMethodCall "meth" [gRaw "hoge", gRaw "foo", "bar"]) `shouldBe` ".meth(hoge, foo, \"bar\")"


checkLiteral :: String -> Text -> Expectation
checkLiteral input expected = do
  (getGScript $ fromString input) `shouldBe` expected
  (getGScript $ gLiteral $ pack input) `shouldBe` expected
