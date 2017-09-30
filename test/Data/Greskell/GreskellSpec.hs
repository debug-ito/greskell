{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Greskell.GreskellSpec (main,spec) where

import Data.String (fromString)
import Data.Text (Text, pack)
import Test.Hspec
import Test.QuickCheck (property, Arbitrary(..))

import Data.Greskell.Greskell
  ( unsafeGreskell, runGreskell, string,
    unsafePlaceHolder, toPlaceHolderVariable,
    unsafeFunCall, unsafeMethodCall,
    Greskell
  )

-- TODO: move this into a single support module.
instance Arbitrary Text where
  arbitrary = fmap pack arbitrary

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "unsafeGreskell" $ it "should be just a raw script text" $ property $ \t ->
    (runGreskell $ unsafeGreskell t) `shouldBe` t
  describe "Num" $ do
    specify "integer" $ do
      let x = 100 :: Greskell Int
      runGreskell x `shouldBe` "123"
    specify "negative integer" $ do
      let x = -56 :: Greskell Int
      runGreskell x `shouldBe` "-(56)"
    specify "operations" $ do
      let x = (30 + 15 * 20 - 10) :: Greskell Int
      runGreskell x `shouldBe` "((30)+((15)*(20)))-(10)"
    specify "abs, signum" $ do
      let x = (signum $ abs (-100)) :: Greskell Int
      runGreskell x  `shouldBe` "java.lang.Long.signum(java.lang.Math.abs(-(100)))"
  describe "Fractional" $ do
    specify "floating point literal" $ do
      let x = 92.12 :: Greskell Double
      (runGreskell x) `shouldBe` "(2303.0/25)"
    specify "operations" $ do
      let x = (100.5 * recip 30 / 20.2) :: Greskell Double
      runGreskell x `shouldBe` "((201.0/2)*(0.1/(30.0/1)))/((101.0/5))"
  describe "Monoid" $ do
    specify "mempty" $ do
      let got = mempty :: Greskell Text
      runGreskell got `shouldBe` "\"\""
    specify "mappend" $ do
      let got = (mappend "foo" "bar") :: Greskell Text
      runGreskell got `shouldBe` "(\"foo\")+(\"bar\")"
  describe "placeHolder" $ it "should create a placeholder variable" $ property $ \i ->
    (runGreskell $ unsafePlaceHolder i) `shouldBe` toPlaceHolderVariable i
  describe "string and fromString" $ do
    specify "empty" $ checkStringLiteral "" "\"\""
    specify "words" $ checkStringLiteral "hoge foo bar"  "\"hoge foo bar\""
    specify "escaped" $ checkStringLiteral "foo 'aaa \n \t \\ \"bar\"" "\"foo 'aaa \\n \\t \\\\ \\\"bar\\\"\""
  describe "unsafeFunCall" $ do
    it "should make function call" $ do
      (runGreskell $ unsafeFunCall "fun" ["foo", "bar"]) `shouldBe` "fun(foo,bar)"
  describe "unsafeMethodCall" $ do
    it "should make method call" $ do
      (runGreskell $ unsafeMethodCall "meth" ["hoge", "foo", "bar"]) `shouldBe` ".meth(hoge,foo,bar)"


checkStringLiteral :: String -> Text -> Expectation
checkStringLiteral input expected = do
  let input' = fromString input :: Greskell Text
  (runGreskell $ input') `shouldBe` expected
  (runGreskell $ string $ pack input) `shouldBe` expected
