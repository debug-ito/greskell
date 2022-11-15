{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.GreskellSpec
    ( main
    , spec
    ) where

import qualified Data.Aeson                    as Aeson
import           Data.String                   (fromString)
import           Data.Text                     (Text, pack)
import           Test.Hspec
import           Test.QuickCheck               (property)

import           Data.Greskell.Greskell        (Greskell, false, list, number, string, toGremlin,
                                                true, unsafeFunCall, unsafeGreskell, value)

import           Data.Greskell.Test.QuickCheck ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_literals
  spec_other

spec_other :: Spec
spec_other = do
  describe "unsafeGreskell" $ it "should be just a raw script text" $ property $ \t ->
    (toGremlin $ unsafeGreskell t) `shouldBe` t
  describe "Num" $ do
    specify "integer" $ do
      let x = 123 :: Greskell Int
      toGremlin x `shouldBe` "123"
    specify "negative integer" $ do
      let x = -56 :: Greskell Int
      toGremlin x `shouldBe` "-(56)"
    specify "operations" $ do
      let x = (30 + 15 * 20 - 10) :: Greskell Int
      toGremlin x `shouldBe` "((30)+((15)*(20)))-(10)"
    specify "abs, signum" $ do
      let x = (signum $ abs (-100)) :: Greskell Int
      toGremlin x  `shouldBe` "java.lang.Long.signum(java.lang.Math.abs(-(100)))"
  describe "Fractional" $ do
    specify "floating point literal" $ do
      let x = 92.12 :: Greskell Double
      (toGremlin x) `shouldBe` "2303.0/25"
    specify "operations" $ do
      let x = (100.5 * recip 30.0 / 20.2) :: Greskell Double
      toGremlin x `shouldBe` "((201.0/2)*(1.0/(30.0/1)))/(101.0/5)"
  describe "Monoid" $ do
    specify "mempty" $ do
      let got = mempty :: Greskell Text
      toGremlin got `shouldBe` "\"\""
    specify "mappend" $ do
      let got = (mappend "foo" "bar") :: Greskell Text
      toGremlin got `shouldBe` "(\"foo\")+(\"bar\")"
  describe "unsafeFunCall" $ do
    it "should make function call" $ do
      (toGremlin $ unsafeFunCall "fun" ["foo", "bar"]) `shouldBe` "fun(foo,bar)"

spec_literals :: Spec
spec_literals = do
  describe "string and fromString" $ do
    specify "empty" $ checkStringLiteral "" "\"\""
    specify "words" $ checkStringLiteral "hoge foo bar"  "\"hoge foo bar\""
    specify "escaped" $ checkStringLiteral "foo 'aaa \n \t \\ \"bar\"" "\"foo 'aaa \\n \\t \\\\ \\\"bar\\\"\""
  describe "list" $ do
    specify "empty" $ do
      toGremlin (list []) `shouldBe` "[]"
    specify "num" $ do
      toGremlin (list $ [(10 :: Greskell Int), 20, 30]) `shouldBe` "[10,20,30]"
    specify "list of lists" $ do
      toGremlin (list $ map list $ [[("" :: Greskell Text)], ["foo", "bar"], ["buzz"]])
        `shouldBe` "[[\"\"],[\"foo\",\"bar\"],[\"buzz\"]]"
  describe "boolean" $ do
    specify "true" $ do
      toGremlin true `shouldBe` "true"
    specify "false" $ do
      toGremlin false `shouldBe` "false"
  describe "number" $ do
    specify "zero" $ do
      toGremlin (number 0) `shouldBe` "0.0"
    specify "positive integer" $ do
      toGremlin (number 1234) `shouldBe` "1234.0"
    specify "negative integer" $ do
      toGremlin (number (-292)) `shouldBe` "-292.0"
    specify "positive floating" $ do
      toGremlin (number 32.123) `shouldBe` "32.123"
    specify "negative floating" $ do
      toGremlin (number (-0.0943)) `shouldBe` "-9.43e-2"
    specify "big positive integer" $ do
      toGremlin (number 3.23e9) `shouldBe` "3.23e9"
  describe "value" $ do
    specify "null" $ do
      toGremlin (value Aeson.Null) `shouldBe` "null"
    specify "bool" $ do
      toGremlin (value $ Aeson.Bool False) `shouldBe` "false"
    specify "integer" $ do
      toGremlin (value $ Aeson.Number 100) `shouldBe` "100.0"
    specify "floating-point number" $ do
      toGremlin (value $ Aeson.Number 10.23) `shouldBe` "10.23"
    specify "String" $ do
      toGremlin (value $ Aeson.String "foobar") `shouldBe` "\"foobar\""
    specify "empty Array" $ do
      toGremlin (value $ Aeson.toJSON ([] :: [Int])) `shouldBe` "[]"
    specify "non-empty Array" $ do
      toGremlin (value $ Aeson.toJSON [(5 :: Int), 6, 7]) `shouldBe` "[5.0,6.0,7.0]"
    specify "empty Object" $ do
      toGremlin (value $ Aeson.object []) `shouldBe` "[:]"


checkStringLiteral :: String -> Text -> Expectation
checkStringLiteral input expected = do
  let input' = fromString input :: Greskell Text
  (toGremlin $ input') `shouldBe` expected
  (toGremlin $ string $ pack input) `shouldBe` expected
