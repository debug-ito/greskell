{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Data.Greskell.GraphSONSpec (main,spec) where

import Data.Aeson (object, (.=), ToJSON(..), FromJSON(..))
import Data.Aeson.Types (parseEither, Value(..), Parser)
import Data.Either (isLeft)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (isInfixOf)
import Data.Int (Int32)
import Data.Text (Text)
import Test.Hspec

import Data.Greskell.GraphSON
  ( GraphSON, parseTypedGraphSON, parseTypedGraphSON',
    typedGraphSON', nonTypedGraphSON
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  fromJSON_spec
  parseTypedGraphSON_spec
  parseTypedGraphSON'_spec

fromJSON_spec :: Spec
fromJSON_spec = describe "FromJSON (recursive)" $ do
  let parse :: Value -> Either String (GraphSON Value)
      parse = parseEither parseJSON
      isLeftG :: Either String (GraphSON Value) -> Bool
      isLeftG = isLeft
  specify "bare map (one entry)" $ do
    let input = object ["foo" .= String "bar"]
    parse input `shouldBe` Right (nonTypedGraphSON input)
  specify "bare map (two entries)" $ do
    let input = object ["foo" .= String "bar", "quux" .= String "hoge"]
    parse input `shouldBe` Right (nonTypedGraphSON input)
  specify "wrapped string" $ do
    let input = object ["@type" .= String "g:String", "@value" .= String "hogehoge"]
    parse input `shouldBe` Right (typedGraphSON' "g:String" $ String "hogehoge")
  specify "wrapped object" $ do
    let body = object ["foo" .= String "bar"]
        input = object ["@type" .= String "g:Map", "@value" .= body]
    parse input `shouldBe` Right (typedGraphSON' "g:Map" body)
  specify "wrong @type type" $ do
    let body = object ["foo" .= String "bar"]
        input = object ["@type" .= Number 10, "@value" .= body]
    parse input `shouldSatisfy` isLeftG
  specify "null @type" $ do
    let body = object ["foo" .= String "bar"]
        input = object ["@type" .= Null, "@value" .= body]
    parse input `shouldSatisfy` isLeftG
  specify "no @value field" $ do
    let input = object ["@type" .= String "g:String", "foo" .= Null]
    parse input `shouldBe` Right (nonTypedGraphSON input)
  specify "confusing bare map (three entries)" $ do
    let input = object [ "@type" .= String "g:String",
                         "@value" .= String "hoge",
                         "@other" .= String "foo"
                       ]
    parse input `shouldBe` Right (nonTypedGraphSON input)

parseTypedGraphSON_spec :: Spec
parseTypedGraphSON_spec = describe "parseTypedGraphSON" $ do
  let parse = parseEither parseTypedGraphSON
  describe "Int32" $ do
    specify "invalid bare value" $ do
      let (Left got) = parse (String "foo") :: Either String (GraphSON Int32)
      got `shouldSatisfy` (isInfixOf "Not a valid typed JSON")
    specify "valid bare value" $ do
      let (Left got) = parse (Number 255) :: Either String (GraphSON Int32)
      got `shouldSatisfy` (isInfixOf "Not a valid typed JSON")
    specify "typed JSON with invalid @type field" $ do
      let (Left got) = parse (object ["@type" .= String "g:Int64", "@value" .= Number 255]) :: Either String (GraphSON Int32)
      got `shouldSatisfy` (isInfixOf "Expected @type")
    specify "typed JSON with valid @type field and invalid @value" $ do
      let (Left got) = parse (object ["@type" .= String "g:Int32", "@value" .= String "hoge"]) :: Either String (GraphSON Int32)
      got `shouldSatisfy` (isInfixOf "expected Int32")
    specify "typed JSON with valid @type and @value" $ do
      let got = parse (object ["@type" .= String "g:Int32", "@value" .= Number 255]) :: Either String (GraphSON Int32)
      got `shouldBe` (Right $ typedGraphSON' "g:Int32" 255)
  describe "HashMap" $ do
    specify "invalid bare value" $ do
      let (Left got) = parse (String "quux") :: Either String (GraphSON (HashMap String String))
      got `shouldSatisfy` (isInfixOf "Not a valid typed JSON")
    specify "valid bare value" $ do
      let (Left got) = parse (object ["hoge" .= String "HOGE", "foo" .= String "FOO"]) :: Either String (GraphSON (HashMap String String))
      got `shouldSatisfy` (isInfixOf "Not a valid typed JSON")
    specify "typed JSON with invalid @type field" $ do
      let (Left got) = parse (object [ "@type" .= String "g:Array",
                                       "@value" .= object ["hoge" .= String "HOGE", "foo" .= String "FOO"]
                                     ]) :: Either String (GraphSON (HashMap String String))
      got `shouldSatisfy` (isInfixOf "Expected @type")
    specify "typed JSON with valid @type field and invalid @value field" $ do
      let (Left got) = parse (object [ "@type" .= String "g:Array",
                                       "@value" .= Number 100
                                     ]) :: Either String (GraphSON (HashMap String String))
      got `shouldSatisfy` (isInfixOf "expected HashMap")
    specify "typed JSON with valid @type and @value" $ do
      let got = parse (object [ "@type" .= String "g:Map",
                                "@value" .= object ["hoge" .= String "HOGE", "foo" .= String "FOO"]
                              ]) :: Either String (GraphSON (HashMap String String))
      got `shouldBe` (Right $ typedGraphSON' "g:Map" $ HM.fromList [("hoge", "HOGE"), ("foo", "FOO")])


parseTypedGraphSON'_spec :: Spec
parseTypedGraphSON'_spec = describe "parseTypedGraphSON'" $ do
  let parse = parseEither parseTypedGraphSON'
  describe "Int32" $ do
    specify "invalid bare value" $ do
      let (Right (Left got)) = parse (String "foo") :: Either String (Either String (GraphSON Int32))
      got `shouldSatisfy` (isInfixOf "Not a valid typed JSON")
    specify "valid bare value" $ do
      let (Right (Left got)) = parse (Number 255) :: Either String (Either String (GraphSON Int32))
      got `shouldSatisfy` (isInfixOf "Not a valid typed JSON")
    specify "typed JSON with invalid @type field" $ do
      let (Left got) = parse (object ["@type" .= String "g:Int64", "@value" .= Number 255]) :: Either String (Either String (GraphSON Int32))
      got `shouldSatisfy` (isInfixOf "Expected @type")
    specify "typed JSON with valid @type field and invalid @value" $ do
      let (Left got) = parse (object ["@type" .= String "g:Int32", "@value" .= String "hoge"]) :: Either String (Either String (GraphSON Int32))
      got `shouldSatisfy` (isInfixOf "expected Int32")
    specify "typed JSON with valid @type and @value" $ do
      let got = parse (object ["@type" .= String "g:Int32", "@value" .= Number 255]) :: Either String (Either String (GraphSON Int32))
      got `shouldBe` (Right $ Right $ typedGraphSON' "g:Int32" 255)
  describe "HashMap" $ do
    specify "invalid bare value" $ do
      let (Right (Left got)) = parse (String "quux") :: Either String (Either String (GraphSON (HashMap String String)))
      got `shouldSatisfy` (isInfixOf "Not a valid typed JSON")
    specify "valid bare value" $ do
      let (Right (Left got)) = parse (object ["hoge" .= String "HOGE", "foo" .= String "FOO"])
                               :: Either String (Either String (GraphSON (HashMap String String)))
      got `shouldSatisfy` (isInfixOf "Not a valid typed JSON")
    specify "typed JSON with invalid @type field" $ do
      let (Left got) = parse (object [ "@type" .= String "g:Array",
                                       "@value" .= object ["hoge" .= String "HOGE", "foo" .= String "FOO"]
                                     ]) :: Either String (Either String (GraphSON (HashMap String String)))
      got `shouldSatisfy` (isInfixOf "Expected @type")
    specify "typed JSON with valid @type field and invalid @value field" $ do
      let (Left got) = parse (object [ "@type" .= String "g:Array",
                                       "@value" .= Number 100
                                     ]) :: Either String (Either String (GraphSON (HashMap String String)))
      got `shouldSatisfy` (isInfixOf "expected HashMap")
    specify "typed JSON with valid @type and @value" $ do
      let got = parse (object [ "@type" .= String "g:Map",
                                "@value" .= object ["hoge" .= String "HOGE", "foo" .= String "FOO"]
                              ]) :: Either String (Either String (GraphSON (HashMap String String)))
      got `shouldBe` (Right $ Right $ typedGraphSON' "g:Map" $ HM.fromList [("hoge", "HOGE"), ("foo", "FOO")])



