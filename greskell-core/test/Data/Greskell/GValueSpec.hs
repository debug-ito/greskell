{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.GValueSpec (main,spec) where

import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Either (isLeft)
import qualified Data.HashMap.Strict as HM
import Data.List (isInfixOf)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Vector as V
import Test.Hspec

import Data.Greskell.GraphSON (nonTypedGraphSON, typedGraphSON')
import Data.Greskell.GValue (GValue(..), GValueBody(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "FromJSON and ToJSON" $ do
    fromToJSON "bare null" "null" (bare GNull)
    fromToJSON "wrapped null" (gson "g:Int" "null") (wrapped "g:Int" GNull)
    fromToJSON "bare number" "100" (bare $ GNumber 100)
    fromToJSON "wrapped number" (gson "g:Int" "100") (wrapped "g:Int" $ GNumber 100)
    fromToJSON "bare bool" "true" (bare $ GBool True)
    fromToJSON "wrapped bool" (gson "g:Boolean" "true") (wrapped "g:Boolean" $ GBool True)
    fromToJSON "bare str" "\"hoge\"" (bare $ GString "hoge")
    fromToJSON "wrapped str" (gson "g:String" "\"hoge\"") (wrapped "g:String" $ GString "hoge")
    fromToJSON "bare array" "[null, \"foo\", 100]"
      (bare $ GArray $ fmap bare $ V.fromList [GNull, GString "foo", GNumber 100])
    fromToJSON "wrapped array" (gson "g:List" "[null, \"foo\", 100]")
      (wrapped "g:List" $ GArray $ fmap bare $ V.fromList [GNull, GString "foo", GNumber 100])
    fromToJSON "bare object" "{\"foo\": \"bar\", \"hoge\": 99, \"quux\": null}"
      (bare $ GObject $ fmap bare $ HM.fromList [("foo", GString "bar"), ("hoge", GNumber 99), ("quux", GNull)])
    fromToJSON "wrapped object" (gson "g:Map" "{\"foo\": \"bar\", \"hoge\": 99, \"quux\": null}")
      (wrapped "g:Map" $ GObject $ fmap bare $ HM.fromList [("foo", GString "bar"), ("hoge", GNumber 99), ("quux", GNull)])
    nested_spec
    double_wrap_spec
    decode_error_spec

nested_spec :: Spec
nested_spec = fromToJSON "mixed nested" input expected
  where
    input = gson "g:List" ("[100, " <> elem_a <> ", " <> elem_b <> "]")
    elem_a = gson "g:Map" ("{\"foo\": 100, \"bar\":" <> (gson "g:Int" "200") <> "}")
    elem_b = gson "g:List" ("[null, " <> (gson "g:Object" "null") <> ", " <> elem_c <> ", " <> (gson "g:Boolean" "false") <> "]")
    elem_c = "{\"xxx\": " <> (gson "g:Int" "200") <> ", \"yyy\": true}"
    expected = wrapped "g:List" $ GArray $ V.fromList [ bare $ GNumber 100, exp_a, exp_b ]
    exp_a = wrapped "g:Map" $ GObject $ HM.fromList
            [ ("foo", bare $ GNumber 100),
              ("bar", wrapped "g:Int" $ GNumber 200)
            ]
    exp_b = wrapped "g:List" $ GArray $ V.fromList
            [ bare GNull,
              wrapped "g:Object" GNull,
              exp_c,
              wrapped "g:Boolean" $ GBool False
            ]
    exp_c = bare $ GObject $ HM.fromList
            [ ("xxx", wrapped "g:Int" $ GNumber 200),
              ("yyy", bare $ GBool True)
            ]

-- Basically this case should not appear in GraphSON encoding, but
-- GValue can parse it. However, the parsed GValue has a GObject with
-- "@type" and "@value" keys explicitly.
double_wrap_spec :: Spec
double_wrap_spec = fromToJSON "double wrapped" input expected
  where
    input = gson "g:Object" $ gson "g:Int" "100"
    expected = wrapped "g:Object" $ GObject $ HM.fromList
               [ ("@type", bare $ GString "g:Int"),
                 ("@value", bare $ GNumber 100)
               ]

decode_error_spec :: Spec
decode_error_spec = do
  specify "wrong @type type" $ do
    let got :: Either String GValue
        got = Aeson.eitherDecode "{\"@type\": 200, \"@value\": 100}"
    got `shouldSatisfy` isLeft
    fromLeft' got `shouldSatisfy` (isInfixOf "@type")
  specify "null @type" $ do
    let got :: Either String GValue
        got = Aeson.eitherDecode "{\"@type\": null, \"@value\": 100}"
    got `shouldSatisfy` isLeft
    fromLeft' got `shouldSatisfy` (isInfixOf "@type")

forceDecode :: FromJSON a => BSL.ByteString -> a
forceDecode json = case Aeson.eitherDecode json of
  Left err -> error ("Unexpected decode failure: " <> err)
  Right a -> a

fromToJSON :: String -> BSL.ByteString -> GValue -> Spec
fromToJSON label input_json expected = specify label $ do
  decoded `shouldBe` expected
  encoded `shouldBe` exp_enc
  where
    decoded = forceDecode input_json
    encoded = Aeson.toJSON decoded
    exp_enc = forceDecode input_json
    
bare :: GValueBody -> GValue
bare = GValue . nonTypedGraphSON

wrapped :: Text -> GValueBody -> GValue
wrapped t b = GValue $ typedGraphSON' t b

gson :: BSL.ByteString -> BSL.ByteString -> BSL.ByteString
gson ftype fvalue = "{\"@type\":\"" <> ftype <> "\",\"@value\":" <> fvalue <> "}"

fromLeft' :: (Show a, Show b) => Either a b -> a
fromLeft' (Left a) = a
fromLeft' e = error ("Expecting Left, but got " ++ show e)
