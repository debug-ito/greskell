{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
module Data.Greskell.GraphSONSpec
    ( main
    , spec
    ) where

import           Data.Aeson                    (FromJSON (..), ToJSON (..), Value (..), object,
                                                (.=))
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.KeyMap             as KM
import           Data.Aeson.Types              (Parser, Value (..), parseEither)
import qualified Data.ByteString.Lazy          as BSL
import           Data.Either                   (isLeft)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HM
import           Data.Int                      (Int32)
import           Data.List                     (isInfixOf)
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text)
import qualified Data.Vector                   as V
import           Test.Hspec

import           Data.Greskell.GMap            (GMapEntry (..), unGMapEntry)
import           Data.Greskell.GraphSON        (FromGraphSON (..), GValue, GValueBody (..),
                                                GraphSON, nonTypedGValue, nonTypedGraphSON,
                                                parseTypedGraphSON, typedGValue', typedGraphSON')
import           Data.Greskell.GraphSON.GValue (unwrapAll, unwrapOne)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "GrpahSON" $ do
    fromJSON_spec
    parseTypedGraphSON_spec
    -- parseTypedGraphSON'_spec
  describe "GValue" $ do
    gvalue_spec
    fromGraphSON_spec

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

isParseTypeError :: String -- ^ expected type
                 -> String -- ^ error message
                 -> Bool -- ^ matched
#if MIN_VERSION_aeson(1,4,3)
isParseTypeError exp_type = isInfixOf ("parsing " <> exp_type <> " failed")
#else
isParseTypeError exp_type = isInfixOf ("expected " <> exp_type)
#endif


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
      got `shouldSatisfy` isParseTypeError "Int32"
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
      got `shouldSatisfy` isParseTypeError "HashMap"
    specify "typed JSON with valid @type and @value" $ do
      let got = parse (object [ "@type" .= String "g:Map",
                                "@value" .= object ["hoge" .= String "HOGE", "foo" .= String "FOO"]
                              ]) :: Either String (GraphSON (HashMap String String))
      got `shouldBe` (Right $ typedGraphSON' "g:Map" $ HM.fromList [("hoge", "HOGE"), ("foo", "FOO")])


-- parseTypedGraphSON'_spec :: Spec
-- parseTypedGraphSON'_spec = describe "parseTypedGraphSON'" $ do
--   let parse = parseEither parseTypedGraphSON'
--   describe "Int32" $ do
--     specify "invalid bare value" $ do
--       let (Right (Left got)) = parse (String "foo") :: Either String (Either String (GraphSON Int32))
--       got `shouldSatisfy` (isInfixOf "Not a valid typed JSON")
--     specify "valid bare value" $ do
--       let (Right (Left got)) = parse (Number 255) :: Either String (Either String (GraphSON Int32))
--       got `shouldSatisfy` (isInfixOf "Not a valid typed JSON")
--     specify "typed JSON with invalid @type field" $ do
--       let (Left got) = parse (object ["@type" .= String "g:Int64", "@value" .= Number 255]) :: Either String (Either String (GraphSON Int32))
--       got `shouldSatisfy` (isInfixOf "Expected @type")
--     specify "typed JSON with valid @type field and invalid @value" $ do
--       let (Left got) = parse (object ["@type" .= String "g:Int32", "@value" .= String "hoge"]) :: Either String (Either String (GraphSON Int32))
--       got `shouldSatisfy` (isInfixOf "expected Int32")
--     specify "typed JSON with valid @type and @value" $ do
--       let got = parse (object ["@type" .= String "g:Int32", "@value" .= Number 255]) :: Either String (Either String (GraphSON Int32))
--       got `shouldBe` (Right $ Right $ typedGraphSON' "g:Int32" 255)
--   describe "HashMap" $ do
--     specify "invalid bare value" $ do
--       let (Right (Left got)) = parse (String "quux") :: Either String (Either String (GraphSON (HashMap String String)))
--       got `shouldSatisfy` (isInfixOf "Not a valid typed JSON")
--     specify "valid bare value" $ do
--       let (Right (Left got)) = parse (object ["hoge" .= String "HOGE", "foo" .= String "FOO"])
--                                :: Either String (Either String (GraphSON (HashMap String String)))
--       got `shouldSatisfy` (isInfixOf "Not a valid typed JSON")
--     specify "typed JSON with invalid @type field" $ do
--       let (Left got) = parse (object [ "@type" .= String "g:Array",
--                                        "@value" .= object ["hoge" .= String "HOGE", "foo" .= String "FOO"]
--                                      ]) :: Either String (Either String (GraphSON (HashMap String String)))
--       got `shouldSatisfy` (isInfixOf "Expected @type")
--     specify "typed JSON with valid @type field and invalid @value field" $ do
--       let (Left got) = parse (object [ "@type" .= String "g:Array",
--                                        "@value" .= Number 100
--                                      ]) :: Either String (Either String (GraphSON (HashMap String String)))
--       got `shouldSatisfy` (isInfixOf "expected HashMap")
--     specify "typed JSON with valid @type and @value" $ do
--       let got = parse (object [ "@type" .= String "g:Map",
--                                 "@value" .= object ["hoge" .= String "HOGE", "foo" .= String "FOO"]
--                               ]) :: Either String (Either String (GraphSON (HashMap String String)))
--       got `shouldBe` (Right $ Right $ typedGraphSON' "g:Map" $ HM.fromList [("hoge", "HOGE"), ("foo", "FOO")])


gvalue_spec :: Spec
gvalue_spec = do
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
      (bare $ GObject $ fmap bare $ KM.fromList [("foo", GString "bar"), ("hoge", GNumber 99), ("quux", GNull)])
    fromToJSON "wrapped object" (gson "g:Map" "{\"foo\": \"bar\", \"hoge\": 99, \"quux\": null}")
      (wrapped "g:Map" $ GObject $ fmap bare $ KM.fromList [("foo", GString "bar"), ("hoge", GNumber 99), ("quux", GNull)])
    nested_spec
    double_wrap_spec
    decode_error_spec
  unwrap_spec

nestedSample :: BSL.ByteString
nestedSample = gson "g:List" ("[100, " <> elem_a <> ", " <> elem_b <> "]")
  where
    elem_a = gson "g:Map" ("{\"foo\": 100, \"bar\":" <> (gson "g:Int" "200") <> "}")
    elem_b = gson "g:List" ("[null, " <> (gson "g:Object" "null") <> ", " <> elem_c <> ", " <> (gson "g:Boolean" "false") <> "]")
    elem_c = "{\"xxx\": " <> (gson "g:Int" "200") <> ", \"yyy\": true}"

nested_spec :: Spec
nested_spec = fromToJSON "mixed nested" nestedSample expected
  where
    expected = wrapped "g:List" $ GArray $ V.fromList [ bare $ GNumber 100, exp_a, exp_b ]
    exp_a = wrapped "g:Map" $ GObject $ KM.fromList
            [ ("foo", bare $ GNumber 100),
              ("bar", wrapped "g:Int" $ GNumber 200)
            ]
    exp_b = wrapped "g:List" $ GArray $ V.fromList
            [ bare GNull,
              wrapped "g:Object" GNull,
              exp_c,
              wrapped "g:Boolean" $ GBool False
            ]
    exp_c = bare $ GObject $ KM.fromList
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
    expected = wrapped "g:Object" $ GObject $ KM.fromList
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
  Right a  -> a

fromToJSON :: String -> BSL.ByteString -> GValue -> Spec
fromToJSON label input_json expected = specify label $ do
  decoded `shouldBe` expected
  encoded `shouldBe` exp_enc
  where
    decoded :: GValue
    decoded = forceDecode input_json
    encoded = Aeson.toJSON decoded
    exp_enc = forceDecode input_json

bare :: GValueBody -> GValue
bare = nonTypedGValue

wrapped :: Text -> GValueBody -> GValue
wrapped = typedGValue'

gson :: BSL.ByteString -> BSL.ByteString -> BSL.ByteString
gson ftype fvalue = "{\"@type\":\"" <> ftype <> "\",\"@value\":" <> fvalue <> "}"

fromLeft' :: (Show a, Show b) => Either a b -> a
fromLeft' (Left a) = a
fromLeft' e        = error ("Expecting Left, but got " ++ show e)

unwrap_spec :: Spec
unwrap_spec = do
  specify "unwrapAll" $ do
    let expected = Array $ V.fromList [Number 100, exp_a, exp_b]
        exp_a = object ["foo" .= Number 100, "bar" .= Number 200]
        exp_b = Array $ V.fromList [Null, Null, exp_c, Bool False]
        exp_c = object ["xxx" .= Number 200, "yyy" .= Bool True]
    (unwrapAll $ forceDecode nestedSample) `shouldBe` expected
  specify "unwrapOne" $ do
    let expected = Array $ V.fromList [Number 100, exp_a, exp_b]
        exp_a = object [ "@type" .= String "g:Map",
                         "@value" .= object ["foo" .= Number 100,
                                             "bar" .= object [ "@type" .= String "g:Int",
                                                               "@value" .= Number 200
                                                             ]
                                            ]
                       ]
        exp_b = object
                [ "@type" .= String "g:List",
                  "@value" .= ( Array $ V.fromList [ Null,
                                                     object ["@type" .= String "g:Object", "@value" .= Null],
                                                     exp_c,
                                                     object ["@type" .= String "g:Boolean", "@value" .= Bool False]
                                                   ]
                              )
                ]
        exp_c = object [ "xxx" .= object ["@type" .= String "g:Int", "@value" .= Number 200],
                         "yyy" .= Bool True
                       ]
    (unwrapOne $ forceDecode nestedSample) `shouldBe` expected

decodeG :: FromGraphSON a => BSL.ByteString -> Either String a
decodeG = parseEither parseGraphSON . forceDecode

fromGraphSON_spec :: Spec
fromGraphSON_spec = describe "FromGraphSON" $ do
  let gint n = gson "g:Int" n
  specify "bare Int" $ do
    let got :: Either String Int
        got = decodeG "199"
    got `shouldBe` Right 199
  specify "wrapped Int" $ do
    let got :: Either String Int
        got = decodeG (gson "g:Int" "256")
    got `shouldBe` Right 256
  specify "bare list" $ do
    let got :: Either String [Int]
        got = decodeG "[1, 2, 3]"
    got `shouldBe` Right [1, 2, 3]
  specify "wrapped list" $ do
    let got :: Either String [Int]
        got = decodeG (gson "g:List" ("[" <> BSL.intercalate "," (map gint ["1","2","3"]) <> "]"))
    got `shouldBe` Right [1, 2, 3]
  specify "bare map" $ do
    let got :: Either String (HashMap Text Int)
        got = decodeG "{\"foo\": 100, \"bar\": 200}"
    got `shouldBe` Right (HM.fromList [("foo", 100), ("bar", 200)])
  specify "wrapped flattened map" $ do
    let got :: Either String (HashMap Text Int)
        got = decodeG (gson "g:Map" ("[\"foo\", " <> gint "100" <> ", \"bar\", " <> gint "200"  <> "]"))
    got `shouldBe` Right (HM.fromList [("foo", 100), ("bar", 200)])
  specify "wrapped flattend map (both keys and values are wrapped)" $ do
    let got :: Either String (HashMap Int Int)
        got = decodeG (gson "g:Map" ("[" <> BSL.intercalate "," (map gint ["1","10","2","20"]) <> "]"))
    got `shouldBe` Right (HM.fromList [(1, 10), (2, 20)])
  specify "bare GMapEntry" $ do
    let got :: Either String (GMapEntry Int Text)
        got = decodeG "{\"123\": \"hoge\"}"
    got `shouldBe` Right (GMapEntry False 123 "hoge")
  specify "wrapped GMapEntry" $ do
    let got :: Either String (GMapEntry Int Text)
        got = decodeG (gson "g:Map" ("[" <> gint "123" <> ", \"hoge\"]"))
    got `shouldBe` Right (GMapEntry True 123 "hoge")
  specify "bare key-value object for GMapEntry" $ do
    let got :: Either String (GMapEntry Int Text)
        got = decodeG "{\"key\": 123, \"value\": \"hoge\"}"
    got `shouldBe` Right (GMapEntry False 123 "hoge")
  specify "bare Maybe" $ do
    let got :: Either String (Maybe Int)
        got = decodeG "null"
    got `shouldBe` Right Nothing
  specify "wrapped Maybe" $ do
    let got :: Either String (Maybe Int)
        got = decodeG (gson "g:Int" "null")
    got `shouldBe` Right Nothing
  specify "bare nested" $ do
    let got :: Either String (HashMap Int [Int])
        got = decodeG "{\"99\": [], \"33\": [1, 2, 3]}"
    got `shouldBe` Right (HM.fromList [(99, []), (33, [1,2,3])])
  specify "wrapped nested" $ do
    let got :: Either String (HashMap [Int] [Int])
        got = decodeG input
        input = gson "g:Map" $ "[" <> key_a <> "," <> val_a <> ", " <> key_b <> ", " <> val_b <> "]"
        key_a = gson "g:List" $ "[" <> gint "1" <> "," <> gint "2" <> "]"
        val_a = gson "g:List" $ "[]"
        key_b = gson "g:List" $ "[" <> gint "3" <> "," <> gint "4" <> ", " <> gint "5" <> "]"
        val_b = gson "g:List" $ "[" <> gint "6" <> "]"
    got `shouldBe` Right (HM.fromList [ ([1,2], []), ([3,4,5], [6])])
