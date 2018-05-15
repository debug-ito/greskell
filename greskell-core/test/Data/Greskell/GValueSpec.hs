{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.GValueSpec (main,spec) where

import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid ((<>))
import Data.Text (Text)
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
    -- TODO: array, object and nested values.

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
