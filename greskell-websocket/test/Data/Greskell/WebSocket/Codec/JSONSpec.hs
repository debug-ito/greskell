{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.WebSocket.Codec.JSONSpec (main,spec) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Aeson (Value(Null))
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromJust)
import Data.Monoid (mempty)
import qualified Data.UUID as UUID
import Test.Hspec

import Data.Greskell.WebSocket.Request (OpEval)
import Data.Greskell.WebSocket.Response
  (ResponseMessage(..), ResponseStatus(..), ResponseResult(..), ResponseCode(..))
import Data.Greskell.WebSocket.Codec (Codec(..))
import Data.Greskell.WebSocket.Codec.JSON (jsonCodec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  decode_spec

decode_spec :: Spec
decode_spec = describe "decodeWith" $ do
  describe "authentication challenge" $ do
    let codec :: Codec OpEval Value
        codec = jsonCodec
        exp_msg = ResponseMessage { requestId = fromJust $ UUID.fromString "41d2e28a-20a4-4ab0-b379-d810dede3786",
                                    status = exp_status,
                                    result = exp_result
                                  }
        exp_status = ResponseStatus { code = Authenticate,
                                      message = "",
                                      attributes = mempty
                                    }
        exp_result = ResponseResult { resultData = Null,
                                      meta = mempty
                                    }
    forM_ ["v1", "v2", "v3"] $ \graphson_ver -> specify graphson_ver $ do
      got <- decodeWith codec <$> BSL.readFile ("samples/data/response_auth_" ++ graphson_ver ++ ".json")
      got `shouldBe` Right exp_msg
      
    
