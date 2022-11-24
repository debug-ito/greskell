{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
module Network.Greskell.WebSocket.Codec.JSONSpec
    ( main
    , spec
    ) where

import           Control.Applicative                         ((<$>))
import           Control.Monad                               (forM_)
import           Data.Aeson                                  (Value (Null, Number), (.=))
import qualified Data.Aeson                                  as A
import qualified Data.Aeson.KeyMap                           as KM
import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Lazy                        as BSL
import           Data.Greskell.GraphSON                      (GValue, GValueBody (..),
                                                              nonTypedGValue, typedGValue')
import           Data.Greskell.Greskell                      (Greskell, unsafeGreskell)
import qualified Data.HashMap.Strict                         as HM
import           Data.Maybe                                  (fromJust)
import           Data.Monoid                                 (mempty, (<>))
import qualified Data.UUID                                   as UUID
import qualified Data.Vector                                 as V
import           Test.Hspec

import           Network.Greskell.WebSocket.Codec            (Codec (..))
import           Network.Greskell.WebSocket.Codec.JSON       (jsonCodec)
import           Network.Greskell.WebSocket.Request          (RequestMessage (..), toRequestMessage)
import           Network.Greskell.WebSocket.Request.Common   (Base64 (..), Operation,
                                                              SASLMechanism (..))
import qualified Network.Greskell.WebSocket.Request.Session  as S
import           Network.Greskell.WebSocket.Request.Standard (OpAuthentication (..), OpEval (..))
import           Network.Greskell.WebSocket.Response         (ResponseCode (..),
                                                              ResponseMessage (..),
                                                              ResponseResult (..),
                                                              ResponseStatus (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  decode_spec
  encode_spec

loadSample :: FilePath -> IO BSL.ByteString
loadSample filename = BSL.readFile ("test/samples/" ++ filename)

loadSampleValue :: FilePath -> IO Value
loadSampleValue filename = do
  json_text <- loadSample filename
  case A.eitherDecode' json_text of
   Left e  -> error e
   Right v -> return v

uuidFromString :: String -> UUID.UUID
uuidFromString = fromJust . UUID.fromString

decode_spec :: Spec
decode_spec = describe "decodeWith" $ do
  describe "authentication challenge" $ do
    let codec :: Codec Value
        codec = jsonCodec
        exp_msg = ResponseMessage { requestId = uuidFromString "41d2e28a-20a4-4ab0-b379-d810dede3786",
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
      got <- decodeWith codec <$> loadSample ("response_auth_" ++ graphson_ver ++ ".json")
      got `shouldBe` Right exp_msg

  describe "standard response" $ do
    let codec :: Codec GValue
        codec = jsonCodec
        expMsg d = ResponseMessage { requestId = uuidFromString "41d2e28a-20a4-4ab0-b379-d810dede3786",
                                     status = exp_status,
                                     result = expResult d
                                   }
        exp_status = ResponseStatus { code = Success,
                                      message = "",
                                      attributes = mempty
                                    }
        expResult d = ResponseResult { resultData = d,
                                       meta = mempty
                                     }
        exp_v1 = nonTypedGValue $ GArray $ V.fromList
                 [ nonTypedGValue $ GObject $ KM.fromList
                   [ ("id", nonTypedGValue $ GNumber 1),
                     ("label", nonTypedGValue $ GString "person"),
                     ("type", nonTypedGValue $ GString "vertex")
                   ]
                 ]
        exp_typed_vertex = typedGValue' "g:Vertex" $ GObject $ KM.fromList
                           [ ("id", typedGValue' "g:Int32" $ GNumber 1),
                             ("label", nonTypedGValue $ GString "person")
                           ]
        exp_v2 = nonTypedGValue $ GArray $ V.fromList [exp_typed_vertex]
        exp_v3 = typedGValue' "g:List" $ GArray $ V.fromList [exp_typed_vertex]
        sampleFile v = "response_standard_" ++ v ++ ".json"
    specify "v1" $ do
      got <- decodeWith codec <$> loadSample (sampleFile "v1")
      got `shouldBe` Right (expMsg exp_v1)
    specify "v2" $ do
      got <- decodeWith codec <$> loadSample (sampleFile "v2")
      got `shouldBe` Right (expMsg exp_v2)
    specify "v3" $ do
      got <- decodeWith codec <$> loadSample (sampleFile "v3")
      got `shouldBe` Right (expMsg exp_v3)

encodedValue :: Codec s -> RequestMessage -> Value
encodedValue c req = case A.eitherDecode $ encodeWith c req of
  Left e  -> error e
  Right v -> v

encodeCase :: String -> RequestMessage -> Spec
encodeCase filename input = specify filename $ do
  expected <- loadSampleValue filename
  encodedValue codec input `shouldBe` expected
  where
    codec :: Codec Value
    codec = jsonCodec

encode_spec :: Spec
encode_spec = describe "encodeWith" $ do
  encodeCase "request_auth_v1.json" $ toRequestMessage (uuidFromString "cb682578-9d92-4499-9ebc-5c6aa73c5397")
    $ OpAuthentication
      { batchSize = Nothing,
        sasl = Base64 (BS.singleton 0 <> "stephphen" <> BS.singleton 0 <> "password"),
        saslMechanism = SASLPlain
      }
  encodeCase "request_sessionless_eval_v1.json" $ toRequestMessage (uuidFromString "cb682578-9d92-4499-9ebc-5c6aa73c5397")
    $ OpEval
      { batchSize = Nothing,
        gremlin = "g.V(x)",
        bindings = Just $ KM.fromList [("x", Number 1)],
        language = Just "gremlin-groovy",
        aliases = Nothing,
        scriptEvaluationTimeout = Nothing
      }
  encodeCase "request_sessionless_eval_aliased_v1.json" $ toRequestMessage (uuidFromString "cb682578-9d92-4499-9ebc-5c6aa73c5397")
    $ OpEval
      { batchSize = Nothing,
        gremlin = "social.V(x)",
        bindings = Just $ KM.fromList [("x", Number 1)],
        language = Just "gremlin-groovy",
        aliases = Just $ HM.fromList [("g", "social")],
        scriptEvaluationTimeout = Nothing
      }
  encodeCase "request_session_eval_v1.json" $ toRequestMessage (uuidFromString "cb682578-9d92-4499-9ebc-5c6aa73c5397")
    $ S.OpEval
      { S.batchSize = Nothing,
        S.gremlin = "g.V(x)",
        S.bindings = Just $ KM.fromList [("x", Number 1)],
        S.language = Just "gremlin-groovy",
        S.aliases = Nothing,
        S.scriptEvaluationTimeout = Nothing,
        S.session = uuidFromString "41d2e28a-20a4-4ab0-b379-d810dede3786",
        S.manageTransaction = Nothing
      }
  encodeCase "request_session_eval_aliased_v1.json" $ toRequestMessage (uuidFromString "cb682578-9d92-4499-9ebc-5c6aa73c5397")
    $ S.OpEval
      { S.batchSize = Nothing,
        S.gremlin = "social.V(x)",
        S.bindings = Just $ KM.fromList [("x", Number 1)],
        S.language = Just "gremlin-groovy",
        S.aliases = Just $ HM.fromList [("g", "social")],
        S.scriptEvaluationTimeout = Nothing,
        S.session = uuidFromString "41d2e28a-20a4-4ab0-b379-d810dede3786",
        S.manageTransaction = Nothing
      }
  encodeCase "request_session_close_v1.json" $ toRequestMessage (uuidFromString "cb682578-9d92-4499-9ebc-5c6aa73c5397")
    $ S.OpClose
      { S.batchSize = Nothing,
        S.session = uuidFromString "41d2e28a-20a4-4ab0-b379-d810dede3786",
        S.force = Nothing
      }
