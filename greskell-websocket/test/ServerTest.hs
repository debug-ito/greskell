{-# LANGUAGE OverloadedStrings #-}
module Main (main,spec) where

import Control.Exception.Safe (bracket)
import Data.Aeson (Value, FromJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (parseEither)
import Data.Greskell.GraphSON (GraphSON, gsonValue)
import Data.UUID.V4 (nextRandom)
import System.Environment (lookupEnv)
import Test.Hspec

import Data.Greskell.WebSocket.Codec.JSON (jsonCodec)
import Data.Greskell.WebSocket.Connection
  ( Host, Port, Connection,
    close, connect, sendRequest', slurpResponses
  )
import Data.Greskell.WebSocket.Request (toRequestMessage)
import Data.Greskell.WebSocket.Request.Standard (OpEval(..))
import Data.Greskell.WebSocket.Response
  ( ResponseMessage(requestId, status, result), ResponseStatus(code), ResponseCode(..),
    ResponseResult(resultData)
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = withEnv $ do
  describe "Connection" $ do
    conn_basic_spec

requireEnv :: String -> IO String
requireEnv env_key = maybe bail return =<< lookupEnv env_key
  where
    bail = pendingWith msg >> return ""
      where
        msg = "Set environment variable "++ env_key ++ " for Server test. "

withEnv :: SpecWith (Host, Port) -> Spec
withEnv = before $ do
  hostname <- requireEnv "GRESKELL_TEST_HOST"
  port <- fmap read $ requireEnv "GRESKELL_TEST_PORT"
  return (hostname, port)

withConn :: (Connection Value -> IO a) -> (Host, Port) -> IO a
withConn act (host, port) = bracket makeConn close act
  where
    makeConn = connect jsonCodec host port "/gremlin"

parseValue :: FromJSON a => Value -> Either String a
parseValue v = Aeson.parseEither parseJSON v

conn_basic_spec :: SpecWith (Host, Port)
conn_basic_spec = do
  specify "basic transaction" $ withConn $ \conn -> do
    rid <- nextRandom
    let op = OpEval { batchSize = Nothing,
                      gremlin = "123",
                      bindings = Nothing,
                      language = Nothing,
                      aliases = Nothing,
                      scriptEvaluationTimeout = Nothing
                    }
        exp_val :: [Int]
        exp_val = [123]
    res <- sendRequest' conn $ toRequestMessage rid op
    got <- (fmap . fmap . fmap) parseValue $ slurpResponses res
    map (requestId) got `shouldBe` [rid]
    map (code . status) got `shouldBe` [Success]
    map (fmap gsonValue . resultData . result) got `shouldBe` [Right exp_val]

