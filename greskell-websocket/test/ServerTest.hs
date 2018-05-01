{-# LANGUAGE OverloadedStrings #-}
module Main (main,spec) where

import Control.Exception.Safe (bracket)
import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (Value(Number), FromJSON(..), ToJSON(toJSON), Object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (parseEither)
import Data.Monoid ((<>))
import qualified Data.HashMap.Strict as HM
import Data.Greskell.GraphSON (GraphSON, gsonValue)
import Data.Text (Text, pack)
import Data.UUID.V4 (nextRandom)
import System.Environment (lookupEnv)
import Test.Hspec

import Data.Greskell.WebSocket.Codec.JSON (jsonCodec)
import Data.Greskell.WebSocket.Codec (Codec)
import Data.Greskell.WebSocket.Connection
  ( Host, Port, Connection, ResponseHandle,
    close, connect, sendRequest', sendRequest, slurpResponses,
    ConnectException(..)
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
spec = do
  no_external_server_spec
  withEnv $ do
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
    makeConn = connect jsonCodec host port

parseValue :: FromJSON a => Value -> Either String a
parseValue v = Aeson.parseEither parseJSON v

opEval :: Text -> OpEval
opEval g = OpEval { batchSize = Nothing,
                    gremlin = g,
                    bindings = Nothing,
                    language = Nothing,
                    aliases = Nothing,
                    scriptEvaluationTimeout = Nothing
                  }

---- 'resultData' should be parsed with GraphSON wrappers to support v1, v2 and v3.
slurpParseEval :: FromJSON a => ResponseHandle Value -> IO [ResponseMessage (Either String (GraphSON [GraphSON a]))]
slurpParseEval rh = (fmap . fmap . fmap) parseValue $ slurpResponses rh

responseValues :: ResponseMessage (Either String (GraphSON [GraphSON a])) -> Either String [a]
responseValues = fmap (map gsonValue . gsonValue) . resultData . result

opSleep :: Int -> OpEval
opSleep time_ms = opEval ("sleep " <> time_str <> "; " <> time_str)
  where
    time_str = pack $ show time_ms

no_external_server_spec :: Spec
no_external_server_spec = describe "Connection" $ describe "connect" $ do
  it "should throw exception on failure" $ do
    let act = connect (jsonCodec :: Codec Value) "this.should.not.be.real.server.com" 8000
    act `shouldThrow` (\ConnectException -> True)
    
  

conn_basic_spec :: SpecWith (Host, Port)
conn_basic_spec = do
  specify "basic transaction" $ withConn $ \conn -> do
    rid <- nextRandom
    let op = opEval "123"
        exp_val :: [Int]
        exp_val = [123]
    res <- sendRequest' conn $ toRequestMessage rid op
    got <- slurpParseEval res
    map (requestId) got `shouldBe` [rid]
    map (code . status) got `shouldBe` [Success]
    map responseValues got `shouldBe` [Right exp_val]
  specify "continuous response with bindings" $ withConn $ \conn -> do
    rid <- nextRandom
    let op = (opEval "x") { batchSize = Just 2,
                            bindings = Just $ HM.fromList [("x", toJSON ([1 .. 10] :: [Int]))]
                          }
        exp_vals :: [Either String [Int]]
        exp_vals = map Right [[1,2], [3,4], [5,6], [7,8], [9,10]]
    got <- slurpParseEval =<< (sendRequest' conn $ toRequestMessage rid op)
    map (requestId) got `shouldBe` replicate 5 rid
    map (code . status) got `shouldBe` ((replicate 4 PartialContent) ++ [Success])
    map responseValues got `shouldBe` exp_vals
  specify "concurrent requests" $ withConn $ \conn -> do
    handles <- mapM (sendRequest conn) $ map opSleep $ map (* 100) $ reverse [1..5]
    got <- (fmap . map . map) responseValues $ mapM slurpParseEval handles :: IO [[Either String [Int]]]
    got `shouldBe` [ [Right [500]],
                     [Right [400]],
                     [Right [300]],
                     [Right [200]],
                     [Right [100]]
                   ]
  specify "make requests from multiple threads" $ withConn $ \conn -> do
    let reqAndRes t = (fmap . map) responseValues $ slurpParseEval =<< (sendRequest conn $ opSleep t)
    got <- mapConcurrently reqAndRes $ replicate 5 200
           :: IO [[Either String [Int]]]
    got `shouldBe` [ [Right [200]],
                     [Right [200]],
                     [Right [200]],
                     [Right [200]],
                     [Right [200]]
                   ]
