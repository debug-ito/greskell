{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module Main (main,spec) where

import Control.Exception.Safe (bracket, Exception, withException, SomeException, throwString)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently, Async, withAsync)
import Control.Monad (when)
import Data.Aeson (Value(Number), FromJSON(..), ToJSON(toJSON), Object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (parseEither)
import Data.Monoid ((<>))
import qualified Data.HashMap.Strict as HM
import Data.Greskell.GraphSON (GraphSON, gsonValue)
import Data.Text (Text, pack)
import Data.UUID.V4 (nextRandom)
import qualified Network.WebSockets as WS
import System.Environment (lookupEnv)
import System.IO (stderr, hPutStrLn)
import Test.Hspec

import Data.Greskell.WebSocket.Connection
  ( Host, Port, Connection, ResponseHandle,
    close, connect, sendRequest', sendRequest, slurpResponses,
    getResponse,
    RequestException(..),
    Settings(onGeneralException), defJSONSettings
  )
import Data.Greskell.WebSocket.Request
  ( RequestMessage(requestId), toRequestMessage, makeRequestMessage
  )
import Data.Greskell.WebSocket.Request.Standard (OpEval(..))
import Data.Greskell.WebSocket.Response
  ( ResponseMessage(requestId, status, result), ResponseStatus(code), ResponseCode(..),
    ResponseResult(resultData)
  )
import qualified Data.Greskell.WebSocket.Response as Response

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  no_external_server_spec
  withEnvForExtServer $ do
    describe "Connection" $ do
      conn_basic_spec
      conn_error_spec
  withEnvForIntServer $ do
    conn_bad_server_spec

requireEnv :: String -> IO String
requireEnv env_key = maybe bail return =<< lookupEnv env_key
  where
    bail = pendingWith msg >> return ""
      where
        msg = "Set environment variable "++ env_key ++ " for Server test. "

withEnvForExtServer :: SpecWith (Host, Port) -> Spec
withEnvForExtServer = before $ do
  hostname <- requireEnv "GRESKELL_TEST_HOST"
  port <- fmap read $ requireEnv "GRESKELL_TEST_PORT"
  return (hostname, port)

withEnvForIntServer :: SpecWith Port -> Spec
withEnvForIntServer = before $ fmap read $ requireEnv "GRESKELL_TEST_INTERNAL_PORT"

ourSettings :: Settings Value
ourSettings = defJSONSettings
              { onGeneralException = \_ e -> error (show e)
                -- basically we don't expect any GeneralException.
              }

withConn :: (Connection Value -> IO a) -> (Host, Port) -> IO a
withConn act (host, port) = bracket makeConn close act
  where
    makeConn = connect defJSONSettings host port

forConn :: Host -> Port -> (Connection Value -> IO a) -> IO a
forConn host port act = withConn act (host, port)

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
opSleep time_ms = opSleep' time_ms time_ms

opSleep' :: Int -> Int -> OpEval
opSleep' time_ms val = opEval ("sleep " <> tshow time_ms <> "; " <> tshow val)
  where
    tshow = pack . show

inspectException :: IO a -> IO a
inspectException act = act `withException` printE
  where
    printE :: SomeException -> IO ()
    printE e = hPutStrLn stderr ("[DEBUG] exception thrown: " ++ show e)

no_external_server_spec :: Spec
no_external_server_spec = describe "Connection" $ describe "connect" $ do
  it "should throw exception on failure" $ do
    let act = connect ourSettings "this.should.not.be.real.server.com" 8000
        expectSomeEx :: SomeException -> Bool
        expectSomeEx _ = True
    inspectException act `shouldThrow` expectSomeEx
    
  

conn_basic_spec :: SpecWith (Host, Port)
conn_basic_spec = do
  specify "basic transaction" $ withConn $ \conn -> do
    rid <- nextRandom
    let op = opEval "123"
        exp_val :: [Int]
        exp_val = [123]
    res <- sendRequest' conn $ toRequestMessage rid op
    got <- slurpParseEval res
    map (Response.requestId) got `shouldBe` [rid]
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
    map (Response.requestId) got `shouldBe` replicate 5 rid
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
    let reqAndRes val = (fmap . map) responseValues $ slurpParseEval =<< (sendRequest conn $ opSleep' 200 val)
    got <- mapConcurrently reqAndRes [1 .. 5]
           :: IO [[Either String [Int]]]
    got `shouldBe` [ [Right [1]],
                     [Right [2]],
                     [Right [3]],
                     [Right [4]],
                     [Right [5]]
                   ]
  specify "requestId should be cleared from the request pool once completed" $ withConn $ \conn -> do
    req <- makeRequestMessage $ opSleep 30
    let evalReq = (fmap . map) responseValues $ slurpParseEval =<< sendRequest' conn req :: IO [Either String [Int]]
    evalReq `shouldReturn` [Right [30]]
    evalReq `shouldReturn` [Right [30]]
          

conn_error_spec :: SpecWith (Host, Port)
conn_error_spec = do
  specify "duplicate requests" $ withConn $ \conn -> do
    req <- makeRequestMessage $ opSleep 300
    let expEx (DuplicateRequestId got_rid) = got_rid == requestId (req :: RequestMessage)
        expEx _ = False
    ok_rh <- sendRequest' conn req
    ng_rh <- sendRequest' conn req
    ok_res <- (fmap . map) responseValues $ slurpParseEval ok_rh :: IO [Either String [Int]]
    ok_res `shouldBe` [Right [300]]
    getResponse ng_rh `shouldThrow` expEx

wsServer :: Int -- ^ port number
         -> (WS.Connection -> IO ())
         -> IO ()
wsServer p act = WS.runServer "localhost" p $ \pending_conn ->
  act =<< WS.acceptRequest pending_conn

conn_bad_server_spec :: SpecWith Port
conn_bad_server_spec = do
  describe "ResponseHandle" $ describe "getResponse" $ do
    let waitForServer = threadDelay 100000
    it "should throw exception when the server closed unexpectedly" $ \port -> do
      let server = wsServer port $ \wsconn -> do
            _ <- WS.receiveDataMessage wsconn
            throwString ( "Server connection abort. Seeing this message in a test console is OK."
                          ++ " It's because of WS.runServer internals."
                        )
          exp_ex :: WS.ConnectionException -> Bool
          exp_ex WS.ConnectionClosed = True
          exp_ex _ = False
      withAsync server $ \_ -> do
        waitForServer
        forConn "localhost" port $ \conn -> do
          rh <- sendRequest conn $ opEval "100"
          (inspectException $ getResponse rh) `shouldThrow` exp_ex
    it "should throw exception when the server cleanly closed connection while there is a pending request" $ \port -> do
      let server = wsServer port $ \wsconn -> do
            _ <- WS.receiveDataMessage wsconn
            WS.sendClose wsconn ("" :: Text)
            (WS.ControlMessage (WS.Close wscode _)) <- WS.receive wsconn
            when (wscode /= 1000) $ do
              throwString ( "Fatal: expects WebSocket status code 1000, but got "
                            ++ show wscode
                          )
          exp_ex ServerClosed = True
          exp_ex _ = False
      withAsync server $ \_ -> do
        waitForServer
        forConn "localhost" port $ \conn -> do
          rh <- sendRequest conn $ opEval "100"
          (inspectException $ getResponse rh) `shouldThrow` exp_ex
          
