{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module Main (main,spec) where

import Control.Exception.Safe (bracket, Exception, withException, SomeException, throwString)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently, Async, withAsync)
import Control.Concurrent.STM
  ( newEmptyTMVarIO, putTMVar, takeTMVar, atomically,
    TVar, newTVarIO, modifyTVar, readTVar,
    TQueue, newTQueueIO, writeTQueue, flushTQueue
  )
import Control.Monad (when, forever, forM_)
import Data.Aeson (Value(Number), FromJSON(..), ToJSON(toJSON), Object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (parseEither)
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid ((<>))
import qualified Data.HashMap.Strict as HM
import Data.Greskell.GraphSON (GraphSON, gsonValue)
import Data.Text (Text, pack)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import qualified Network.WebSockets as WS
import System.Environment (lookupEnv)
import System.IO (stderr, hPutStrLn)
import Test.Hspec

import Data.Greskell.WebSocket.Codec (decodeBinary)
import Data.Greskell.WebSocket.Connection
  ( Host, Port, Connection, ResponseHandle,
    close, connect, sendRequest', sendRequest, slurpResponses,
    getResponse,
    RequestException(..), GeneralException(..),
    Settings(onGeneralException, responseTimeout, concurrency, requestQueueSize),
    defJSONSettings
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
              { onGeneralException = \e -> error (show e)
                -- basically we don't expect any GeneralException.
              }

withConn :: (Connection Value -> IO a) -> (Host, Port) -> IO a
withConn act (host, port) = forConn host port act

forConn :: Host -> Port -> (Connection Value -> IO a) -> IO a
forConn = forConn' defJSONSettings

forConn' :: Settings Value -> Host -> Port -> (Connection Value -> IO a) -> IO a
forConn' settings host port act = bracket makeConn close act
  where
    makeConn = connect settings host port

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

slurpEvalValues :: FromJSON a => ResponseHandle Value -> IO [Either String [a]]
slurpEvalValues rh = (fmap . map) responseValues $ slurpParseEval rh


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
    got <- mapM slurpEvalValues handles :: IO [[Either String [Int]]]
    got `shouldBe` [ [Right [500]],
                     [Right [400]],
                     [Right [300]],
                     [Right [200]],
                     [Right [100]]
                   ]
  specify "make requests from multiple threads" $ withConn $ \conn -> do
    let reqAndRes val = slurpEvalValues =<< (sendRequest conn $ opSleep' 200 val)
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
    let evalReq = slurpEvalValues =<< sendRequest' conn req :: IO [Either String [Int]]
    evalReq `shouldReturn` [Right [30]]
    evalReq `shouldReturn` [Right [30]]
  specify "client concurrency should be bounded by 'concurrency' + 'requestQueueSize'" $ \(host, port) -> do
    let input_concurrency = 2
        input_qreq_size = 1
        settings = defJSONSettings { concurrency = input_concurrency,
                                     requestQueueSize = input_qreq_size
                                   }
        exp_concurrency_max = input_concurrency + input_qreq_size
    forConn' settings host port $ \conn -> do
      var_cur_concurrency <- newTVarIO 0 :: IO (TVar Int)
      concurrency_history <- newTQueueIO :: IO (TQueue Int)
      let  updateConc f = do
             modifyTVar var_cur_concurrency f
             conc <- readTVar var_cur_concurrency
             writeTQueue concurrency_history conc
           makeReq v = do
             rh <- sendRequest conn $ opSleep' 2000 v
             atomically $ updateConc (+ 1)
             ret <- slurpEvalValues rh :: IO [Either String [Int]]
             atomically $ updateConc (subtract 1)
             return ret
      got <- mapConcurrently makeReq [1..10]
      got `shouldBe` map (\v -> [Right [v]]) [1..10]
      got_hist <- atomically $ flushTQueue concurrency_history
      length got_hist `shouldBe` (2 * 10)
      forM_ got_hist $ \conc -> conc `shouldSatisfy` (<= exp_concurrency_max)


conn_error_spec :: SpecWith (Host, Port)
conn_error_spec = do
  specify "duplicate requests" $ withConn $ \conn -> do
    req <- makeRequestMessage $ opSleep 300
    let expEx (DuplicateRequestId got_rid) = got_rid == requestId (req :: RequestMessage)
        expEx _ = False
    ok_rh <- sendRequest' conn req
    ng_rh <- sendRequest' conn req
    ok_res <- slurpEvalValues ok_rh :: IO [Either String [Int]]
    ok_res `shouldBe` [Right [300]]
    getResponse ng_rh `shouldThrow` expEx
  specify "request timeout" $ \(host, port) -> do
    let settings = ourSettings { responseTimeout = 1 }
        expEx ResponseTimeout = True
        expEx _ = False
    forConn' settings host port $ \conn -> do
      rh <- sendRequest conn $ opSleep 2000
      (getResponse rh) `shouldThrow` expEx

wsServer :: Int -- ^ port number
         -> (WS.Connection -> IO ())
         -> IO ()
wsServer p act = WS.runServer "localhost" p $ \pending_conn ->
  act =<< WS.acceptRequest pending_conn

parseRequest :: BSL.ByteString -> Either String RequestMessage
parseRequest raw_msg = do
  (_, payload) <- decodeBinary raw_msg
  Aeson.eitherDecode payload

receiveRequest :: WS.Connection -> IO RequestMessage
receiveRequest wsconn = do
  raw_msg <- WS.receiveData wsconn
  case parseRequest raw_msg of
   Left e -> throwString e
   Right r -> return r

simpleRawResponse :: UUID -> Int -> Text -> Text
simpleRawResponse request_id status_code data_content =
  "{\"requestId\":\"" <> UUID.toText request_id <> "\","
  <> "\"status\":{\"code\":" <> (pack $ show status_code) <> ",\"message\":\"\",\"attributes\":{}},"
  <> "\"result\":{\"data\":" <> data_content <> ",\"meta\":{}}}"

succUUID :: UUID -> UUID
succUUID orig = UUID.fromWords a b c d'
  where
    (a, b, c ,d) = UUID.toWords orig
    d' = succ d

conn_bad_server_spec :: SpecWith Port
conn_bad_server_spec = do
  let waitForServer = threadDelay 100000
  describe "ResponseHandle" $ describe "getResponse" $ do
    it "should throw exception when the server closes the connection while there is a pending request" $ \port -> do
      let server = wsServer port $ \wsconn -> do
            _ <- WS.receiveDataMessage wsconn
            throwString ( "Server connection abort. Seeing this message in a test console is OK."
                          ++ " It's because of WS.runServer internals."
                        )
          exp_ex ServerClosed = True
          exp_ex _ = False
      withAsync server $ \_ -> do
        waitForServer
        forConn "localhost" port $ \conn -> do
          rh <- sendRequest conn $ opEval "100"
          (inspectException $ getResponse rh) `shouldThrow` exp_ex
    it "should throw exception when the server sends Close request while there is a pending request" $ \port -> do
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
    it "should be ok if the server actively closes the connection" $ \port -> do
      let server = wsServer port $ \wsconn -> do
            req <- receiveRequest wsconn
            WS.sendBinaryData wsconn $ simpleRawResponse (requestId (req :: RequestMessage)) 200 "[99]"
      withAsync server $ \_ -> do
        waitForServer
        forConn "localhost" port $ \conn -> do
          rh <- sendRequest conn $ opEval "99"
          got <- slurpEvalValues rh :: IO [Either String [Int]]
          got `shouldBe` [Right [99]]
  describe "Settings" $ describe "onGeneralException" $ do
    it "should be called on unexpected requestId" $ \port -> do
      report_gex <- newEmptyTMVarIO
      let server = wsServer port $ \wsconn -> do
            req <- receiveRequest wsconn
            let res_id = succUUID $ requestId (req :: RequestMessage)  -- deliberately send wrong requestId.
                res = simpleRawResponse res_id 200 "[333]"
            WS.sendBinaryData wsconn res
          reportEx ex = atomically $ putTMVar report_gex ex
          settings = defJSONSettings { onGeneralException = reportEx }
      withAsync server $ \_ -> do
        waitForServer
        forConn' settings "localhost" port $ \conn -> do
          req <- makeRequestMessage $ opEval "333"
          let exp_res_id = succUUID $ requestId (req :: RequestMessage)
          _ <- sendRequest' conn req
          got <- atomically $ takeTMVar report_gex
          got `shouldBe` UnexpectedRequestId exp_res_id
    it "should be called on failure to parse response" $ \port -> do
      report_gex <- newEmptyTMVarIO
      let server = wsServer port $ \wsconn -> do
            _ <- receiveRequest wsconn
            WS.sendBinaryData wsconn ("hoge hoge hoge" :: Text)
          settings = defJSONSettings { onGeneralException = \e -> atomically $ putTMVar report_gex e }
      withAsync server $ \_ -> do
        waitForServer
        forConn' settings "localhost" port $ \conn -> do
          let expEx (ResponseParseFailure _) = True
              expEx _ = False
          _ <- sendRequest conn $ opEval "100"
          got <- atomically $ takeTMVar report_gex
          got `shouldSatisfy` expEx
    it "should throw ResponseTimeout exception when the server endlessly sends responses" $ \port -> do
      let server = wsServer port $ \wsconn -> do
            req <- receiveRequest wsconn
            let res_id = requestId (req :: RequestMessage)
            sendContinuousRes wsconn res_id 20
          sendContinuousRes :: WS.Connection -> UUID -> Int -> IO ()
          sendContinuousRes wsconn res_id n = do
            let finish = n == 0
            threadDelay 200000
            WS.sendBinaryData wsconn $ simpleRawResponse res_id (if finish then 200 else 206) "[1]"
            if finish
              then return ()
              else sendContinuousRes wsconn res_id (n - 1)
          settings = ourSettings { responseTimeout = 1 }
          expEx ResponseTimeout = True
          expEx _ = False
      withAsync server $ \_ -> do
        waitForServer
        forConn' settings "localhost" port $ \conn -> do
          rh <- sendRequest conn $ opEval "99"
          forM_ ([1..3] :: [Int]) $ \_ -> do
            mgot <- (fmap . fmap) (responseValues . fmap parseValue) $ getResponse rh :: IO (Maybe (Either String [Int]))
            mgot `shouldBe` (Just $ Right [1])
          slurpResponses rh `shouldThrow` expEx
          
