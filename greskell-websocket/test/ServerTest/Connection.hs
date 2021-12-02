{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module ServerTest.Connection (main,spec) where

import Control.Exception.Safe (bracket, Exception, withException, SomeException, throwString)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently, Async, withAsync, async, wait)
import Control.Concurrent.STM
  ( newEmptyTMVarIO, putTMVar, takeTMVar, atomically,
    TVar, newTVarIO, modifyTVar, readTVar,
    TQueue, newTQueueIO, writeTQueue, flushTQueue
  )
import Control.Monad (when, forever, forM_, mapM)
import Data.Aeson (Value(Number), FromJSON(..), ToJSON(toJSON), Object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (parseEither)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Monoid ((<>))
import Data.Greskell.GraphSON (GraphSON, gsonValue)
import Data.Text (Text, pack)
import qualified Data.Vector as V
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import qualified Network.WebSockets as WS
import System.IO (stderr, hPutStrLn)
import Test.Hspec

import Network.Greskell.WebSocket.Connection
  ( Host, Port, Connection, ResponseHandle,
    close, connect, sendRequest', sendRequest, slurpResponses,
    nextResponse,
    RequestException(..), GeneralException(..),
    Settings(onGeneralException, responseTimeout, concurrency, requestQueueSize),
    defJSONSettings
  )
import Network.Greskell.WebSocket.Request
  ( RequestMessage(requestId), toRequestMessage, makeRequestMessage
  )
import Network.Greskell.WebSocket.Request.Standard (OpEval(..))
import Network.Greskell.WebSocket.Response
  ( ResponseMessage(requestId, status, result), ResponseStatus(code), ResponseCode(..),
    ResponseResult(resultData)
  )
import qualified Network.Greskell.WebSocket.Response as Response

import qualified TestUtil.TCounter as TCounter
import TestUtil.Env (withEnvForExtServer, withEnvForIntServer)
import TestUtil.MockServer (wsServer, receiveRequest, simpleRawResponse, waitForServer)


main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Connection" $ do
  no_external_server_spec
  withEnvForExtServer $ do
    conn_basic_spec
    conn_error_spec
    conn_close_spec
  withEnvForIntServer $ do
    conn_bad_server_spec

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
slurpParseEval rh = (fmap . fmap . fmap) parseValue $ fmap V.toList $ slurpResponses rh

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
no_external_server_spec = describe "connect" $ do
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
                            bindings = Just $ KM.fromList [("x", toJSON ([1 .. 10] :: [Int]))]
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
      tcounter <- TCounter.new
      let makeReq v = TCounter.count tcounter
                      (sendRequest conn $ opSleep' 500 v)
                      (\rh -> slurpEvalValues rh :: IO [Either String [Int]])
      got <- mapConcurrently makeReq [1..10]
      got `shouldBe` map (\v -> [Right [v]]) [1..10]
      got_hist <- TCounter.history tcounter
      length got_hist `shouldBe` (2 * 10)
      forM_ got_hist $ \conc -> conc `shouldSatisfy` (<= exp_concurrency_max)
  specify "multiple threads reading a single ResponseHandle" $ withConn $ \conn -> do
    let op = (opEval "[1,2,3,4,5,6,7,8,9,10]")
             { batchSize = Just 1
             }
    rh <- sendRequest conn op
    mgot <- mapConcurrently (const $ nextResponse rh) ([1..15] :: [Int])
    (length $ filter isNothing mgot) `shouldBe` 5
    let got = map fromJust $ filter isJust mgot
        got_data = map (responseValues . fmap parseValue) got :: [Either String [Int]]
    map (code . status) got `shouldMatchList` (Success : replicate 9 PartialContent)
    got_data `shouldMatchList` map (\v -> Right [v]) [1..10]

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
    nextResponse ng_rh `shouldThrow` expEx
  specify "request timeout" $ \(host, port) -> do
    let settings = ourSettings { responseTimeout = 1 }
        expEx ResponseTimeout = True
        expEx _ = False
    forConn' settings host port $ \conn -> do
      rh <- sendRequest conn $ opSleep 2000
      (nextResponse rh) `shouldThrow` expEx

conn_close_spec :: SpecWith (Host, Port)
conn_close_spec = describe "close" $ do
  it "should wait for a pending request to finish" $ withConn $ \conn -> do
    rh <- sendRequest conn $ opSleep 100
    close conn
    got <- slurpEvalValues rh :: IO [Either String [Int]]
    got `shouldBe` [Right [100]]
  it "should block for all pending requests" $ withConn $ \conn -> do
    tc <- TCounter.new
    let makeReq :: Int -> IO [Either String [Int]]
        makeReq t = do
          ret <- TCounter.count tc (sendRequest conn $ opSleep t) slurpEvalValues
          close conn  -- more than one close don't hurt anybody (after all requests are sent.)
          return ret
    req_threads <- mapM (async . makeReq) [200, 400, 600]
    TCounter.waitFor tc (== 3)
    close conn
    TCounter.now tc `shouldReturn` 0
    got_hist <- TCounter.history tc
    length got_hist `shouldBe` 6
    got <- mapM wait req_threads
    got `shouldBe` map (\v -> [Right [v]]) [200, 400, 600]
  it "should make nextResponse throw AlreadyClosed exception. nextResponse should throw it every time it's called" $ withConn $ \conn -> do
    let expEx AlreadyClosed = True
        expEx _ = False
    close conn
    rh <- sendRequest conn $ opEval "999"
    nextResponse rh `shouldThrow` expEx
    nextResponse rh `shouldThrow` expEx 
    nextResponse rh `shouldThrow` expEx
  

succUUID :: UUID -> UUID
succUUID orig = UUID.fromWords a b c d'
  where
    (a, b, c ,d) = UUID.toWords orig
    d' = succ d

conn_bad_server_spec :: SpecWith Port
conn_bad_server_spec = do
  describe "ResponseHandle" $ describe "nextResponse" $ do
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
          (inspectException $ nextResponse rh) `shouldThrow` exp_ex
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
          (inspectException $ nextResponse rh) `shouldThrow` exp_ex
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
    it "should throw AlreadyClosed exception after the server actively closes the connection" $ \port -> do
      let server = wsServer port $ \wsconn -> do
            threadDelay 10000
            WS.sendClose wsconn ("" :: Text)
          expEx AlreadyClosed = True
          expEx _ = False
      withAsync server $ \_ -> do
        waitForServer
        forConn "localhost" port $ \conn -> do
          threadDelay 40000
          rh <- sendRequest conn $ opEval "256"
          nextResponse rh `shouldThrow` expEx
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
            mgot <- (fmap . fmap) (responseValues . fmap parseValue) $ nextResponse rh :: IO (Maybe (Either String [Int]))
            mgot `shouldBe` (Just $ Right [1])
          slurpResponses rh `shouldThrow` expEx
          
