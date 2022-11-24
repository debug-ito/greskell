{-# LANGUAGE OverloadedStrings #-}
module ServerTest.Client
    ( main
    , spec
    ) where

import           Control.Concurrent                    (threadDelay)
import           Control.Concurrent.Async              (mapConcurrently, withAsync)
import           Control.Exception.Safe                (bracket)
import           Control.Monad                         (forM_)
import           Data.Aeson                            (Value (Number))
import qualified Data.Aeson.KeyMap                     as KM
import           Data.HashMap.Strict                   (HashMap)
import           Data.Maybe                            (fromJust, isJust, isNothing)
import           Data.Text                             (Text)
import qualified Data.Vector                           as V
import qualified Network.WebSockets                    as WS
import           Test.Hspec

import           Data.Greskell.GMap                    (GMap, GMapEntry, unGMapEntry)
import           Data.Greskell.Greskell                (Greskell)
import qualified Data.Greskell.Greskell                as G

import           Network.Greskell.WebSocket.Client     (Client, Host, Options, Port,
                                                        SubmitException (ParseError, ResponseError),
                                                        batchSize, close, connectWith,
                                                        connectionSettings, defOptions, nextResult,
                                                        responseTimeout, slurpResults, submit)
import           Network.Greskell.WebSocket.Connection (RequestException (ResponseTimeout))
import           Network.Greskell.WebSocket.Request    (RequestMessage (requestId))
import qualified Network.Greskell.WebSocket.Response   as Res

import           TestUtil.Env                          (withEnvForExtServer, withEnvForIntServer)
import           TestUtil.MockServer                   (receiveRequest, simpleRawResponse,
                                                        waitForServer, wsServer)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Client" $ do
  withEnvForExtServer $ do
    client_basic_spec
  withEnvForIntServer $ do
    bad_server_spec

withClient :: (Client -> IO a) -> (Host, Port) -> IO a
withClient act (host, port) = forClient' defOptions host port act

forClient :: Host -> Port -> (Client -> IO a) -> IO a
forClient = forClient' defOptions

forClient' :: Options -> Host -> Port -> (Client -> IO a) -> IO a
forClient' opt host port act = bracket (connectWith opt host port) close act

client_basic_spec :: SpecWith (Host, Port)
client_basic_spec = do
  specify "eval Int" $ withClient $ \client -> do
    let g = 108 :: Greskell Int
    rh <- submit client g Nothing
    nextResult rh `shouldReturn` Just 108
    nextResult rh `shouldReturn` Nothing
    nextResult rh `shouldReturn` Nothing
  specify "eval Text" $ withClient $ \client -> do
    let g = "hoge" :: Greskell Text
    rh <- submit client g Nothing
    nextResult rh `shouldReturn` Just "hoge"
    nextResult rh `shouldReturn` Nothing
    nextResult rh `shouldReturn` Nothing
  specify "eval [Int]" $ withClient $ \client -> do
    let g = G.list $ map fromInteger [1..20] :: Greskell [Int]
    rh <- submit client g Nothing
    forM_  [1..20] $ \n ->
      nextResult rh `shouldReturn` Just n
    nextResult rh `shouldReturn` Nothing
    nextResult rh `shouldReturn` Nothing
  specify "eval (GMap HashMap Int Text)" $ withClient $ \client -> do
    let g :: Greskell (GMap HashMap Int Text)
        g = G.unsafeGreskell "[100: 'hoge', 200: 'foo', 300: 'bar']"
    rh <- submit client g Nothing
    got <- fmap (fmap unGMapEntry) $ slurpResults rh
    (V.toList got) `shouldMatchList` [(100, "hoge"), (200, "foo"), (300, "bar")]
  specify "eval (Maybe Int)" $ withClient $ \client -> do
    let g :: Greskell (Maybe Int)
        g = G.unsafeGreskell "100"
    rh <- submit client g Nothing
    slurpResults rh `shouldReturn` V.fromList [Just 100]
  specify "eval (bound Double)" $ withClient $ \client -> do
    let gx = G.unsafeGreskell "x"
        g = 92.125 + gx :: Greskell Double
        b = KM.fromList [("x", Number 22.25)]
    rh <- submit client g (Just b)
    slurpResults rh `shouldReturn` V.fromList [114.375]
  specify "multiple response messages" $ \(host, port) -> do
    let opt = defOptions { batchSize = Just 3
                         }
        g = G.list $ map fromInteger [1..31] :: Greskell [Int]
    forClient' opt host port $ \client -> do
      rh <- submit client g Nothing
      slurpResults rh `shouldReturn` V.fromList [1..31]
  specify "ParseError exception" $ withClient $ \client -> do
    let g :: Greskell Int
        g = G.unsafeGreskell "\"some string\""
        expEx (ParseError _ _) = True
        expEx _                = False
    rh <- submit client g Nothing
    nextResult rh `shouldThrow` expEx
    nextResult rh `shouldThrow` expEx
  specify "multiple threads reading on a single ResultHandle" $ \(host, port) -> do
    let opt = defOptions { batchSize = Just 3
                         }
        g = G.list $ map fromInteger [1..50] :: Greskell [Int]
    forClient' opt host port $ \client -> do
      rh <- submit client g Nothing
      mgot <- mapConcurrently (const $ nextResult rh) ([1..52] :: [Int])
      nextResult rh `shouldReturn` Nothing
      (length $ filter isNothing mgot) `shouldBe` 2
      let got = map fromJust $ filter isJust mgot
      got `shouldMatchList` [1..50]
  specify "Gremlin script throwing exception" $ withClient $ \client -> do
    let g :: Greskell Int
        g = G.unsafeGreskell "throw new Exception(\"BOOM\")"
        expEx (ResponseError res) = (Res.code $ Res.status res) == Res.ScriptEvaluationError
        expEx _                   = False
    rh <- submit client g Nothing
    nextResult rh `shouldThrow` expEx
    nextResult rh `shouldThrow` expEx


bad_server_spec :: SpecWith Port
bad_server_spec = do
  specify "error ResponseMessage" $ \port -> do
    let server = wsServer port $ \wsconn -> do
          req <- receiveRequest wsconn
          WS.sendBinaryData wsconn $ simpleRawResponse (requestId req) 500 "null"
        expEx (ResponseError res) = (Res.code $ Res.status res) == Res.ServerError
        expEx _                   = False
    withAsync server $ \_ -> do
      waitForServer
      forClient "localhost" port $ \client -> do
        let g = 100 :: Greskell Int
        rh <- submit client g Nothing
        nextResult rh `shouldThrow` expEx
        nextResult rh `shouldThrow` expEx
        nextResult rh `shouldThrow` expEx
  specify "response timeout" $ \port -> do
    let server = wsServer port $ \wsconn -> do
          req <- receiveRequest wsconn
          WS.sendBinaryData wsconn $ simpleRawResponse (requestId req) 206 "[99]"
          threadDelay 10000000
        opt = defOptions { connectionSettings = sett
                         }
        sett = (connectionSettings defOptions)
               { responseTimeout = 1
               }
        expEx ResponseTimeout = True
        expEx _               = False
    withAsync server $ \_ -> do
      waitForServer
      forClient' opt "localhost" port $ \client -> do
        let g = G.list [99, 100, 101, 102] :: Greskell [Int]
        rh <- submit client g Nothing
        nextResult rh `shouldReturn` Just 99
        nextResult rh `shouldThrow` expEx
        nextResult rh `shouldThrow` expEx

