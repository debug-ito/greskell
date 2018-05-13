{-# LANGUAGE OverloadedStrings #-}
module ServerTest.Client (main,spec) where

import Control.Concurrent.Async (withAsync)
import Control.Exception.Safe (bracket)
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Network.WebSockets as WS
import Test.Hspec

import Data.Greskell.Greskell (Greskell)
import qualified Data.Greskell.Greskell as G
import Data.Greskell.GMap (GMap, GMapEntry, unGMapEntry)

import Data.Greskell.WebSocket.Client
  ( Host, Port, Client, Options,
    connectWith, close, submit,
    defOptions, batchSize,
    nextResult, slurpResults,
    SubmitException(ResponseError)
  )
import Data.Greskell.WebSocket.Request (RequestMessage(requestId))
import qualified Data.Greskell.WebSocket.Response as Res

import TestUtil.Env (withEnvForExtServer, withEnvForIntServer)
import TestUtil.MockServer
  ( wsServer, receiveRequest, simpleRawResponse, waitForServer
  )

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
  specify "eval (GMap Int String)" $ withClient $ \client -> do
    let g :: Greskell (GMap Int String)
        g = G.unsafeGreskell "[100: 'hoge', 200: 'foo', 300: 'bar']"
    rh <- submit client g Nothing
    got <- fmap (map unGMapEntry) $ slurpResults rh
    got `shouldMatchList` [(100, "hoge"), (200, "foo"), (300, "bar")]
  specify "eval (Maybe Int)" $ withClient $ \client -> do
    let g :: Greskell (Maybe Int)
        g = G.unsafeGreskell "100"
    rh <- submit client g Nothing
    slurpResults rh `shouldReturn` [Just 100]
  specify "multiple response messages" $ \(host, port) -> do
    let opt = defOptions { batchSize = Just 3
                         }
        g = G.list $ map fromInteger [1..31] :: Greskell [Int]
    forClient' opt host port $ \client -> do
      rh <- submit client g Nothing
      slurpResults rh `shouldReturn` [1..31]

bad_server_spec :: SpecWith Port
bad_server_spec = do
  specify "error ResponseMessage" $ \port -> do
    let server = wsServer port $ \wsconn -> do
          req <- receiveRequest wsconn
          WS.sendBinaryData wsconn $ simpleRawResponse (requestId req) 500 "null"
        expEx (ResponseError res) = (Res.code $ Res.status res) == Res.ServerError
        expEx _ = False
    withAsync server $ \_ -> do
      waitForServer
      forClient "localhost" port $ \client -> do
        let g = 100 :: Greskell Int
        rh <- submit client g Nothing
        nextResult rh `shouldThrow` expEx
        nextResult rh `shouldThrow` expEx
        nextResult rh `shouldThrow` expEx
    
