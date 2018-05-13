{-# LANGUAGE OverloadedStrings #-}
module TestUtil.MockServer
       ( wsServer,
         parseRequest,
         receiveRequest,
         simpleRawResponse,
         waitForServer
       ) where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (throwString)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Network.WebSockets as WS

import Data.Greskell.WebSocket.Codec (decodeBinary)
import Data.Greskell.WebSocket.Request (RequestMessage)


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

waitForServer :: IO ()
waitForServer = threadDelay 100000
