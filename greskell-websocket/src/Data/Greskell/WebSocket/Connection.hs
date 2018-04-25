-- |
-- Module: Data.Greskell.WebSocket.Connection
-- Description: WebSocket Connection to Gremlin Server
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.WebSocket.Connection
       ( -- * Make a Connection
         connect,
         close,
         Connection,
         Host,
         Port,
         Path,
         -- * Make a request
         sendRequest,
         ResponseHandle,
         getResponse
       ) where

import Control.Applicative ((<$>), (<|>))
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
  ( TBQueue, readTBQueue,
    TQueue, writeTQueue, newTQueueIO, readTQueue,
    atomically
  )
import Data.UUID (UUID)
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Lazy as BSL

import Data.Greskell.WebSocket.Codec (Codec(decodeWith, encodeWith))
import Data.Greskell.WebSocket.Request (RequestMessage)
import Data.Greskell.WebSocket.Response (ResponseMessage)

-- | A WebSocket connection to a Gremlin Server.
data Connection

-- | Host name or an IP address.
type Host = String

-- | TCP port number.
type Port = Int

-- | WebSocket end-point path
type Path = String

-- | Make a 'Connection' to a Gremlin Server.
--
-- TODO: define exception spec.
connect :: Host -> Port -> Path -> IO Connection
connect = undefined

-- | Close the 'Connection'.
close :: Connection -> IO ()
close = undefined


type RawReq = BSL.ByteString

type RawRes = BSL.ByteString

type ReqID = UUID

data ReqPack = ReqPack
               { reqData :: RawReq,
                 reqId :: ReqID,
                 reqOutput :: TQueue RawRes
               }

startWS :: Host -> Port -> Path -> TBQueue ReqPack -> IO ()
startWS host port path qreq = WS.runClient host port path $ \wsconn -> do
  qres <- newTQueueIO
  withAsync (runRxLoop wsconn qres) $ \_ -> 
    runMuxLoop (WSContext wsconn) qreq qres 


data WSContext s = WSContext
                   { wscConn :: WS.Connection
                     -- TODO: Map requestId (TQueue BS)的なやつを入れる。IORefかな？ mutable特化のものが望ましい。
                   }

-- | Multiplexer thread.
runMuxLoop :: WSContext s -> TBQueue ReqPack -> TQueue RawRes -> IO ()
runMuxLoop (WSContext { wscConn = wsconn }) qreq qres = loop
  where
    loop = do
      event <- atomically $ (Left <$> readTBQueue qreq) <|> (Right <$> readTQueue qres)
      case event of
       Left req -> handleReq req
       Right res -> handleRes res
      loop
    handleReq req = do
      -- TODO: handle new request. Who should create a new requestId?
      WS.sendBinaryData wsconn $ reqData req
    handleRes res = undefined -- TODO: handle response message

-- | Receiver thread.
runRxLoop :: WS.Connection -> TQueue RawRes -> IO ()
runRxLoop wsconn qres = loop
  where
    loop = do
      got <- WS.receiveData wsconn -- TODO: handle exception
      atomically $ writeTQueue qres got
      loop
  

-- | A handle associated in a 'Connection' for a pair of request and
-- response. You can retrieve 'ResponseMessage's from this object.
data ResponseHandle s

-- | Send a 'RequestMessage' to the server.
--
-- TODO: define exception spec.
sendRequest :: Codec s -> Connection -> RequestMessage -> IO (ResponseHandle s)
sendRequest = undefined

-- | Get a 'ResponseMessage' from 'ResponseHandle'.
--
-- TODO: define exception spec.
getResponse :: ResponseHandle s -> IO (ResponseMessage s)
getResponse = undefined
