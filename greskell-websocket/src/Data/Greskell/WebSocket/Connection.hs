{-# LANGUAGE DuplicateRecordFields #-}
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
         sendRequest',
         ResponseHandle,
         getResponse,
         slurpResponses
       ) where

import Control.Applicative ((<$>), (<|>))
import Control.Concurrent.Async (withAsync, Async, async)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
  ( TBQueue, readTBQueue, newTBQueueIO, writeTBQueue,
    TQueue, writeTQueue, newTQueueIO, readTQueue,
    atomically
  )
import Data.Aeson (Value)
import qualified Data.DList as DL
import Data.Monoid (mempty, (<>))
import Data.UUID (UUID)
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashTable.IO as HT

import Data.Greskell.WebSocket.Codec (Codec(decodeWith, encodeWith), encodeBinaryWith)
import Data.Greskell.WebSocket.Request
  ( RequestMessage(RequestMessage, requestId),
    Operation, makeRequestMessage
  )
import Data.Greskell.WebSocket.Response (ResponseMessage(ResponseMessage, requestId))

-- | Host name or an IP address.
type Host = String

-- | TCP port number.
type Port = Int

-- | WebSocket end-point path
type Path = String

-- | Make a 'Connection' to a Gremlin Server.
--
-- TODO: define exception spec.
connect :: Codec s -> Host -> Port -> Path -> IO (Connection s)
connect codec host port path = do
  qreq <- newTBQueueIO qreq_size
  ws_thread <- async $ runWSConn codec host port path qreq
  return $ Connection { connQReq = qreq,
                        connWSThread = ws_thread,
                        connCodec = codec
                      }
  where
    qreq_size = 512 -- TODO: make it configurable

-- | Close the 'Connection'.
close :: Connection s -> IO ()
close (Connection { connWSThread = ws_async }) = Async.cancel ws_async
-- TODO: maybe we need some more clean-up operations...


type RawReq = BSL.ByteString

type RawRes = BSL.ByteString

type ReqID = UUID

-- | Package of request data and related stuff. It's passed from the
-- caller thread into WS handling thread.
data ReqPack s = ReqPack
                 { reqData :: !RawReq,
                   reqId :: !ReqID,
                   reqOutput :: !(TQueue (ResponseMessage s))
                 }

-- | A WebSocket connection to a Gremlin Server.
data Connection s =
  Connection
  { connQReq :: !(TBQueue (ReqPack s)),
    connWSThread :: !(Async ()),
    connCodec :: !(Codec s)
  }


-- | A thread taking care of a WS connection.
runWSConn :: Codec s -> Host -> Port -> Path -> TBQueue (ReqPack s) -> IO ()
runWSConn codec host port path qreq = WS.runClient host port path $ \wsconn -> do -- TODO: handle exception
  qres <- newTQueueIO
  req_pool <- HT.new
  withAsync (runRxLoop wsconn qres) $ \_ -> 
    runMuxLoop wsconn req_pool codec qreq qres 

-- | (requestId of pending request) --> (output channel of the corresponding responses)
type ReqPool s = HT.BasicHashTable ReqID (TQueue (ResponseMessage s))

-- | Multiplexer loop.
runMuxLoop :: WS.Connection -> ReqPool s -> Codec s -> TBQueue (ReqPack s) -> TQueue RawRes -> IO ()
runMuxLoop wsconn req_pool codec qreq qres = loop
  where
    loop = do
      event <- atomically $ (Left <$> readTBQueue qreq) <|> (Right <$> readTQueue qres)
      case event of
       Left req -> handleReq req
       Right res -> handleRes res
      loop
    handleReq req = do
      HT.insert req_pool (reqId req) (reqOutput req) -- TODO: if the reqId already exists, it's error.
      WS.sendBinaryData wsconn $ reqData req
    handleRes res = case decodeWith codec res of -- TODO: perhaps we have to decode MIME type packaging
      Left err -> undefined -- TODO: handle parse error
      Right res_msg -> handleResMsg res_msg
    handleResMsg res_msg@(ResponseMessage { requestId = rid }) = do
      m_qout <- HT.lookup req_pool rid
      case m_qout of
       Nothing -> undefined -- TODO: handle unknown requestId case.
       Just qout -> atomically $ writeTQueue qout res_msg


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
newtype ResponseHandle s = ResponseHandle (TQueue (ResponseMessage s))

-- | Make a 'RequestMessage' from an 'Operation' and send it.
sendRequest :: Operation o => Connection s -> o -> IO (ResponseHandle s)
sendRequest conn o = sendRequest' conn =<< makeRequestMessage o

-- | Send a 'RequestMessage' to the server.
--
-- TODO: define exception spec.
sendRequest' :: Connection s -> RequestMessage -> IO (ResponseHandle s)
sendRequest' (Connection { connCodec = codec, connQReq = qreq }) req_msg@(RequestMessage { requestId = rid }) = do
  qout <- newTQueueIO
  atomically $ writeTBQueue qreq $ ReqPack { reqData = encodeBinaryWith codec req_msg,
                                             reqId = rid,
                                             reqOutput = qout
                                           }
  return $ ResponseHandle qout

-- | Get a 'ResponseMessage' from 'ResponseHandle'. If you have
-- already got all responses, it returns 'Nothing'.
--
-- TODO: define exception spec.
getResponse :: ResponseHandle s -> IO (Maybe (ResponseMessage s))
getResponse (ResponseHandle qout) = fmap Just $ atomically $ readTQueue qout
-- TODO: inspect the received message, and clean up the ReqPack from ReqPool.

-- | Get all remaining 'ResponseMessage's from 'ResponseHandle'.
slurpResponses :: ResponseHandle s -> IO [ResponseMessage s]
slurpResponses h = fmap DL.toList $ go mempty
  where
    go got = do
      mres <- getResponse h
      case mres of
       Nothing -> return got
       Just res -> go (got <> DL.singleton res)
