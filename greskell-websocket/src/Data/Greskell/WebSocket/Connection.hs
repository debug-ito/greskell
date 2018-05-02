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
         -- * Make a request
         sendRequest,
         sendRequest',
         ResponseHandle,
         getResponse,
         slurpResponses,
         -- * Exceptions
         RequestException(..)
       ) where

import Control.Applicative ((<$>), (<|>))
import Control.Concurrent.Async (withAsync, Async, async)
import qualified Control.Concurrent.Async as Async
import Control.Monad (void)
import Control.Concurrent.STM
  ( TBQueue, readTBQueue, newTBQueueIO, writeTBQueue,
    TQueue, writeTQueue, newTQueueIO, readTQueue,
    atomically, STM,
    TVar, newTVarIO, readTVar, writeTVar,
    TMVar, tryPutTMVar, tryReadTMVar, putTMVar, newEmptyTMVarIO, readTMVar
  )
import Control.Exception.Safe
  ( Exception, SomeException, withException, throw,
    handleAny
  )
import Control.Monad (when)
import Data.Aeson (Value)
import qualified Data.DList as DL
import Data.Monoid (mempty, (<>))
import Data.Typeable (Typeable)
import Data.UUID (UUID)
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashTable.IO as HT

import Data.Greskell.WebSocket.Codec (Codec(decodeWith, encodeWith), encodeBinaryWith)
import Data.Greskell.WebSocket.Request
  ( RequestMessage(RequestMessage, requestId),
    Operation, makeRequestMessage
  )
import Data.Greskell.WebSocket.Response
  ( ResponseMessage(ResponseMessage, requestId, status),
    ResponseStatus(code),
    isTerminating
  )

-- | Host name or an IP address.
type Host = String

-- | TCP port number.
type Port = Int



-- | Make a 'Connection' to a Gremlin Server.
--
-- If it fails to connect to the specified server, it throws an
-- exception.
connect :: Codec s -> Host -> Port -> IO (Connection s)
connect codec host port = do
  qreq <- newTBQueueIO qreq_size
  var_connect_result <- newEmptyTMVarIO
  ws_thread <- async $ runWSConn codec host port ws_path qreq var_connect_result
  eret <- atomically $ readTMVar var_connect_result
  case eret of
   Left e -> throw e
   Right () -> return $ Connection { connQReq = qreq,
                                     connWSThread = ws_thread,
                                     connCodec = codec
                                   }
  where
    qreq_size = 512 -- TODO: make it configurable
    ws_path = "/gremlin" -- TODO: make it configurable

-- | Close the 'Connection'.
close :: Connection s -> IO ()
close (Connection { connWSThread = ws_async }) = Async.cancel ws_async
-- TODO: maybe we need some more clean-up operations...


type RawReq = BSL.ByteString

type RawRes = BSL.ByteString

type ReqID = UUID


-- | Package of Response data and related stuff.
type ResPack s = Either RequestException (ResponseMessage s)

-- | Package of request data and related stuff. It's passed from the
-- caller thread into WS handling thread.
data ReqPack s = ReqPack
                 { reqData :: !RawReq,
                   reqId :: !ReqID,
                   reqOutput :: !(TQueue (ResPack s))
                 }

-- | A WebSocket connection to a Gremlin Server.
data Connection s =
  Connection
  { connQReq :: !(TBQueue (ReqPack s)),
    connWSThread :: !(Async ()),
    connCodec :: !(Codec s)
  }

type Path = String

-- | A thread taking care of a WS connection.
runWSConn :: Codec s -> Host -> Port -> Path -> TBQueue (ReqPack s) -> TMVar (Either SomeException ()) -> IO ()
runWSConn codec host port path qreq var_connect_result = doConnect `withException` reportException
  where
    doConnect = WS.runClient host port path $ \wsconn -> do
      is_success <- isConnectSuccess
      if not is_success
        then return () -- result is already reported at var_connect_result
        else setupMux wsconn
    setupMux wsconn = do
      qres <- newTQueueIO
      req_pool <- HT.new
      withAsync (runRxLoop wsconn qres) $ \_ -> 
        runMuxLoop wsconn req_pool codec qreq qres 
    reportException :: SomeException -> IO ()
    reportException cause =
      -- TODO: exception can be thrown while runMuxLoop runs.
      void $ atomically $ tryPutTMVar var_connect_result $ Left cause
    isConnectSuccess = atomically $ do
      mret <- tryReadTMVar var_connect_result
      case mret of
       -- usually, mret should be Nothing.
       Nothing -> do
         putTMVar var_connect_result $ Right ()
         return True
       Just (Right _) -> return True
       Just (Left _) -> return False

-- | An exception related to a specific request.
data RequestException = SendException SomeException
                      deriving (Show,Typeable)

instance Exception RequestException

-- | (requestId of pending request) --> (output channel of the corresponding responses)
type ReqPool s = HT.BasicHashTable ReqID (TQueue (ResPack s))

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
      handleAny abortAllWith $ WS.sendBinaryData wsconn $ reqData req
    handleRes res = case decodeWith codec res of
      Left err -> undefined -- TODO: handle parse error
      Right res_msg -> handleResMsg res_msg
    handleResMsg res_msg@(ResponseMessage { requestId = rid }) = do
      m_qout <- HT.lookup req_pool rid
      case m_qout of
       Nothing -> undefined -- TODO: handle unknown requestId case.
       Just qout -> atomically $ writeTQueue qout $ Right res_msg
    abortPendingReq rid ex = do
      m_qout <- HT.lookup req_pool rid
      case m_qout of
       Nothing -> return () -- TODO: we might as well emit warning here.
       Just qout -> do
         HT.delete req_pool rid
         atomically $ writeTQueue qout $ Left ex
    abortAllWith ex = undefined -- TODO: abort everything! close the Connection entirely. what should we do?
       


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
data ResponseHandle s =
  ResponseHandle
  { rhGetResponse :: STM (ResPack s),
    rhTerminated :: TVar Bool
  }

instance Functor ResponseHandle where
  fmap f rh = rh { rhGetResponse = (fmap . fmap . fmap) f $ rhGetResponse rh }


-- | Make a 'RequestMessage' from an 'Operation' and send it.
--
-- Usually this function does not throw any exception. Exceptions
-- about sending requests are reported when you operate on
-- 'ResponseHandle'.
sendRequest :: Operation o => Connection s -> o -> IO (ResponseHandle s)
sendRequest conn o = sendRequest' conn =<< makeRequestMessage o

-- | Like 'sendRequest', but you can pass a 'RequestMessage' directly
-- to this function.
sendRequest' :: Connection s -> RequestMessage -> IO (ResponseHandle s)
sendRequest' (Connection { connCodec = codec, connQReq = qreq }) req_msg@(RequestMessage { requestId = rid }) = do
  qout <- newTQueueIO
  atomically $ writeTBQueue qreq $ ReqPack { reqData = encodeBinaryWith codec req_msg,
                                             reqId = rid,
                                             reqOutput = qout
                                           }
  var_term <- newTVarIO False
  let rhandle = ResponseHandle
                { rhGetResponse = readTQueue qout,
                  rhTerminated = var_term
                }
  return rhandle

-- | Get a 'ResponseMessage' from 'ResponseHandle'. If you have
-- already got all responses, it returns 'Nothing'.
--
-- TODO: define exception spec.
getResponse :: ResponseHandle s -> IO (Maybe (ResponseMessage s))
getResponse rh = atomically $ do
  termed <- readTVar $ rhTerminated rh
  if termed
    then return Nothing
    else readResponse
  where
    readResponse = do
      eres <- rhGetResponse rh
      case eres of
       Left ex -> throw ex
       Right res -> do
         updateTermed res
         return $ Just res
    updateTermed res =
      when (isTerminating $ code $ status res) $ do
        writeTVar (rhTerminated rh) True
        -- TODO: clean up the ReqPack and ReqPool.

-- | Get all remaining 'ResponseMessage's from 'ResponseHandle'.
slurpResponses :: ResponseHandle s -> IO [ResponseMessage s]
slurpResponses h = fmap DL.toList $ go mempty
  where
    go got = do
      mres <- getResponse h
      case mres of
       Nothing -> return got
       Just res -> go (got <> DL.singleton res)
