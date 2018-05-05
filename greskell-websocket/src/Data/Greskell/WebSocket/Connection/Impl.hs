{-# LANGUAGE DuplicateRecordFields #-}
-- |
-- Module: Data.Greskell.WebSocket.Connection.Impl
-- Description: internal implementation of Connection
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This is an internal module. It deliberately exports everything. The
-- upper module is responsible to make a proper export list.
module Data.Greskell.WebSocket.Connection.Impl where

import Control.Applicative ((<$>), (<|>), empty)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, Async, async, waitCatchSTM, waitAnySTM)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
  ( TBQueue, readTBQueue, newTBQueueIO, writeTBQueue, flushTBQueue,
    TQueue, writeTQueue, newTQueueIO, readTQueue,
    TVar, newTVarIO, readTVar, writeTVar,
    TMVar, tryPutTMVar, tryReadTMVar, putTMVar, newEmptyTMVarIO, readTMVar,
    STM, atomically, retry
  )
import Control.Exception.Safe
  ( Exception(toException), SomeException, withException, throw, try, finally
  )
import Control.Monad (when, void, forM_)
import Data.Aeson (Value)
import qualified Data.DList as DL
import Data.Monoid (mempty, (<>))
import Data.Typeable (Typeable)
import Data.UUID (UUID)
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashTable.IO as HT

import Data.Greskell.WebSocket.Codec (Codec(decodeWith, encodeWith), encodeBinaryWith)
import Data.Greskell.WebSocket.Connection.Settings (Settings)
import qualified Data.Greskell.WebSocket.Connection.Settings as Settings
import Data.Greskell.WebSocket.Connection.Type
  ( Connection(..), ConnectionState(..),
    ResPack, ReqID, ReqPack(..), RawRes,
    GeneralException(..)
  )
import Data.Greskell.WebSocket.Request
  ( RequestMessage(RequestMessage, requestId),
    Operation, makeRequestMessage
  )
import Data.Greskell.WebSocket.Response
  ( ResponseMessage(ResponseMessage, requestId, status),
    ResponseStatus(ResponseStatus, code),
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
connect :: Settings s -> Host -> Port -> IO (Connection s)
connect settings host port = do
  req_pool <- HT.new  -- Do not manipulate req_pool in this thread. It belongs to runWSConn thread.
  qreq <- newTBQueueIO qreq_size
  var_connect_result <- newEmptyTMVarIO
  var_conn_state <- newTVarIO ConnOpen
  ws_thread <- async $ runWSConn settings host port ws_path req_pool qreq var_connect_result var_conn_state
  eret <- atomically $ readTMVar var_connect_result
  case eret of
   Left e -> throw e
   Right () -> return $ Connection { connQReq = qreq,
                                     connState = var_conn_state,
                                     connWSThread = ws_thread,
                                     connCodec = codec
                                   }
  where
    codec = Settings.codec settings
    qreq_size = Settings.requestQueueSize settings
    ws_path = Settings.endpointPath settings

-- | Close the 'Connection'.
--
-- If there are pending requests in the 'Connection', 'close' function
-- blocks for them to complete or time out.
-- 
-- Calling 'close' on a 'Connection' already closed (or waiting to close) does nothing.
close :: Connection s -> IO ()
close conn = do
  need_wait <- atomically $ do
    cur_state <- readTVar $ connState conn
    case cur_state of
     ConnClosed -> return False
     ConnClosing -> return True
     ConnOpen -> do
       -- writeTBQueue (connQReq conn) $ ReqPackClose ---- close requestを飛ばさない方法をトライ
       writeTVar (connState conn) ConnClosing
       return True
  if need_wait then waitForClose else return ()
  where
    waitForClose = atomically $ do
      cur_state <- readTVar $ connState conn
      if cur_state == ConnClosed
        then return ()
        else retry
  
-- TODO: maybe we need some more clean-up operations...
-- TODO: specify and test close behavior.

type Path = String

-- | A thread taking care of a WS connection.
runWSConn :: Settings s
          -> Host -> Port -> Path
          -> ReqPool s -> TBQueue (ReqPack s)
          -> TMVar (Either SomeException ()) -> TVar ConnectionState
          -> IO ()
runWSConn settings host port path req_pool qreq var_connect_result var_conn_state =
  (doConnect `withException` reportFatalEx) `finally` finalize
  where
    doConnect = WS.runClient host port path $ \wsconn -> do
      is_success <- checkAndReportConnectSuccess
      if not is_success
        then return () -- result is already reported at var_connect_result
        else setupMux wsconn
    setupMux wsconn = do
      qres <- newTQueueIO
      withAsync (runRxLoop wsconn qres) $ \rx_thread -> 
        runMuxLoop wsconn req_pool settings qreq qres (readTVar var_conn_state) rx_thread
    checkAndReportConnectSuccess = atomically $ do
      mret <- tryReadTMVar var_connect_result
      case mret of
       -- usually, mret should be Nothing.
       Nothing -> do
         putTMVar var_connect_result $ Right ()
         return True
       Just (Right _) -> return True
       Just (Left _) -> return False
    reportFatalEx :: SomeException -> IO ()
    reportFatalEx cause = do
      reportToConnectCaller cause
      reportToReqPool req_pool cause
      reportToQReq qreq cause
    reportToConnectCaller cause = void $ atomically $ tryPutTMVar var_connect_result $ Left cause
    finalize = do
      cleanupReqPool req_pool
      atomically $ writeTVar var_conn_state ConnClosed

reportToReqPool :: ReqPool s -> SomeException -> IO ()
reportToReqPool req_pool cause = HT.mapM_ forEntry req_pool
  where
    forEntry (_, entry) = atomically $ writeTQueue (rpeOutput entry) $ Left cause

reportToQReq :: TBQueue (ReqPack s) -> SomeException -> IO ()
reportToQReq qreq cause = atomically $ do
  reqpacks <- flushTBQueue qreq
  forM_ reqpacks reportToReqPack
  where
    reportToReqPack reqp = writeTQueue (reqOutput reqp) $ Left cause

-- | An exception related to a specific request.
data RequestException =
    AlreadyClosed
    -- ^ The connection is already closed before sending the request.
  | ServerClosed
    -- ^ The server closed the connection before sending response for
    -- this request
  | DuplicateRequestId UUID
    -- ^ The requestId (kept in this object) is already pending in the
    -- connection.
  | ResponseTimeout
    -- ^ The server fails to send ResponseMessages within
    -- 'Settings.responseTimeout'.
  deriving (Show,Eq,Typeable)

instance Exception RequestException

data ReqPoolEntry s =
  ReqPoolEntry
  { rpeReqId :: !ReqID,
    rpeOutput :: !(TQueue (ResPack s)),
    rpeTimer :: !(Async ReqID)
    -- ^ timer thread to time out response.
  }

-- | (requestId of pending request) --> (objects related to that pending request)
type ReqPool s = HT.BasicHashTable ReqID (ReqPoolEntry s)

-- | Multiplexed event object
data MuxEvent s = EvReq (ReqPack s)
                | EvRes RawRes
                | EvActiveClose
                | EvRxFinish
                | EvRxError SomeException
                | EvResponseTimeout ReqID

-- | HashTable's mutateIO is available since 1.2.3.0
tryInsertToReqPool :: ReqPool s
                   -> ReqID
                   -> IO (ReqPoolEntry s) -- ^ action to create the new entry.
                   -> IO Bool -- ^ 'True' if insertion is successful.
tryInsertToReqPool req_pool rid makeEntry = do
  mexist_entry <- HT.lookup req_pool rid
  case mexist_entry of
   Just _ -> return False
   Nothing -> do
     new_entry <- makeEntry
     HT.insert req_pool rid new_entry
     return True

cleanupReqPoolEntry :: ReqPoolEntry s -> IO ()
cleanupReqPoolEntry entry = Async.cancel $ rpeTimer entry

removeReqPoolEntry :: ReqPool s -> ReqPoolEntry s -> IO ()
removeReqPoolEntry req_pool entry = do
  cleanupReqPoolEntry entry
  HT.delete req_pool $ rpeReqId entry

cleanupReqPool :: ReqPool s -> IO ()
cleanupReqPool req_pool = HT.mapM_ forEntry req_pool
  where
    forEntry (_, entry) = cleanupReqPoolEntry entry

getAllResponseTimers :: ReqPool s -> IO [Async ReqID]
getAllResponseTimers req_pool = (fmap . fmap) toTimer $ HT.toList req_pool
  where
    toTimer (_, entry) = rpeTimer entry

-- | Multiplexer loop.
runMuxLoop :: WS.Connection -> ReqPool s -> Settings s
           -> TBQueue (ReqPack s) -> TQueue RawRes -> STM ConnectionState
           -> Async ()
           -> IO ()
runMuxLoop wsconn req_pool settings qreq qres readConnState rx_thread = loop
  where
    codec = Settings.codec settings
    loop = do
      res_timers <- getAllResponseTimers req_pool
      event <- atomically $ getEventSTM res_timers
      case event of
       EvReq req -> handleReq req >> loop
       EvRes res -> handleRes res >> loop
       EvActiveClose -> return ()
       EvRxFinish -> handleRxFinish
       EvRxError e -> throw e
       EvResponseTimeout rid -> handleResponseTimeout rid >> loop
    getEventSTM res_timers = getRequest
                             <|> (EvRes <$> readTQueue qres)
                             <|> makeEvActiveClose
                             <|> (rxResultToEvent <$> waitCatchSTM rx_thread)
                             <|> (timeoutToEvent <$> waitAnySTM res_timers)
        where
          max_concurrency = Settings.concurrency settings
          cur_concurrency = length res_timers
          getRequest = if cur_concurrency < max_concurrency
                       then EvReq <$> readTBQueue qreq
                       else empty
          rxResultToEvent (Right ()) = EvRxFinish
          rxResultToEvent (Left e) = EvRxError e
          timeoutToEvent (_, rid) = EvResponseTimeout rid
          makeEvActiveClose = do
            if cur_concurrency > 0
              then empty
              else do
                conn_state <- readConnState
                if conn_state == ConnOpen then empty else return EvActiveClose
    handleReq req = do
      insert_ok <- tryInsertToReqPool req_pool rid makeNewEntry
      if insert_ok
        then WS.sendBinaryData wsconn $ reqData req
        else reportError
        where
          rid = reqId req
          qout = reqOutput req
          makeNewEntry = do
            timer_thread <- runTimer (Settings.responseTimeout settings) rid
            return $ ReqPoolEntry { rpeReqId = rid,
                                    rpeOutput = qout,
                                    rpeTimer = timer_thread
                                  }
          reportError =
            atomically $ writeTQueue qout $ Left $ toException $ DuplicateRequestId rid
    handleRes res = case decodeWith codec res of
      Left err -> Settings.onGeneralException settings $ ResponseParseFailure err
      Right res_msg -> handleResMsg res_msg
    handleResMsg res_msg@(ResponseMessage { requestId = rid }) = do
      m_entry <- HT.lookup req_pool rid
      case m_entry of
       Nothing -> Settings.onGeneralException settings $ UnexpectedRequestId rid
       Just entry -> do
         when (isTerminatingResponse res_msg) $ do
           removeReqPoolEntry req_pool entry
         atomically $ writeTQueue (rpeOutput entry) $ Right res_msg
    handleRxFinish = do
      -- RxFinish is an error for pending requests. If there is no
      -- pending requests, it's totally normal.
      let ex = toException ServerClosed
      reportToReqPool req_pool ex
      reportToQReq qreq ex
    handleResponseTimeout rid = do
      mentry <- HT.lookup req_pool rid
      case mentry of
       Nothing -> return () -- this case may happen if the response came just before the time-out, I think.
       Just entry -> do
         atomically $ writeTQueue (rpeOutput entry) $ Left $ toException $ ResponseTimeout
         removeReqPoolEntry req_pool entry


-- | Receiver thread. It keeps receiving data from WS until the
-- connection finishes cleanly. Basically every exception is raised to
-- the caller.
runRxLoop :: WS.Connection -> TQueue RawRes -> IO ()
runRxLoop wsconn qres = loop
  where
    loop = do
      mgot <- tryReceive
      case mgot of
        Nothing -> return ()
        Just got -> do
          atomically $ writeTQueue qres got
          loop
    tryReceive = toMaybe =<< (try $ WS.receiveData wsconn)
      where
        toMaybe (Right d) = return $ Just d
        toMaybe (Left e@(WS.CloseRequest close_status _)) = do
          if close_status == 1000 -- "normal closure". See sec. 7.4, RFC 6455.
            then return Nothing
            else throw e
        -- We allow the server to close the connection without sending Close request message.
        toMaybe (Left WS.ConnectionClosed) = return Nothing
        toMaybe (Left e) = throw e

runTimer :: Int -> ReqID -> IO (Async ReqID)
runTimer wait_sec rid = async $ do
  threadDelay $ wait_sec * 1000000
  return rid
  

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
sendRequest' conn req_msg = do
  qout <- newTQueueIO
  is_open <- getConnectionOpen
  if is_open
    then sendReqPack qout
    else reportAlreadyClosed qout
  makeResHandle qout
  where
    codec = connCodec conn
    qreq = connQReq conn
    var_conn_state = connState conn
    rid = requestId (req_msg :: RequestMessage)
    getConnectionOpen = fmap (== ConnOpen) $ atomically $ readTVar var_conn_state
    sendReqPack qout = do
      atomically $ writeTBQueue qreq reqpack
      where
        reqpack = ReqPack
                  { reqData = encodeBinaryWith codec req_msg,
                    reqId = rid,
                    reqOutput = qout
                  }
    makeResHandle qout = do
      var_term <- newTVarIO False
      return $ ResponseHandle
               { rhGetResponse = readTQueue qout,
                 rhTerminated = var_term
               }
    reportAlreadyClosed qout = do
      atomically $ writeTQueue qout $ Left $ toException $ AlreadyClosed
    

-- | Get a 'ResponseMessage' from 'ResponseHandle'. If you have
-- already got all responses, it returns 'Nothing'. This function may
-- block for a new 'ResponseMessage' to come.
--
-- On error, it may throw all sorts of exceptions including
-- 'RequestException'.
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
      when (isTerminatingResponse res) $ do
        writeTVar (rhTerminated rh) True

isTerminatingResponse :: ResponseMessage s -> Bool
isTerminatingResponse (ResponseMessage { status = (ResponseStatus { code = c }) }) =
  isTerminating c

-- | Get all remaining 'ResponseMessage's from 'ResponseHandle'.
slurpResponses :: ResponseHandle s -> IO [ResponseMessage s]
slurpResponses h = fmap DL.toList $ go mempty
  where
    go got = do
      mres <- getResponse h
      case mres of
       Nothing -> return got
       Just res -> go (got <> DL.singleton res)
