{-# LANGUAGE DuplicateRecordFields, TypeFamilies #-}
-- |
-- Module: Network.Greskell.WebSocket.Client.Impl
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __Internal module__. It's like
-- "Network.Greskell.WebSocket.Connection.Impl".
module Network.Greskell.WebSocket.Client.Impl
       where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.STM
  ( STM, atomically,
    TVar, newTVarIO, readTVar, writeTVar
  )
import Control.Exception.Safe
  ( throw, Typeable, Exception, SomeException, catch
  )
import Data.Aeson (Object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (Parser)
import Data.Greskell.Greskell (ToGreskell(GreskellReturn), toGremlin)
import Data.Greskell.GraphSON (GraphSON, gsonValue, GValue, FromGraphSON(..), parseEither)
import Data.Greskell.AsIterator (AsIterator(IteratorItem))
import Data.Monoid (mempty)
import Data.Vector (Vector, (!))
import Data.Text (Text)
import Data.Traversable (traverse)

import Network.Greskell.WebSocket.Client.Options (Options)
import qualified Network.Greskell.WebSocket.Client.Options as Opt
import Network.Greskell.WebSocket.Connection
  ( Host, Port, Connection, ResponseHandle
  )
import qualified Network.Greskell.WebSocket.Connection as Conn
import qualified Network.Greskell.WebSocket.Request.Standard as ReqStd
import Network.Greskell.WebSocket.Response (ResponseCode, ResponseMessage)
import qualified Network.Greskell.WebSocket.Response as Res
import Network.Greskell.WebSocket.Util (slurp)


-- | A client that establishes a connection to the Gremlin Server. You
-- can send Gremlin expression for evaluation by 'submit' function.
data Client =
  Client
  { clientOpts :: Options,
    clientConn :: Connection GValue
  }

-- | Create a 'Client' to a Gremlin Server, with the default 'Options'.
connect :: Host -> Port -> IO Client
connect = connectWith Opt.defOptions

-- | Create a 'Client' to a Gremlin Server.
connectWith :: Options -> Host -> Port -> IO Client
connectWith opts host port = do
  conn <- Conn.connect (Opt.connectionSettings opts) host port
  return $ Client { clientOpts = opts,
                    clientConn = conn
                  }

-- | Close the connection to the server and release other resources of
-- the 'Client'.
close :: Client -> IO ()
close c = Conn.close $ clientConn c

data HandleState =
    HandleOpen
  | HandleClose
  | HandleError SomeException
  deriving (Show)

-- | A handle to receive the result of evaluation of a Gremlin script
-- from the server.
data ResultHandle v =
  ResultHandle
  { rhResHandle :: ResponseHandle GValue,
    rhParseGValue :: GValue -> Either String (Vector v),
    rhResultCache :: TVar (Vector v),
    rhNextResultIndex :: TVar Int,
    rhState :: TVar HandleState
  }

submitBase :: FromGraphSON r => Client -> Text -> Maybe Object -> IO (ResultHandle r)
submitBase client script bindings = do
  rh <- Conn.sendRequest conn op
  (cache, index, state) <- (,,) <$> newTVarIO mempty <*> newTVarIO 0 <*> newTVarIO HandleOpen
  return $ ResultHandle { rhResHandle = rh,
                          rhParseGValue = parseEither,
                          rhResultCache = cache,
                          rhNextResultIndex = index,
                          rhState = state
                        }
  where
    conn = clientConn client
    opts = clientOpts client
    op = ReqStd.OpEval { ReqStd.batchSize = Opt.batchSize opts,
                         ReqStd.gremlin = script,
                         ReqStd.bindings = bindings,
                         ReqStd.language = Opt.language opts,
                         ReqStd.aliases = Opt.aliases opts,
                         ReqStd.scriptEvaluationTimeout = Opt.scriptEvaluationTimeout opts
                       }

-- | Submit a Gremlin script to the server. You can get its results by
-- 'ResultHandle'. The result type @v@ is determined by the script
-- type @g@.
-- 
-- Usually this function does not throw any exception. Exceptions
-- about sending requests are reported when you operate on
-- 'ResultHandle'.
submit :: (ToGreskell g, r ~ GreskellReturn g, AsIterator r, v ~ IteratorItem r, FromGraphSON v)
       => Client
       -> g -- ^ Gremlin script
       -> Maybe Object -- ^ bindings
       -> IO (ResultHandle v)
submit client greskell bindings = submitBase client (toGremlin greskell) bindings

-- | Less type-safe version of 'submit'.
submitRaw :: Client
          -> Text -- ^ Gremlin script
          -> Maybe Object -- ^ bindings
          -> IO (ResultHandle GValue)
submitRaw = submitBase

-- | Exception about 'submit' operation and getting its result.
data SubmitException =
    ResponseError (ResponseMessage GValue)
    -- ^ The server returns a 'ResponseMessage' with error 'ResponseCode'.
  | ParseError (ResponseMessage GValue) String
    -- ^ The client fails to parse the \"data\" field of the
    -- 'ResponseMessage'. The error message is kept in the 'String'
    -- field.
  deriving (Show,Typeable)

instance Exception SubmitException


-- | Get the next value from the 'ResultHandle'. If you have got all
-- values, it returns 'Nothing'.  This function may block for a new
-- response to come.
--
-- On error, it may throw all sorts of exceptions including
-- 'SubmitException' and 'Conn.RequestException'. For example, if the
-- submitted Gremlin script throws an exception, 'nextResult' throws
-- 'ResponseError' with 'ResponseCode' of 'Res.ScriptEvaluationError'.
nextResult :: ResultHandle v -> IO (Maybe v)
nextResult = atomically . nextResultSTM

-- | 'STM' version of 'nextResult'.
nextResultSTM :: ResultHandle v -> STM (Maybe v)
nextResultSTM rh = do
  cur_state <- readTVar $ rhState rh
  case cur_state of
   HandleError err -> throw err
   HandleClose -> return Nothing
   HandleOpen -> doNext `withExceptionSTM` gotoError
  where
    doNext = do
      mret <- getNext
      case mret of
       Nothing -> writeTVar (rhState rh) HandleClose
       _ -> return ()
      return mret
    getNext = do
      mnext <- getNextCachedResult rh
      case mnext of
       Just v -> return $ Just v
       Nothing -> loadResponse rh
    -- 'withException' function is for MonadMask and STM is not a
    -- MonadMask. So we use catch-and-rethrow by hand.
    withExceptionSTM main finish =
      main `catch` (\ex -> finish ex >> throw ex)
    gotoError ex = writeTVar (rhState rh) $ HandleError ex
    

getNextCachedResult :: ResultHandle v -> STM (Maybe v)
getNextCachedResult rh = do
  (cache, index) <- (,) <$> (readTVar $ rhResultCache rh) <*> (readTVar $ rhNextResultIndex rh)
  if index < length cache
    then fromCache cache index
    else return Nothing
  where
    fromCache cache index = do
      writeTVar (rhNextResultIndex rh) $ index + 1
      return $ Just (cache ! index)

loadResponse :: ResultHandle v -> STM (Maybe v)
loadResponse rh = parseResponse =<< (Conn.nextResponseSTM $ rhResHandle rh)
  where
    parseResponse Nothing = return Nothing
    parseResponse (Just res) = 
      case Res.code $ Res.status res of
       Res.Success -> parseData res
       Res.NoContent -> return Nothing
       Res.PartialContent -> parseData res
       _ -> throw $ ResponseError res -- TODO: handle Authenticate code
    parseData res =
      case rhParseGValue rh $ Res.resultData $ Res.result res of
       Left err -> throw $ ParseError res err
       Right parsed -> do
         writeTVar (rhResultCache rh) parsed
         if length parsed == 0
           then do
             writeTVar (rhNextResultIndex rh) 0
             return Nothing
           else do
             writeTVar (rhNextResultIndex rh) 1
             return $ Just (parsed ! 0)

-- | Get all remaining results from 'ResultHandle'.
slurpResults :: ResultHandle v -> IO [v]
slurpResults h = slurp $ nextResult h
      
