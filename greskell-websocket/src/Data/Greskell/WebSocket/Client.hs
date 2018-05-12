{-# LANGUAGE DuplicateRecordFields, TypeFamilies #-}
-- |
-- Module: Data.Greskell.WebSocket.Client
-- Description: High-level interface to Gremlin Server
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.WebSocket.Client
       ( Client,
         connect,
         close,
         submit,
         submitRaw,
         nextResult,
         nextResultSTM,
         ResultHandle,
         SubmitException(..)
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.STM
  ( STM, atomically,
    TVar, newTVarIO, readTVar, writeTVar
  )
import Control.Exception.Safe (throw, Typeable, Exception)
import Data.Aeson (Object, Value, FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (Parser)
import Data.Greskell.Greskell (ToGreskell(GreskellReturn), toGremlin)
import Data.Greskell.GraphSON (GraphSON, gsonValue)
import Data.Greskell.IteratorItem (IteratorItem)
import Data.Monoid (mempty)
import Data.Vector (Vector, (!))
import Data.Text (Text)
import Data.Traversable (traverse)

import Data.Greskell.WebSocket.Client.Options (Options(..))
import Data.Greskell.WebSocket.Connection
  ( Host, Port, Connection, ResponseHandle
  )
import qualified Data.Greskell.WebSocket.Connection as Conn
import qualified Data.Greskell.WebSocket.Request.Standard as ReqStd
import Data.Greskell.WebSocket.Response (ResponseCode, ResponseMessage)
import qualified Data.Greskell.WebSocket.Response as Res


-- | A client that establishes a connection to the Gremlin Server. You
-- can send Gremlin expression for evaluation by 'submit' function.
data Client =
  Client
  { clientOpts :: Options,
    clientConn :: Connection Value
  }

-- | Create a 'Client' to a Gremlin Server.
connect :: Options -> Host -> Port -> IO Client
connect opts host port = do
  conn <- Conn.connect (connectionSettings opts) host port
  return $ Client { clientOpts = opts,
                    clientConn = conn
                  }

-- | Close the connection to the server and release other resources of
-- the 'Client'.
close :: Client -> IO ()
close c = Conn.close $ clientConn c

-- | A handle to receive the result of evaluation of a Gremlin script
-- from the server.
data ResultHandle v =
  ResultHandle
  { rhResHandle :: ResponseHandle Value,
    rhParseValue :: Value -> Either String (Vector v),
    rhResultCache :: TVar (Vector v),
    rhNextResultIndex :: TVar Int
  }

unwrapGraphSONResult :: GraphSON (Vector (GraphSON v)) -> Vector v
unwrapGraphSONResult = fmap gsonValue . gsonValue

submitBase :: FromJSON r => Client -> Text -> Maybe Object -> IO (ResultHandle r)
submitBase client script bindings = do
  rh <- Conn.sendRequest conn op
  (cache, index) <- (,) <$> newTVarIO mempty <*> newTVarIO 0
  return $ ResultHandle { rhResHandle = rh,
                          rhParseValue = parser,
                          rhResultCache = cache,
                          rhNextResultIndex = index
                        }
  where
    conn = clientConn client
    -- TODO: make these fields configurable
    op = ReqStd.OpEval { ReqStd.batchSize = Nothing,
                         ReqStd.gremlin = script,
                         ReqStd.bindings = bindings,
                         ReqStd.language = Nothing,
                         ReqStd.aliases = Nothing,
                         ReqStd.scriptEvaluationTimeout = Nothing
                       }
    parser val = resultToEither $ fmap unwrapGraphSONResult $ Aeson.fromJSON val
    resultToEither (Aeson.Error s) = Left s
    resultToEither (Aeson.Success a) = Right a

submit :: (ToGreskell g, r ~ IteratorItem (GreskellReturn g), FromJSON r)
       => Client
       -> g -- ^ Gresmlin script
       -> Maybe Object -- ^ bindings
       -> IO (ResultHandle r)
submit client greskell bindings = submitBase client (toGremlin greskell) bindings

-- | Less type-safe version of 'submit'.
submitRaw :: Client
          -> Text -- ^ Gremlin script
          -> Maybe Object -- ^ bindings
          -> IO (ResultHandle Value)
submitRaw = submitBase

-- | Exception about 'submit' operation and getting its result.
data SubmitException =
    ResponseError (ResponseMessage Value)
    -- ^ The server returns a 'ResponseMessage' with error 'ResponseCode'.
  | ParseError (ResponseMessage Value) String
    -- ^ The client fails to parse the \"data\" field of the
    -- 'ResponseMessage'. The error message is kept in the 'String'
    -- field.
  deriving (Show,Typeable)

instance Exception SubmitException


-- | Get the next value from the 'ResultHandle'. If you have got all
-- values, it returns 'Nothing'.
nextResult :: ResultHandle v -> IO (Maybe v)
nextResult = atomically . nextResultSTM

-- | 'STM' version of 'nextResult'.
nextResultSTM :: ResultHandle v -> STM (Maybe v)
nextResultSTM rh = do
  mnext <- getNextCachedResult rh
  case mnext of
   Just v -> return $ Just v
   Nothing -> loadResponse rh

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

-- TODO: maybe we should cache the exception/Nothing returned from nextResponseSTM
loadResponse :: ResultHandle v -> STM (Maybe v)
loadResponse rh = parseResponse =<< (Conn.nextResponseSTM $ rhResHandle rh)
  where
    parseResponse Nothing = return Nothing
    parseResponse (Just res) = 
      case Res.code $ Res.status res of
       Res.Success -> parseData res
       Res.NoContent -> return Nothing
       Res.PartialContent -> parseData res
       error_code -> throw $ ResponseError res -- TODO: handle Authenticate code
    parseData res =
      case rhParseValue rh $ Res.resultData $ Res.result res of
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
      

-- nextResponseと同様、throwしたあと連続してnextResultしても引き続きthrowさせないといけない。

-- そもそも1つのResponseHandleまたはResultHandleを複数スレッドからnextするテスト書かないといけなくね。
