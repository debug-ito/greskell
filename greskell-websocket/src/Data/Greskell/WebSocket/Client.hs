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
         ResultHandle,
         withParser,
         SubmitException(..)
       ) where

import Control.Exception.Safe (throw, Typeable, Exception)
import Data.Aeson (Object, Value, FromJSON)
import qualified Data.Aeson as Aeson
import Data.Greskell.Greskell (ToGreskell(GreskellReturn), toGremlin)
import Data.Greskell.IteratorItem (IteratorItem)
import Data.Text (Text)

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
    rhParseValue :: Value -> Either String v
  }

submit :: (ToGreskell g, r ~ IteratorItem (GreskellReturn g), FromJSON r)
       => Client
       -> g -- ^ Gresmlin script
       -> Maybe Object -- ^ bindings
       -> IO (ResultHandle r)
submit client greskell bindings = fmap (withParser decodeValue) $ submitRaw client script bindings
  where
    script = toGremlin greskell
    decodeValue = resultToEither . Aeson.fromJSON
    resultToEither (Aeson.Error s) = Left s
    resultToEither (Aeson.Success a) = Right a

-- | Less type-safe version of 'submit'.
submitRaw :: Client
          -> Text -- ^ Gremlin script
          -> Maybe Object -- ^ bindings
          -> IO (ResultHandle Value)
submitRaw client script bindings = do
  rh <- Conn.sendRequest conn op
  return $ ResultHandle { rhResHandle = rh,
                          rhParseValue = Right
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

withParser :: (a -> Either String b) -> ResultHandle a -> ResultHandle b
withParser f rh = rh { rhParseValue = new_parser }
  where
    new_parser v = f =<< rhParseValue rh v

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
nextResult rh = parseResponse =<< (Conn.nextResponse $ rhResHandle rh)
  where
    parseResponse Nothing = return Nothing
    parseResponse (Just res) =
      case Res.code $ Res.status res of
       Res.Success -> parseData res
       Res.NoContent -> return Nothing
       Res.PartialContent -> parseData res
       error_code -> throw $ ResponseError res -- TODO: handle Authenticate code
    parseData = undefined -- TODO
