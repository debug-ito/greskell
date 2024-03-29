-- |
-- Module: Network.Greskell.WebSocket.Connection.Type
-- Description: common types for Connection
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This is an internal module. This defines and exports common types
-- used by Connection modules. The upper module is responsible to
-- limit exports from this module.
module Network.Greskell.WebSocket.Connection.Type
    ( RawReq
    , RawRes
    , ReqID
    , ResPack
    , ReqPack (..)
    , ConnectionState (..)
    , Connection (..)
    , GeneralException (..)
    ) where

import           Control.Concurrent.Async            (Async)
import           Control.Concurrent.STM              (TBQueue, TQueue, TVar)
import           Control.Exception.Safe              (Exception, SomeException, Typeable)
import qualified Data.ByteString.Lazy                as BSL
import           Data.UUID                           (UUID)

import           Network.Greskell.WebSocket.Codec    (Codec)
import           Network.Greskell.WebSocket.Response (ResponseMessage)

type RawReq = BSL.ByteString
type RawRes = BSL.ByteString
type ReqID = UUID

-- | Package of Response data and related stuff.
type ResPack s = Either SomeException (ResponseMessage s)

-- | Package of request data and related stuff. It's passed from the
-- caller thread into WS handling thread.
data ReqPack s
  = ReqPack
      { reqData   :: !RawReq
        -- ^ Encoded request data
      , reqId     :: !ReqID
        -- ^ request ID
      , reqOutput :: !(TQueue (ResPack s))
        -- ^ the output queue for incoming response for this request.
      }

-- | State of the 'Connection'.
data ConnectionState
  = ConnOpen
  -- ^ Connection is open and ready to use.
  | ConnClosing
  -- ^ Connection is closing. It rejects new requests, but keeps
  -- receiving responses for pending requests. When there is no
  -- pending requests, it goes to 'ConnClosed'.
  | ConnClosed
  -- ^ Connection is closed. It rejects requests, and it doesn't
  -- expect any responses. It can close the underlying WebSocket
  -- connection.
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | A WebSocket connection to a Gremlin Server.
--
-- Type @s@ is the body of Response, as in 'ResponseMessage'.
data Connection s
  = Connection
      { connQReq     :: !(TBQueue (ReqPack s))
        -- ^ Request queue to WS (Mux) thread.
      , connState    :: !(TVar ConnectionState)
      , connWSThread :: !(Async ())
        -- ^ WS (Mux) thread. It keeps the underlying WebSocket
        -- connection, watches various types of events and responds to
        -- those events.
      , connCodec    :: !(Codec s)
      }

-- | Exception general to a 'Connection'. It's not related to specific
-- requests.
data GeneralException
  = UnexpectedRequestId UUID
  -- ^ Server sends a 'ResponseMessage' with unknown requestId, which
  -- is kept in this exception.
  | ResponseParseFailure String
  -- ^ The 'Connection' fails to parse a data from the server. The
  -- error message is kept in this exception.
  deriving (Eq, Show, Typeable)

instance Exception GeneralException
