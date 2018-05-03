-- |
-- Module: Data.Greskell.WebSocket.Connection.Type
-- Description: common types for Connection
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This is an internal module. This defines and exports common types
-- used by Connection modules. The upper module is responsible to
-- limit exports from this module.
module Data.Greskell.WebSocket.Connection.Type
  ( RawReq,
    RawRes,
    ReqID,
    ResPack,
    ReqPack(..),
    Connection(..),
    GeneralException(..)
  ) where

import Control.Concurrent.Async (Async)
import Control.Exception.Safe (SomeException, Typeable, Exception)
import Control.Concurrent.STM (TQueue, TBQueue)
import qualified Data.ByteString.Lazy as BSL
import Data.UUID (UUID)

import Data.Greskell.WebSocket.Response (ResponseMessage)
import Data.Greskell.WebSocket.Codec (Codec)

type RawReq = BSL.ByteString
type RawRes = BSL.ByteString
type ReqID = UUID

-- | Package of Response data and related stuff.
type ResPack s = Either SomeException (ResponseMessage s)

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

-- | Exception general to a 'Connection'. It's not related to specific
-- requests.
data GeneralException =
  UnexpectedRequestId
  -- ^ Server sends a 'ResponseMessage' with unknown requestId.
  deriving (Show,Eq,Typeable)

instance Exception GeneralException
