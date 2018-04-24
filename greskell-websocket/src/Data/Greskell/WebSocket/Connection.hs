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
         ResponseHandle,
         getResponse
       ) where

import Data.Greskell.WebSocket.Codec (Codec)
import Data.Greskell.WebSocket.Request (RequestMessage)
import Data.Greskell.WebSocket.Response (ResponseMessage)

-- | A WebSocket connection to a Gremlin Server.
data Connection

-- | Host name or an IP address.
type Host = String

-- | TCP port number.
type Port = Int

-- | Make a 'Connection' to a Gremlin Server.
--
-- TODO: define exception spec.
connect :: Host -> Port -> IO Connection
connect = undefined

-- | Close the 'Connection'.
close :: Connection -> IO ()
close = undefined

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
