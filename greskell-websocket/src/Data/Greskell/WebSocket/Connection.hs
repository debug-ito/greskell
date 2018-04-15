-- |
-- Module: Data.Greskell.WebSocket.Connection
-- Description: WebSocket Connection to Gremlin Server
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.WebSocket.Connection
       ( connect,
         close,
         Connection,
         Host,
         Port
       ) where

-- | A WebSocket connection to a Gremlin Server.
data Connection

-- | Host name or an IP address.
type Host = String

-- | TCP port number.
type Port = Int

-- | Make a 'Connection' to a Gremlin Server.
connect :: Host -> Port -> IO Connection
connect = undefined

-- | Close the 'Connection'.
close :: Connection -> IO ()
close = undefined

