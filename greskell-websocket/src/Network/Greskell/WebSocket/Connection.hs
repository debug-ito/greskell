-- |
-- Module: Network.Greskell.WebSocket.Connection
-- Description: WebSocket Connection to Gremlin Server
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Network.Greskell.WebSocket.Connection
       ( -- * Make a Connection
         connect,
         close,
         Connection,
         Host,
         Port,
         -- ** Settings for Connection
         module Network.Greskell.WebSocket.Connection.Settings,
         -- * Make a request
         sendRequest,
         sendRequest',
         ResponseHandle,
         nextResponse,
         nextResponseSTM,
         slurpResponses,
         -- * Exceptions
         GeneralException(..),
         RequestException(..)
       ) where

import Network.Greskell.WebSocket.Connection.Impl
import Network.Greskell.WebSocket.Connection.Settings
import Network.Greskell.WebSocket.Connection.Type
