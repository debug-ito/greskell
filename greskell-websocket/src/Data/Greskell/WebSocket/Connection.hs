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
         -- ** Settings for Connection
         module Data.Greskell.WebSocket.Connection.Settings,
         -- * Make a request
         sendRequest,
         sendRequest',
         ResponseHandle,
         getResponse,
         slurpResponses,
         -- * Exceptions
         GeneralException(..),
         RequestException(..)
       ) where

import Data.Greskell.WebSocket.Connection.Impl
import Data.Greskell.WebSocket.Connection.Settings
import Data.Greskell.WebSocket.Connection.Type
