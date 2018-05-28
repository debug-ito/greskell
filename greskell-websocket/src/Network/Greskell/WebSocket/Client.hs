-- |
-- Module: Network.Greskell.WebSocket.Client
-- Description: High-level interface to Gremlin Server
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Network.Greskell.WebSocket.Client
       ( -- * Make a Client
         connect,
         connectWith,
         close,
         Client,
         Host,
         Port,
         -- ** Options for Client
         module Network.Greskell.WebSocket.Client.Options,
         -- * Submit evaluation requests
         submit,
         submitRaw,
         ResultHandle,
         nextResult,
         nextResultSTM,
         slurpResults,
         -- * Exceptions
         SubmitException(..)
       ) where

import Network.Greskell.WebSocket.Client.Impl
import Network.Greskell.WebSocket.Client.Options
import Network.Greskell.WebSocket.Connection (Host, Port)
