-- |
-- Module: Data.Greskell.WebSocket.Client
-- Description: High-level interface to Gremlin Server
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.WebSocket.Client
       ( -- * Make a Client
         connect,
         close,
         Client,
         Host,
         Port,
         -- ** Options for Client
         module Data.Greskell.WebSocket.Client.Options,
         -- * Submit evaluation requests
         submit,
         submitRaw,
         nextResult,
         nextResultSTM,
         -- * Exceptions
         ResultHandle,
         SubmitException(..)
       ) where

import Data.Greskell.WebSocket.Client.Impl
import Data.Greskell.WebSocket.Client.Options
import Data.Greskell.WebSocket.Connection (Host, Port)
