-- |
-- Module: Data.Greskell.WebSocket.Client.Options
-- Description: Options to create a Client
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.WebSocket.Client.Options
       ( -- * Options
         Options,
         defOptions,
         -- ** accessor functions
         connectionSettings,
         -- * Settings
         module Data.Greskell.WebSocket.Connection.Settings
       ) where

import Data.Aeson (Value)

import Data.Greskell.WebSocket.Connection (Connection)
import Data.Greskell.WebSocket.Connection.Settings

-- | Configuration options to create a client for Gremlin Server.
--
-- You can get the default 'Options' by 'defOptions' function, and
-- customize its fields by accessor functions.
data Options =
  Options
  { connectionSettings :: Settings Value
    -- ^ Settings for the underlying 'Connection'. Default:
    -- 'defJSONSettings'.
  }

defOptions :: Options
defOptions =
  Options
  { connectionSettings = defJSONSettings
  }
