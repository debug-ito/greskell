-- |
-- Module: Data.Greskell.WebSocket.Client.Settings
-- Description: Settings to create a Client
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.WebSocket.Client.Settings
       ( Settings(..)
       ) where

import Data.Aeson (Value)

import qualified Data.Greskell.WebSocket.Connection.Settings as ConnSet

data Settings =
  Settings
  { connectionSettings :: ConnSet.Settings Value
  }
