-- |
-- Module: Data.Greskell.WebSocket.Client.Options
-- Description: Options to create a Client
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.WebSocket.Client.Options
       ( Options(..)
       ) where

import Data.Aeson (Value)

import Data.Greskell.WebSocket.Connection.Settings (Settings)

data Options =
  Options
  { connectionSettings :: Settings Value
  }
