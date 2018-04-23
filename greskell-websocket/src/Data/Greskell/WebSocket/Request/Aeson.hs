-- |
-- Module: Data.Greskell.WebSocket.Request.Aeson
-- Description: parser support
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __Internal module. End-users should not use this.__
module Data.Greskell.WebSocket.Request.Aeson
       ( genericToJSON, genericToEncoding, genericParseJSON,
         opt
       ) where

import Data.Aeson
  ( genericToJSON, genericToEncoding, genericParseJSON,
    defaultOptions, omitNothingFields, Options
  )

opt :: Options
opt = defaultOptions { omitNothingFields = True }
