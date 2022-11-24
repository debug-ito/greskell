-- |
-- Module: Network.Greskell.WebSocket.Request.Aeson
-- Description: parser support
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __Internal module. End-users should not use this.__
module Network.Greskell.WebSocket.Request.Aeson
    ( genericToJSON
    , genericToEncoding
    , genericParseJSON
    , opt
    , toObject
    ) where

import           Data.Aeson       (Object, ToJSON (..), Value (Object), genericParseJSON,
                                   genericToEncoding, genericToJSON)
import           Data.Aeson.Types (Options, defaultOptions, omitNothingFields)

opt :: Options
opt = defaultOptions { omitNothingFields = True }

toObject :: (ToJSON a) => a -> Object
toObject = expectObject . toJSON
  where
    expectObject (Object o) = o
    expectObject _          = error "Expect Object, but got something else"
