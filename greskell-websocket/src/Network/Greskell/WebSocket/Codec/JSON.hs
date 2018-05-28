{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Network.Greskell.WebSocket.Codec.JSON
-- Description: application\/json codec
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Network.Greskell.WebSocket.Codec.JSON
       ( jsonCodec
       ) where

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as A
import Data.Aeson.Types (parseEither)

import Data.Greskell.GraphSON (FromGraphSON(..))

import Network.Greskell.WebSocket.Codec (Codec(..))

-- | Simple \"application\/json\" codec.
--
-- The encoder uses GraphSON v1 format. The decoder supports all
-- GraphSON v1, v2 and v3.
jsonCodec :: (FromGraphSON s) => Codec s
jsonCodec = Codec { mimeType = "application/json",
                    encodeWith = encode,
                    decodeWith = decode
                  }
  where
    encode = A.encode
    decode bs = parseEither parseGraphSON =<< A.eitherDecode' bs
