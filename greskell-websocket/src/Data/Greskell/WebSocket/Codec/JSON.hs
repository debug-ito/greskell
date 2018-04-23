{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Data.Greskell.WebSocket.Codec.JSON
-- Description: application/json codec
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.WebSocket.Codec.JSON
       ( jsonCodec
       ) where

import Data.Aeson (ToJSON, FromJSON, eitherDecode')
import qualified Data.Aeson as A
import Data.Bifunctor (first)
import Data.Text (pack)

import Data.Greskell.WebSocket.Codec (Codec(..))

-- | Simple \"application/json\" codec.
--
-- The encoder uses GraphSON v1 format. The decoder supports all
-- GraphSON v1, v2 and v3.
jsonCodec :: (FromJSON s) => Codec s
jsonCodec = Codec { mimeType = "application/json",
                    encodeWith = encode,
                    decodeWith = decode
                  }
  where
    encode = A.encode
    decode = first pack . eitherDecode'
