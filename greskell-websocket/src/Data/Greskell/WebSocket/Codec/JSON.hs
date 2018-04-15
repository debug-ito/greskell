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

import Data.Aeson (ToJSON, FromJSON)

import Data.Greskell.WebSocket.Codec (Codec(..))

-- | Simple \"application/json\" codec.
jsonCodec :: (ToJSON q, FromJSON s) => Codec q s
jsonCodec = Codec { mimeType = "application/json",
                    encodeWith = encode,
                    decodeWith = decode
                  }
  where
    encode = undefined
    decode = undefined
