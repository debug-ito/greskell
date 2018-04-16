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
import Data.Bifunctor (first)
import Data.Text (pack)

import Data.Greskell.WebSocket.Codec (Codec(..))

-- | Simple \"application/json\" codec.
jsonCodec :: (FromJSON s) => Codec q s -- TODO: we will need some constaints.
jsonCodec = Codec { mimeType = "application/json",
                    encodeWith = encode,
                    decodeWith = decode
                  }
  where
    encode = undefined
    decode = first pack . eitherDecode'
