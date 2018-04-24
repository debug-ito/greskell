-- |
-- Module: Data.Greskell.WebSocket.Codec
-- Description: Encoder/decoder of Request/Response
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.WebSocket.Codec
       ( Codec(..),
         ErrorMessage
       ) where

import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)

import Data.Greskell.WebSocket.Request (RequestMessage)
import Data.Greskell.WebSocket.Response (ResponseMessage)

type ErrorMessage = String

-- | Encoder of 'RequestMessage' and decoder of 'ResponseMessage',
-- associated with a MIME type.
data Codec s =
  Codec
  { mimeType :: Text,
    encodeWith :: RequestMessage -> BSL.ByteString,
    decodeWith :: BSL.ByteString -> Either ErrorMessage (ResponseMessage s)
  }

