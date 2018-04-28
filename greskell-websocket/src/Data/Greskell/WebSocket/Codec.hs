-- |
-- Module: Data.Greskell.WebSocket.Codec
-- Description: Encoder/decoder of Request/Response
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.WebSocket.Codec
       ( Codec(..),
         ErrorMessage,
         encodeBinaryWith,
         messageHeader
       ) where

import qualified Data.ByteString.Lazy as BSL
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

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

-- | Make a request message header.
messageHeader :: Text -- ^ MIME type
              -> BSL.ByteString
messageHeader mime = BSL.singleton size <> mime_bin
  where
    size = fromIntegral $ BSL.length mime_bin -- what if 'mime' is too long??
    mime_bin = BSL.fromStrict $ encodeUtf8 mime

-- | Encode a 'ResponseMessage' into a \"binary\" format of Gremlin
-- Server. The result includes the message \"header\" and the
-- \"payload\".
encodeBinaryWith :: Codec s -> RequestMessage -> BSL.ByteString
encodeBinaryWith c req = messageHeader (mimeType c) <> encodeWith c req
