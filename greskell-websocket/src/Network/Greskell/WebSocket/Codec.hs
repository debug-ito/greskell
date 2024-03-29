-- |
-- Module: Network.Greskell.WebSocket.Codec
-- Description: Encoder\/decoder of Request\/Response
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
--
module Network.Greskell.WebSocket.Codec
    ( -- * Codec
      Codec (..)
      -- * Request encoder
    , encodeBinaryWith
    , messageHeader
      -- * Request decoder
    , decodeBinary
    ) where

import           Control.Monad                       (when)
import qualified Data.ByteString.Lazy                as BSL
import           Data.Monoid                         ((<>))
import           Data.Text                           (Text)
import           Data.Text.Encoding                  (decodeUtf8', encodeUtf8)

import           Network.Greskell.WebSocket.Request  (RequestMessage)
import           Network.Greskell.WebSocket.Response (ResponseMessage)

-- | Encoder of 'RequestMessage' and decoder of 'ResponseMessage',
-- associated with a MIME type.
--
-- Type @s@ is the body of Response.
data Codec s
  = Codec
      { mimeType   :: Text
        -- ^ MIME type sent to the server
      , encodeWith :: RequestMessage -> BSL.ByteString
        -- ^ Request encoder
      , decodeWith :: BSL.ByteString -> Either String (ResponseMessage s)
        -- ^ Response decoder
      }

instance Functor Codec where
  fmap f c = c { decodeWith = (fmap . fmap . fmap) f $ decodeWith c }

-- | Make a request message header.
messageHeader :: Text -- ^ MIME type
              -> BSL.ByteString
messageHeader mime = BSL.singleton size <> mime_bin
  where
    size = fromIntegral $ BSL.length mime_bin -- what if 'mime' is too long??
    mime_bin = BSL.fromStrict $ encodeUtf8 mime

-- | Encode a 'RequestMessage' into a \"binary\" format of Gremlin
-- Server. The result includes the message \"header\" and the
-- \"payload\".
encodeBinaryWith :: Codec s -> RequestMessage -> BSL.ByteString
encodeBinaryWith c req = messageHeader (mimeType c) <> encodeWith c req

-- | Decode a message in the \"binary\" format. This is mainly for
-- testing purposes.
decodeBinary :: BSL.ByteString
             -> Either String (Text, BSL.ByteString) -- ^ (mimeType, payload)
decodeBinary raw_msg = do
  case BSL.uncons raw_msg of
   Nothing               -> Left "Length of MIME type is missing in the header."
   Just (mime_len, rest) -> decodeMimeAndPayload mime_len rest
  where
    decodeMimeAndPayload mime_lenw msg = do
      when (BSL.length mime_field /= mime_len) $ Left ("Too short MIME field: " <> show mime_field)
      mime_text <- either (Left . show) Right $ decodeUtf8' $ BSL.toStrict $ mime_field
      return (mime_text, payload)
      where
        (mime_field, payload) = BSL.splitAt mime_len msg
        mime_len = fromIntegral mime_lenw
