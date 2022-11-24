{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Network.Greskell.WebSocket.Request.Common
-- Description: Common data types for Request objects
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
--
module Network.Greskell.WebSocket.Request.Common
    ( Operation (..)
    , SASLMechanism (..)
    , Base64 (..)
    ) where

import           Control.Applicative    (empty)
import           Data.Aeson             (FromJSON (..), Object, ToJSON (..), Value (String))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as B64
import           Data.Text              (Text, unpack)
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)


-- | Class of operation objects.
class Operation o where
  opProcessor :: o -> Text
  -- ^ \"processor\" field.
  opName :: o -> Text
  -- ^ \"op\" field.
  opArgs :: o -> Object
  -- ^ \"args\" field.

instance (Operation a, Operation b) => Operation (Either a b) where
  opProcessor e = either opProcessor opProcessor e
  opName e = either opName opName e
  opArgs e = either opArgs opArgs e


-- | Possible SASL mechanisms.
data SASLMechanism
  = SASLPlain -- ^ \"PLAIN\" SASL
  | SASLGSSAPI -- ^ \"GSSAPI\" SASL
  deriving (Bounded, Enum, Eq, Ord, Show)

instance ToJSON SASLMechanism where
  toJSON = toJSON . toText
    where
      toText :: SASLMechanism -> Text
      toText SASLPlain  = "PLAIN"
      toText SASLGSSAPI = "GSSAPI"

instance FromJSON SASLMechanism where
  parseJSON (String s) = case s of
    "PLAIN"  -> return SASLPlain
    "GSSAPI" -> return SASLGSSAPI
    _        -> fail ("Unknown SASLMechanism: " ++ unpack s)
  parseJSON _ = empty

-- | A raw 'ByteString' encoded to\/decoded from a base64 text.
--
-- 'ToJSON' instance encodes the raw 'ByteString' to a base64-encoded
-- 'Text'. 'FromJSON' is its inverse.
newtype Base64
  = Base64 { unByte64 :: ByteString }
  deriving (Eq, Ord, Show)

instance ToJSON Base64 where
  toJSON (Base64 bs) = toJSON $ decodeUtf8 $ B64.encode bs

instance FromJSON Base64 where
  parseJSON (String t) = either fail (return . Base64) $ B64.decode $ encodeUtf8 t
  parseJSON _          = empty

