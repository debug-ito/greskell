{-# LANGUAGE DuplicateRecordFields, DeriveGeneric #-}
-- |
-- Module: Data.Greskell.WebSocket.Request
-- Description: Request to Gremlin Server
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.WebSocket.Request
       ( -- * RequestMessage
         RequestMessage(..),
         toRequestMessage
       ) where

import Data.Aeson (Object, ToJSON(..), FromJSON(..))
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

import qualified Data.Greskell.WebSocket.Request.Aeson as GAeson
import Data.Greskell.WebSocket.Request.Common (Operation(..))


-- | RequestMessage to a Gremlin Server.
data RequestMessage =
  RequestMessage
  { requestId :: !UUID,
    op :: !Text,
    processor :: !Text,
    args :: !Object
  }
  deriving (Show,Eq,Generic)

instance ToJSON RequestMessage where
  toJSON = GAeson.genericToJSON GAeson.opt
  toEncoding = GAeson.genericToEncoding GAeson.opt

instance FromJSON RequestMessage where
  parseJSON = GAeson.genericParseJSON GAeson.opt

toRequestMessage :: Operation o => UUID -> o -> RequestMessage
toRequestMessage rid o =
  RequestMessage { requestId = rid,
                   op = opName o,
                   processor = opProcessor o,
                   args = opArgs o
                 }
