{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, OverloadedStrings #-}
-- |
-- Module: Data.Greskell.WebSocket.Request.Session
-- Description: Operation objects for session OpProcessor
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.WebSocket.Request.Session
       ( OpAuthentication(..),
         SessionID,
         OpEval(..),
         OpClose(..)
       ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Object)
import Data.UUID (UUID)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import GHC.Generics (Generic)

import qualified Data.Greskell.WebSocket.Request.Aeson as GAeson
import Data.Greskell.WebSocket.Request.Common
  (Base64, SASLMechanism, Operation(..))

data OpAuthentication =
  OpAuthentication
  { batchSize :: !(Maybe Int),
    sasl :: !Base64,
    saslMechanism :: !SASLMechanism
  }
  deriving (Show,Eq,Ord,Generic)

instance ToJSON OpAuthentication where
  toJSON = GAeson.genericToJSON GAeson.opt
  toEncoding = GAeson.genericToEncoding GAeson.opt

instance FromJSON OpAuthentication where
  parseJSON = GAeson.genericParseJSON GAeson.opt

instance Operation OpAuthentication where
  opProcessor _ = "session"
  opName _ = "authentication"
  opArgs = GAeson.toObject


type SessionID = UUID

data OpEval =
  OpEval
  { batchSize :: !(Maybe Int),
    gremlin :: !Text,
    bindings :: !(Maybe Object),
    language :: !(Maybe Text),
    aliases :: !(Maybe (HashMap Text Text)),
    scriptEvaluationTimeout :: !(Maybe Int),
    session :: !SessionID,
    manageTransaction :: !(Maybe Bool)
  }
  deriving (Show,Eq,Generic)

instance ToJSON OpEval where
  toJSON = GAeson.genericToJSON GAeson.opt
  toEncoding = GAeson.genericToEncoding GAeson.opt

instance FromJSON OpEval where
  parseJSON = GAeson.genericParseJSON GAeson.opt

instance Operation OpEval where
  opProcessor _ = "session"
  opName _ = "eval"
  opArgs = GAeson.toObject


data OpClose =
  OpClose
  { batchSize :: !(Maybe Int),
    session :: !SessionID,
    force :: !(Maybe Bool)
  }
  deriving (Show,Eq,Ord,Generic)

instance ToJSON OpClose where
  toJSON = GAeson.genericToJSON GAeson.opt
  toEncoding = GAeson.genericToEncoding GAeson.opt

instance FromJSON OpClose where
  parseJSON = GAeson.genericParseJSON GAeson.opt

instance Operation OpClose where
  opProcessor _ = "session"
  opName _ = "close"
  opArgs = GAeson.toObject

