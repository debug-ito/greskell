{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |
-- Module: Network.Greskell.WebSocket.Request.Session
-- Description: Operation objects for session OpProcessor
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
--
module Network.Greskell.WebSocket.Request.Session
    ( -- * OpAuthentication
      OpAuthentication (..)
      -- * OpEval
    , SessionID
    , OpEval (..)
      -- * OpClose
    , OpClose (..)
    ) where

import           Data.Aeson                                (FromJSON (..), Object, ToJSON (..))
import           Data.HashMap.Strict                       (HashMap)
import           Data.Text                                 (Text)
import           Data.UUID                                 (UUID)
import           GHC.Generics                              (Generic)

import qualified Network.Greskell.WebSocket.Request.Aeson  as GAeson
import           Network.Greskell.WebSocket.Request.Common (Base64, Operation (..), SASLMechanism)

data OpAuthentication
  = OpAuthentication
      { batchSize     :: !(Maybe Int)
      , sasl          :: !Base64
      , saslMechanism :: !SASLMechanism
      }
  deriving (Eq, Generic, Ord, Show)

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

data OpEval
  = OpEval
      { batchSize               :: !(Maybe Int)
      , gremlin                 :: !Text
      , bindings                :: !(Maybe Object)
      , language                :: !(Maybe Text)
      , aliases                 :: !(Maybe (HashMap Text Text))
      , scriptEvaluationTimeout :: !(Maybe Int)
      , session                 :: !SessionID
      , manageTransaction       :: !(Maybe Bool)
      }
  deriving (Eq, Generic, Show)

instance ToJSON OpEval where
  toJSON = GAeson.genericToJSON GAeson.opt
  toEncoding = GAeson.genericToEncoding GAeson.opt

instance FromJSON OpEval where
  parseJSON = GAeson.genericParseJSON GAeson.opt

instance Operation OpEval where
  opProcessor _ = "session"
  opName _ = "eval"
  opArgs = GAeson.toObject


data OpClose
  = OpClose
      { batchSize :: !(Maybe Int)
      , session   :: !SessionID
      , force     :: !(Maybe Bool)
      }
  deriving (Eq, Generic, Ord, Show)

instance ToJSON OpClose where
  toJSON = GAeson.genericToJSON GAeson.opt
  toEncoding = GAeson.genericToEncoding GAeson.opt

instance FromJSON OpClose where
  parseJSON = GAeson.genericParseJSON GAeson.opt

instance Operation OpClose where
  opProcessor _ = "session"
  opName _ = "close"
  opArgs = GAeson.toObject

