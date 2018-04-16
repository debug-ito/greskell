{-# LANGUAGE DuplicateRecordFields #-}
-- |
-- Module: Data.Greskell.WebSocket.Request
-- Description: Request to Gremlin Server
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.WebSocket.Request
       ( -- * RequestMessage
         RequestMessage(..),
         -- * Operations
         Operation(..),
         OpAuthentication(..),
         SASLMechanism(..),
         OpEval(..),
         OpSessionEval(..),
         SessionID,
         OpClose(..)
       ) where

import Data.Aeson (Object)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.UUID (UUID)

-- | RequestMessage to a Gremlin Server.
data RequestMessage q =
  RequestMessage
  { requestId :: !UUID,
    -- ^ \"requestId\" field.
    requestOperation :: !q
    -- ^ "Operation" object.
  }
  deriving (Show,Eq,Ord)

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

-- | \"authentication\" operation.
data OpAuthentication =
  OpAuthentication
  { processor :: !Text,
    -- | \"processor\" field.
    batchSize :: !(Maybe Int),
    sasl :: !ByteString,
    saslMechanism :: !SASLMechanism
  }
  deriving (Show,Eq,Ord)

-- | Possible SASL mechanisms.
data SASLMechanism = SASLPlain -- ^ \"PLAIN\" SASL
                   | SASLGSSAPI -- ^ \"GSSAPI\" SASL
                   deriving (Show,Eq,Ord,Enum,Bounded)

-- | Sessionless \"eval\" operation.
data OpEval =
  OpEval
  { batchSize :: !(Maybe Int),
    gremlin :: !ByteString,
    binding :: !(Maybe Object),
    language :: !(Maybe Text),
    aliases :: !(Maybe (HashMap Text Text)),
    scriptEvaluationTimeout :: !Int
  }
  deriving (Show,Eq)

-- | Session ID.
type SessionID = UUID

-- | \"eval\" operation in session.
data OpSessionEval =
  OpSessionEval
  { eval :: !OpEval,
    session :: !SessionID,
    manageTransaction :: !(Maybe Bool)
  }
  deriving (Show,Eq)

-- | Session \"close\" operation.
data OpClose =
  OpClose
  { batchSize :: !(Maybe Int),
    session :: !SessionID,
    force :: !(Maybe Bool)
  }
  deriving (Show,Eq,Ord)
