-- |
-- Module: Data.Greskell.WebSocket.Response
-- Description: Response from Gremlin Server
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.WebSocket.Response
       ( ResponseMessage(..),
         ResponseStatus(..),
         ResponseResult(..),
         ResponseCode(..),
         codeToInt,
         codeFromInt
       ) where

import Data.Aeson (Object)
import Data.Text (Text)
import Data.UUID (UUID)

-- | Response status code
data ResponseCode =
    Success
  | NoContent
  | PartialContent
  | Unauthorized
  | Authenticate
  | MalformedRequest
  | InvalidRequestArguments
  | ServerError
  | ScriptEvaluationError
  | ServerTimeout
  | ServerSerializationError
  deriving (Show,Eq,Ord,Enum,Bounded)

codeToInt :: ResponseCode -> Int
codeToInt c = case c of
  Success -> 200
  NoContent -> 204
  PartialContent -> 206
  Unauthorized -> 401
  Authenticate -> 407
  MalformedRequest -> 498
  InvalidRequestArguments -> 499
  ServerError -> 500
  ScriptEvaluationError -> 597
  ServerTimeout -> 598
  ServerSerializationError -> 599

codeFromInt :: Int -> Maybe ResponseCode
codeFromInt i = case i of
  200 -> Just Success
  204 -> Just NoContent
  206 -> Just PartialContent
  401 -> Just Unauthorized
  407 -> Just Authenticate
  498 -> Just MalformedRequest
  499 -> Just InvalidRequestArguments
  500 -> Just ServerError
  597 -> Just ScriptEvaluationError
  598 -> Just ServerTimeout
  599 -> Just ServerSerializationError
  _ -> Nothing

-- | \"status\" field.
data ResponseStatus =
  ResponseStatus
  { code :: ResponseCode,
    message :: Text,
    attributes :: Object
  }
  deriving (Show,Eq)

-- | \"result\" field.
data ResponseResult s =
  ResponseResult
  { resultData :: s,
    -- ^ \"data\" field.
    meta :: Object
  }
  deriving (Show,Eq)

-- | ResponseMessage object from Gremlin Server.
data ResponseMessage s =
  ResponseMessage
  { requestId :: UUID,
    status :: ResponseStatus,
    result :: ResponseResult s
  }
  deriving (Show,Eq)
