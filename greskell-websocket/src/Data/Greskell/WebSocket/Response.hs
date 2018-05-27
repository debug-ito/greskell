{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
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
         codeFromInt,
         isTerminating
       ) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
  ( Object, ToJSON(..), FromJSON(..), Value(Number, Object),
    defaultOptions, genericParseJSON
  )
import Data.Greskell.GraphSON
  ( gsonValue, FromGraphSON(..), parseUnwrapAll, (.:),
    GValueBody(..), gValueBody
  )
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)



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

-- | Returns 'True' if the 'ResponseCode' is a terminating code.
isTerminating :: ResponseCode -> Bool
isTerminating PartialContent = False
isTerminating _ = True

instance FromJSON ResponseCode where
  parseJSON (Number n) = maybe err return $ codeFromInt $ floor n
    where
      err = fail ("Unknown response code: " ++ show n)
  parseJSON v = fail ("Expected Number, but got " ++ show v)

instance FromGraphSON ResponseCode where
  parseGraphSON = parseUnwrapAll

instance ToJSON ResponseCode where
  toJSON = toJSON . codeToInt

-- | \"status\" field.
data ResponseStatus =
  ResponseStatus
  { code :: !ResponseCode,
    message :: !Text,
    attributes :: !Object
  }
  deriving (Show,Eq,Generic)

instance FromJSON ResponseStatus where
  parseJSON v = parseGraphSON =<< parseJSON v

instance FromGraphSON ResponseStatus where
  parseGraphSON gv = case gValueBody gv of
    GObject o ->
      ResponseStatus
      <$> o .: "code"
      <*> o .: "message"
      <*> o .: "attributes"
    gb -> fail ("Expected GObject, but got " ++ show gb)
  

-- | \"result\" field.
data ResponseResult s =
  ResponseResult
  { resultData :: !s,
    -- ^ \"data\" field.
    meta :: !Object
  }
  deriving (Show,Eq,Generic)

instance FromGraphSON s => FromJSON (ResponseResult s) where
  parseJSON v = parseGraphSON =<< parseJSON v

instance FromGraphSON s => FromGraphSON (ResponseResult s) where
  parseGraphSON gv = case gValueBody gv of
    GObject o -> 
      ResponseResult
      <$> o .: "data"
      <*> o .: "meta"
    gb -> fail ("Expected GObject, but got " ++ show gb)

instance Functor ResponseResult where
  fmap f rr = rr { resultData = f $ resultData rr }

-- | ResponseMessage object from Gremlin Server.
--
-- Type @s@ is the type of the response data.
data ResponseMessage s =
  ResponseMessage
  { requestId :: !UUID,
    status :: !ResponseStatus,
    result :: !(ResponseResult s)
  }
  deriving (Show,Eq,Generic)

instance FromGraphSON s => FromJSON (ResponseMessage s) where
  parseJSON v = parseGraphSON =<< parseJSON v

instance FromGraphSON s => FromGraphSON (ResponseMessage s) where
  parseGraphSON gv = case gValueBody gv of
    GObject o -> 
      ResponseMessage
      <$> (o .: "requestId")
      <*> (o .: "status")
      <*> (o .: "result")
    gb -> fail ("Expected GObject, but got " ++ show gb)

instance Functor ResponseMessage where
  fmap f rm = rm { result = fmap f $ result rm }
