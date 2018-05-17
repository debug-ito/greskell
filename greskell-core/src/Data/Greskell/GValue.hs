{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module: Data.Greskell.GValue
-- Description: Aeson Value maybe wrapped in GraphSON
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.GValue
       ( GValue(..),
         GValueBody(..),
         unwrapGraphSON
       ) where

import Control.Applicative ((<$>))
import Data.Aeson (Value(..), FromJSON(..), ToJSON(..))
import qualified Data.Aeson.Types as Aeson (Parser)
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import Data.Greskell.GraphSON (GraphSON(..))
import Data.Hashable (Hashable(..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Traversable (traverse)
import Data.Vector (Vector)
import GHC.Generics (Generic)

-- | An Aeson 'Value' wrapped in 'GraphSON' wrapper type. Useful to
-- parse JSON text in GraphSON format.
newtype GValue = GValue { unGValue :: GraphSON GValueBody }
                 deriving (Show,Eq,Generic)

instance Hashable GValue

data GValueBody =
    GObject !(HashMap Text GValue)
  | GArray !(Vector GValue)
  | GString !Text
  | GNumber !Scientific
  | GBool !Bool
  | GNull
  deriving (Show,Eq,Generic)

instance Hashable GValueBody where
-- See Data.Aeson.Types.Internal
  hashWithSalt s (GObject o) = s `hashWithSalt` (0::Int) `hashWithSalt` o
  hashWithSalt s (GArray a) = foldl' hashWithSalt (s `hashWithSalt` (1::Int)) a
  hashWithSalt s (GString str) = s `hashWithSalt` (2::Int) `hashWithSalt` str
  hashWithSalt s (GNumber n) = s `hashWithSalt` (3::Int) `hashWithSalt` n
  hashWithSalt s (GBool b) = s `hashWithSalt` (4::Int) `hashWithSalt` b
  hashWithSalt s GNull = s `hashWithSalt` (5::Int)

-- | Parse 'GraphSON' wrappers recursively in 'Value', making it into
-- 'GValue'.
instance FromJSON GValue where
  parseJSON input = do
    gv <- parseJSON input
    recursed_value <- recurse $ gsonValue gv
    return $ GValue $ gv { gsonValue = recursed_value }
    where
      recurse :: Value -> Aeson.Parser GValueBody
      recurse (Object o) = GObject <$> traverse parseJSON o
      recurse (Array a) = GArray <$> traverse parseJSON a
      recurse (String s) = return $ GString s
      recurse (Number n) = return $ GNumber n
      recurse (Bool b) = return $ GBool b
      recurse Null = return GNull

-- TODO: implement tests for unwrapGraphSON.

-- TODO: make FromGraphSON class
    
-- | Reconstruct 'Value' from 'GValue'.
instance ToJSON GValue where
  toJSON (GValue gson_body) = toJSON $ fmap toJSON gson_body

instance ToJSON GValueBody where
  toJSON (GObject o) = toJSON o
  toJSON (GArray a) = toJSON a
  toJSON (GString s) = String s
  toJSON (GNumber n) = Number n
  toJSON (GBool b) = Bool b
  toJSON GNull = Null

-- | Just remove 'GraphSON' wrappers from 'GValue'.
unwrapGraphSON :: GValue -> Value
unwrapGraphSON (GValue gson_body) = unwrapBody $ gsonValue gson_body
  where
    unwrapBody GNull = Null
    unwrapBody (GBool b) = Bool b
    unwrapBody (GNumber n) = Number n
    unwrapBody (GString s) = String s
    unwrapBody (GArray a) = Array $ fmap unwrapGraphSON a
    unwrapBody (GObject o) = Object $ fmap unwrapGraphSON o
