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
         toGValue,
         fromGValue,
         unwrapGraphSON
       ) where

import Data.Aeson (Value, FromJSON(..), ToJSON(..))
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import Data.Greskell.GraphSON (GraphSON)
import Data.Hashable (Hashable(..))
import Data.Scientific (Scientific)
import Data.Text (Text)
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

instance FromJSON GValue where
  parseJSON = return . toGValue

instance ToJSON GValue where
  toJSON = fromGValue

-- | Parse 'GraphSON' wrappers recursively in 'Value', making it into
-- 'GValue'.
toGValue :: Value -> GValue
toGValue = undefined

-- | Reconstruct 'Value' from 'GValue'.
--
-- prop> toGValue . fromGValue == id
fromGValue :: GValue -> Value
fromGValue = undefined

-- | Just remove 'GraphSON' wrappers from 'GValue'.
unwrapGraphSON :: GValue -> Value
unwrapGraphSON = undefined
