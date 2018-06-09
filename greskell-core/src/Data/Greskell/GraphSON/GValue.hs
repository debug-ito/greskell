{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module: Data.Greskell.GraphSON.GValue
-- Description: Aeson Value with GraphSON wrappers
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This module is for advanced use. Most users should just use "Data.Greskell.GraphSON".__
--
-- This module defines 'GValue' and exposes its deconstructors.
module Data.Greskell.GraphSON.GValue
       ( -- * GValue type
         GValue(..),
         GValueBody(..),
         -- ** constructors
         nonTypedGValue,
         typedGValue',
         -- ** deconstructors
         -- $caveat_decon
         unwrapAll,
         unwrapOne,
         gValueBody,
         gValueType
       ) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
  ( ToJSON(toJSON), FromJSON(parseJSON), Value(..)
  )
import Data.Aeson.Types (Parser)
import Data.Foldable (foldl')
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Data.Greskell.GraphSON.Core
  ( nonTypedGraphSON, typedGraphSON', GraphSON(..)
  )

-- | An Aeson 'Value' wrapped in 'GraphSON' wrapper type. Basically
-- this type is the Haskell representaiton of a GraphSON-encoded
-- document.
--
-- This type is used to parse GraphSON documents. See also
-- 'FromGraphSON' class.
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
      recurse :: Value -> Parser GValueBody
      recurse (Object o) = GObject <$> traverse parseJSON o
      recurse (Array a) = GArray <$> traverse parseJSON a
      recurse (String s) = return $ GString s
      recurse (Number n) = return $ GNumber n
      recurse (Bool b) = return $ GBool b
      recurse Null = return GNull

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

-- | Create a 'GValue' without \"@type\" field.
nonTypedGValue :: GValueBody -> GValue
nonTypedGValue = GValue . nonTypedGraphSON

-- | Create a 'GValue' with the given \"@type\" field.
typedGValue' :: Text -- ^ \"@type\" field.
             -> GValueBody -> GValue
typedGValue' t b = GValue $ typedGraphSON' t b


-- $caveat_decon
--
-- __In most cases, you should not use these deconstructors.__ That is
-- because internal structure of 'GValue' may vary depending on the
-- Gremlin server instance and its serializer. You should instead use
-- parsers based on 'Data.Greskell.GraphSON.FromGraphSON' class, such
-- as 'Data.Greskell.GraphSON.parseEither'.
--
-- If you are implementing parsers for GraphSON objects described in
-- Gremlin IO Reference
-- (<http://tinkerpop.apache.org/docs/current/dev/io/>), you may use
-- these descructors.
--

-- | Remove all 'GraphSON' wrappers recursively from 'GValue'.
unwrapAll :: GValue -> Value
unwrapAll = unwrapBase unwrapAll

-- | Remove the top-level 'GraphSON' wrapper, but leave other wrappers
-- as-is. The remaining wrappers are reconstructed by 'toJSON' to make
-- them into 'Value'.
unwrapOne :: GValue -> Value
unwrapOne = unwrapBase toJSON

unwrapBase :: (GValue -> Value) -> GValue -> Value
unwrapBase mapChild (GValue gson_body) = unwrapBody $ gsonValue gson_body
  where
    unwrapBody GNull = Null
    unwrapBody (GBool b) = Bool b
    unwrapBody (GNumber n) = Number n
    unwrapBody (GString s) = String s
    unwrapBody (GArray a) = Array $ fmap mapChild a
    unwrapBody (GObject o) = Object $ fmap mapChild o

-- | Get the 'GValueBody' from 'GValue'.
gValueBody :: GValue -> GValueBody
gValueBody = gsonValue . unGValue

-- | Get the 'gsonType' field from 'GValue'.
gValueType :: GValue -> Maybe Text
gValueType = gsonType . unGValue
