{-# LANGUAGE TypeFamilies #-}
-- |
-- Module: Data.Greskell.GMap
-- Description: data type for g:Map
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.GMap
       ( GMap(..) 
       ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Exts (IsList(Item, fromList))

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Aeson as Aeson
-- >>> import Data.HashMap.Strict (HashMap, toList)
-- >>> import Data.List (sort)

-- | 'GMap' corresponds to @g:Map@ type in GraphSON.
-- 
-- In GraphSON v3, @g:Map@ is encoded as a flattened list of keys and
-- values. 'FromJSON' and 'ToJSON' instances of 'GMap' implements this
-- encoding.
--
-- - type @c@: container type for a map (e.g. 'Data.Map.Map' and
--   'Data.HashMap.Strict.HashMap').
-- - type @k@: key of the map.
-- - type @v@: value of the map.
--
-- >>> fmap (sort . toList . unGMap) $ (Aeson.decode "[10, \"ten\", 11, \"eleven\"]" :: Maybe (GMap HashMap Int String))
-- Just [(10,"ten"),(11,"evel")]
-- >>> fmap (sort . toList . unGMap) $ (Aeson.decode "[]" :: Maybe (GMap HashMap String String))
-- Just []
-- >>> fmap (sort . toList . unGMap) $ (Aeson.eitherDecode "[10, \"ten\", 11]" :: Either String (GMap HashMap Int String))
-- Left "Fail to parse a list into GMap because there are odd number of elements."
newtype GMap c k v = GMap { unGMap :: c k v }
                   deriving (Show,Eq,Ord)

instance (FromJSON k, FromJSON v, IsList (c k v), Item (c k v) ~ (k,v)) => FromJSON (GMap c k v) where
  parseJSON = undefined

instance (ToJSON k, ToJSON v, IsList (c k v), Item (c k v) ~ (k,v)) => ToJSON (GMap c k v) where
  toJSON = undefined
