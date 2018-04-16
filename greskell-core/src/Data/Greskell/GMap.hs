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

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import Data.Foldable (length)
import Data.Vector ((!))
import GHC.Exts (IsList(Item, fromList, toList))

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Aeson as Aeson
-- >>> import Data.HashMap.Strict (HashMap)
-- >>> import Data.List (sort, isInfixOf)
-- >>> import Data.Either (isLeft, fromLeft)

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
-- >>> fmap (sort . toList . unGMap) $ (Aeson.eitherDecode "[10, \"ten\", 11, \"eleven\"]" :: Either String (GMap HashMap Int String))
-- Right [(10,"ten"),(11,"eleven")]
-- >>> fmap (sort . toList . unGMap) $ (Aeson.eitherDecode "[]" :: Either String (GMap HashMap String String))
-- Right []
-- >>> let (Left err_msg) = (Aeson.eitherDecode "[10, \"ten\", 11]" :: Either String (GMap HashMap Int String))
-- >>> "odd number of elements" `isInfixOf` err_msg
-- True
-- >>> Aeson.toJSON $ GMap $ (fromList [(10, "ten")] :: HashMap Int String)
-- Array [Number 10.0,String "ten"]
newtype GMap c k v = GMap { unGMap :: c k v }
                   deriving (Show,Eq,Ord)

instance (FromJSON k, FromJSON v, IsList (c k v), Item (c k v) ~ (k,v)) => FromJSON (GMap c k v) where
  parseJSON (Array v) = if odd vlen
                        then fail "Fail to parse a list into GMap because there are odd number of elements."
                        else fmap (GMap . fromList) pairs
    where
      vlen = length v
      pairList = map (\i -> (v ! (i * 2), v ! (i * 2 + 1))) [0 .. ((vlen `div` 2) - 1)]
      parsePair (vk, vv) = (,) <$> parseJSON vk <*> parseJSON vv
      pairs = mapM parsePair pairList
  parseJSON _ = fail "Expects Array"

instance (ToJSON k, ToJSON v, IsList (c k v), Item (c k v) ~ (k,v)) => ToJSON (GMap c k v) where
  toJSON (GMap m) = toJSON $ flatten $ map toValuePair $ toList m
    where
      toValuePair (k, v) = (toJSON k, toJSON v)
      flatten pl = (\(k, v) -> [k, v]) =<< pl
