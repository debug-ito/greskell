{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
-- |
-- Module: Data.Greskell.GMap
-- Description: data type for g:Map
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.GMap
       ( -- * FlattenedMap
         FlattenedMap(..),
         -- * GMap
         GMap(..),
         unGMap
       ) where

import Control.Applicative ((<$>), (<*>), (<|>), empty)
import Data.Aeson
  ( FromJSON(..), ToJSON(..), Value(..),
    FromJSONKey, ToJSONKey
  )
import Data.Foldable (length)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector ((!))
import GHC.Exts (IsList(Item, fromList, toList))

import Data.Greskell.GraphSON
  (GraphSON(gsonValue), GraphSONTyped(..), parseTypedGraphSON', typedGraphSON)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Aeson as Aeson
-- >>> import Data.HashMap.Strict (HashMap)
-- >>> import Data.List (sort)
-- >>> import Data.Either (isLeft, fromLeft)

-- | JSON encoding of a map as an array of flattened key-value pairs.
-- 
-- 'ToJSON' instance of this type encodes the internal map as an array
-- of keys and values. 'FromJSON' instance of this type parses that
-- flattened map.
--
-- - type @c@: container type for a map (e.g. 'Data.Map.Map' and
--   'Data.HashMap.Strict.HashMap').
-- - type @k@: key of the map.
-- - type @v@: value of the map.
--
-- >>> let decode s = Aeson.eitherDecode s :: Either String (FlattenedMap HashMap Int String)
-- >>> fmap (sort . toList . unFlattenedMap) $ decode "[10, \"ten\", 11, \"eleven\"]"
-- Right [(10,"ten"),(11,"eleven")]
-- >>> fmap (sort . toList . unFlattenedMap) $ decode "[]"
-- Right []
-- >>> let (Left err_msg) = decode "[10, \"ten\", 11]"
-- >>> err_msg
-- ...odd number of elements...
-- >>> Aeson.encode $ FlattenedMap $ (fromList [(10, "ten")] :: HashMap Int String)
-- "[10,\"ten\"]"
newtype FlattenedMap c k v = FlattenedMap { unFlattenedMap :: c k v }
                   deriving (Show,Eq,Ord)

instance (FromJSON k, FromJSON v, IsList (c k v), Item (c k v) ~ (k,v)) => FromJSON (FlattenedMap c k v) where
  parseJSON (Array v) = if odd vlen
                        then fail "Fail to parse a list into FlattenedMap because there are odd number of elements."
                        else fmap (FlattenedMap . fromList) pairs
    where
      vlen = length v
      pairList = map (\i -> (v ! (i * 2), v ! (i * 2 + 1))) [0 .. ((vlen `div` 2) - 1)]
      parsePair (vk, vv) = (,) <$> parseJSON vk <*> parseJSON vv
      pairs = mapM parsePair pairList
  parseJSON _ = fail "Expects Array"

instance (ToJSON k, ToJSON v, IsList (c k v), Item (c k v) ~ (k,v)) => ToJSON (FlattenedMap c k v) where
  toJSON (FlattenedMap m) = toJSON $ flatten $ map toValuePair $ toList m
    where
      toValuePair (k, v) = (toJSON k, toJSON v)
      flatten pl = (\(k, v) -> [k, v]) =<< pl

-- | Map to \"g:Map\".
instance GraphSONTyped (FlattenedMap c k v) where
  gsonTypeFor _ = "g:Map"




-- | Haskell representation of @g:Map@ type.
--
-- GraphSON v1 and v2 encode Java @Map@ type as a JSON Object, while
-- GraphSON v3 encodes it as an array of flattened keys and values
-- (like 'FlattenedMap'.)  'GMap' type handles both encoding schemes.
--
-- >>> Aeson.eitherDecode "{\"ten\": 10}" :: Either String (GMap Text Int)
-- Right (GMap {gmapFlat = False, gmapValue = fromList [("ten",10)]})
-- >>> Aeson.eitherDecode "[\"ten\", 10]" :: Either String (GMap Text Int)
-- Right (GMap {gmapFlat = True, gmapValue = fromList [("ten",10)]})
-- >>> Aeson.encode $ GMap False (fromList [(9, "nine")] :: HashMap Int Text)
-- "{\"9\":\"nine\"}"
-- >>> Aeson.encode $ GMap True (fromList [(9, "nine")] :: HashMap Int Text)
-- "[9,\"nine\"]"
data GMap k v =
  GMap
  { gmapFlat :: !Bool,
    -- ^ If 'True', the map is encoded as an array. If 'False', it's
    -- encoded as a JSON Object.
    gmapValue :: !(HashMap k v)
    -- ^ Map implementation.
  }
  deriving (Show,Eq)

instance (FromJSON k, FromJSONKey k, Eq k, Hashable k, FromJSON v) => FromJSON (GMap k v) where
  parseJSON v@(Object _) = GMap False <$> parseJSON v
  parseJSON v@(Array _) = (GMap True .unFlattenedMap) <$> parseJSON v
  parseJSON _ = empty

instance (ToJSON k, ToJSONKey k, Eq k, Hashable k, ToJSON v) => ToJSON (GMap k v) where
  toJSON gm = if gmapFlat gm
              then toJSON $ FlattenedMap $ unGMap gm
              else toJSON $ unGMap gm

-- | Map to \"g:Map\".
instance GraphSONTyped (GMap k v) where
  gsonTypeFor _ = "g:Map"

-- | Get 'HashMap' from 'GMap'.
unGMap :: GMap k v -> HashMap k v
unGMap = gmapValue
