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
         -- * GraphSONObject
         GraphSONObject(..),
         gsonObject
       ) where

import Control.Applicative ((<$>), (<*>), (<|>))
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import Data.Foldable (length)
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

-- | JSON encoding of a map as a flattened list of key-value pairs.
-- 
-- 'ToJSON' instance of this type encodes the internal map as a list
-- of keys and values. 'FromJSON' instance of this type parses that
-- flattened list.
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

-- | If key type of a @g:Map@ is Text, the @g:Map@ type can be
-- expressed as a plain JSON object (in GraphSON v1 and v2) as well as
-- a @g:Map@ object (in GraphSON v3). 'GraphSONObject' parses and
-- formats both cases.
--
-- Note that 'FromJSON' instance of this type tries to parse the
-- GraphSON \"typed\" object (i.e. \"{\"\@type\": ...}\" stuff), so
-- enclosing 'GraphSONObject' with 'GraphSON' type is usually a bad
-- idea.
--
-- >>> Aeson.eitherDecode "{\"ten\": 10}" :: Either String (GraphSONObject Int)
-- Right (GraphSONObject (fromList [("ten",10)]))
-- >>> Aeson.eitherDecode "{\"@type\": \"g:Map\", \"@value\": [\"ten\", 10]}" :: Either String (GraphSONObject Int)
-- Right (GraphSONGMap (fromList [("ten",10)]))
-- >>> Aeson.encode $ GraphSONObject (fromList [("ten", 10)] :: HashMap Text Int)
-- "{\"ten\":10}"
-- >>> let result = Aeson.encode $ GraphSONGMap (fromList [("ten", 10)] :: HashMap Text Int)
-- >>> result
-- ...\"@type\":\"g:Map\"...
-- >>> result
-- ...\"@value\":[\"ten\",10]...
data GraphSONObject v = GraphSONObject (HashMap Text v)
                        -- ^ the 'HashMap' is encoded as a plain JSON object.
                      | GraphSONGMap (HashMap Text v)
                        -- ^ the 'HashMap' is encoded as a @g:Map@ object.
                      deriving (Show,Eq)

-- | Map to \"g:Map\".
instance GraphSONTyped (GraphSONObject v) where
  gsonTypeFor _ = "g:Map"

instance (FromJSON v) => FromJSON (GraphSONObject v) where
  parseJSON v = either toObject toGMap =<< parseTypedGraphSON' v
    where
      toObject _ = GraphSONObject <$> parseJSON v
      toGMap = return . GraphSONGMap . unFlattenedMap . gsonValue

instance (ToJSON v) => ToJSON (GraphSONObject v) where
  toJSON (GraphSONObject hm) = toJSON hm
  toJSON (GraphSONGMap hm) = toJSON $ typedGraphSON $ FlattenedMap hm

-- | Extract 'HashMap' from 'GraphSONObject'.
gsonObject :: GraphSONObject v -> HashMap Text v
gsonObject (GraphSONObject hm) = hm
gsonObject (GraphSONGMap hm) = hm
