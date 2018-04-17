{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
-- |
-- Module: Data.Greskell.GMap
-- Description: data type for g:Map
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.GMap
       ( -- * GMap
         GMap(..),
         -- * GraphSONObject
         GraphSONObject(..)
       ) where

import Control.Applicative ((<$>), (<*>), (<|>))
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import Data.Foldable (length)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector ((!))
import GHC.Exts (IsList(Item, fromList, toList))

import Data.Greskell.GraphSON
  (GraphSON(gsonValue), GraphSONTyped(..), parseTypedGraphSON, typedGraphSON)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Aeson as Aeson
-- >>> import Data.HashMap.Strict (HashMap)
-- >>> import Data.List (sort, isInfixOf)
-- >>> import Data.Either (isLeft, fromLeft)

-- | Haskell representation of @g:Map@ type in GraphSON.
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
-- >>> Aeson.encode $ GMap $ (fromList [(10, "ten")] :: HashMap Int String)
-- "[10,\"ten\"]"
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

-- | Map to \"g:Map\".
instance GraphSONTyped (GMap c k v) where
  gsonTypeFor _ = "g:Map"

-- | If key type of a @g:Map@ is Text, the @g:Map@ type can be
-- expressed as a plain JSON object (in GraphSON v1 and v2) as well as
-- a @g:Map@ object (in GraphSON v3). 'GraphSONObject' parses and
-- formats both cases.
--
-- >>> Aeson.eitherDecode "{\"ten\": 10}" :: Either String (GraphSONObject Int)
-- Right (GraphSONObject (fromList [("ten",10)]))
-- >>> Aeson.eitherDecode "{\"@type\": \"g:Map\", \"@value\": [\"ten\", 10]}" :: Either String (GraphSONObject Int)
-- Right (GraphSONGMap (GMap {unGMap = fromList [("ten",10)]}))
-- >>> Aeson.encode $ GraphSONObject (fromList [("ten", 10)] :: HashMap Text Int)
-- "{\"ten\":10}"
-- >>> let result = Aeson.encode $ GraphSONGMap $ GMap (fromList [("ten", 10)] :: HashMap Text Int)
-- >>> result
-- ...\"@type\":\"g:Map\"...
-- >>> result
-- ...\"@value\":[\"ten\",10]...
data GraphSONObject v = GraphSONObject (HashMap Text v)
                        -- ^ the 'HashMap' is encoded as a plain JSON object.
                      | GraphSONGMap (GMap HashMap Text v)
                        -- ^ the 'HashMap' is encoded as a @g:Map@ object.
                      deriving (Show,Eq)

-- | Map to \"g:Map\".
instance GraphSONTyped (GraphSONObject v) where
  gsonTypeFor _ = "g:Map"

instance (FromJSON v) => FromJSON (GraphSONObject v) where
  parseJSON v = (fmap toGraphSONGMap $ parseTypedGraphSON v) <|> (fmap GraphSONObject $ parseJSON v)
    where
      toGraphSONGMap = GraphSONGMap . gsonValue

instance (ToJSON v) => ToJSON (GraphSONObject v) where
  toJSON (GraphSONObject hm) = toJSON hm
  toJSON (GraphSONGMap gm) = toJSON $ typedGraphSON gm
