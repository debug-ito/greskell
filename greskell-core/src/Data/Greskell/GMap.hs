{-# LANGUAGE TypeFamilies, OverloadedStrings, GeneralizedNewtypeDeriving, DeriveTraversable #-}
-- |
-- Module: Data.Greskell.GMap
-- Description: data type for g:Map
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.GMap
       ( -- * FlattenedMap
         FlattenedMap(..),
         parseToFlattenedMap,
         -- * GMap
         GMap(..),
         unGMap,
         singleton,
         toList,
         parseToGMap,
         -- * GMapEntry
         GMapEntry(..),
         unGMapEntry
       ) where

import Control.Applicative ((<$>), (<*>), (<|>), empty)
import Data.Aeson
  ( FromJSON(..), ToJSON(..), Value(..),
    FromJSONKey, ToJSONKey
  )
import Data.Aeson.Types (Parser)
import Data.Foldable (length, Foldable)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Text (Text)
import Data.Traversable (Traversable)
import Data.Vector ((!), Vector)
import GHC.Exts (IsList(Item))
import qualified GHC.Exts as List (IsList(fromList, toList))

import Data.Greskell.GraphSON.GraphSONTyped (GraphSONTyped(..))

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Aeson as Aeson
-- >>> import Data.HashMap.Strict (HashMap)
-- >>> import qualified Data.HashMap.Strict as HashMap
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
-- >>> let toSortedList = sort . HashMap.toList . unFlattenedMap
-- >>> fmap toSortedList $ decode "[10, \"ten\", 11, \"eleven\"]"
-- Right [(10,"ten"),(11,"eleven")]
-- >>> fmap toSortedList $ decode "[]"
-- Right []
-- >>> let (Left err_msg) = decode "[10, \"ten\", 11]"
-- >>> err_msg
-- ...odd number of elements...
-- >>> Aeson.encode $ FlattenedMap $ (HashMap.fromList [(10, "ten")] :: HashMap Int String)
-- "[10,\"ten\"]"
newtype FlattenedMap c k v = FlattenedMap { unFlattenedMap :: c k v }
                   deriving (Show,Eq,Ord,Foldable,Traversable,Functor)

instance (FromJSON k, FromJSON v, IsList (c k v), Item (c k v) ~ (k,v)) => FromJSON (FlattenedMap c k v) where
  parseJSON (Array v) = parseToFlattenedMap parseJSON parseJSON v
  parseJSON v = fail ("Expects Array, but got " ++ show v)

-- | General parser for 'FlattenedMap'.
parseToFlattenedMap :: (IsList (c k v), Item (c k v) ~ (k,v))
                    => (s -> Parser k) -- ^ key parser
                    -> (s -> Parser v) -- ^ value parser
                    -> Vector s -- ^ input vector of flattened key-values.
                    -> Parser (FlattenedMap c k v)
parseToFlattenedMap parseKey parseValue v =
  if odd vlen
  then fail "Fail to parse a list into FlattenedMap because there are odd number of elements."
  else fmap (FlattenedMap . List.fromList) pairs
  where
    vlen = length v
    pairList = map (\i -> (v ! (i * 2), v ! (i * 2 + 1))) [0 .. ((vlen `div` 2) - 1)]
    parsePair (vk, vv) = (,) <$> parseKey vk <*> parseValue vv
    pairs = mapM parsePair pairList

instance (ToJSON k, ToJSON v, IsList (c k v), Item (c k v) ~ (k,v)) => ToJSON (FlattenedMap c k v) where
  toJSON (FlattenedMap m) = toJSON $ flatten $ map toValuePair $ List.toList m
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
-- - type @c@: container type for a map (e.g. 'Data.Map.Map' and
--   'Data.HashMap.Strict.HashMap').
-- - type @k@: key of the map.
-- - type @v@: value of the map.
--
-- >>> Aeson.eitherDecode "{\"ten\": 10}" :: Either String (GMap HashMap Text Int)
-- Right (GMap {gmapFlat = False, gmapValue = fromList [("ten",10)]})
-- >>> Aeson.eitherDecode "[\"ten\", 10]" :: Either String (GMap HashMap Text Int)
-- Right (GMap {gmapFlat = True, gmapValue = fromList [("ten",10)]})
-- >>> Aeson.encode $ GMap False (HashMap.fromList [(9, "nine")] :: HashMap Int Text)
-- "{\"9\":\"nine\"}"
-- >>> Aeson.encode $ GMap True (HashMap.fromList [(9, "nine")] :: HashMap Int Text)
-- "[9,\"nine\"]"
data GMap c k v =
  GMap
  { gmapFlat :: !Bool,
    -- ^ If 'True', the map is encoded as an array. If 'False', it's
    -- encoded as a JSON Object.
    gmapValue :: !(c k v)
    -- ^ Map implementation.
  }
  deriving (Show,Eq,Foldable,Traversable,Functor)

-- | General parser for 'GMap'.
parseToGMap :: (IsList (c k v), Item (c k v) ~ (k,v))
            => (s -> Parser k) -- ^ key parser
            -> (s -> Parser v) -- ^ value parser
            -> (HashMap Text s -> Parser (c k v)) -- ^ object parser
            -> Either (HashMap Text s) (Vector s) -- ^ input object or flattened key-values.
            -> Parser (GMap c k v)
parseToGMap _ _ op (Left o) = fmap (GMap False) $ op o
parseToGMap kp vp _ (Right v) = fmap (GMap True . unFlattenedMap) $ parseToFlattenedMap kp vp v

instance (FromJSON k, FromJSON v, IsList (c k v), Item (c k v) ~ (k,v), FromJSON (c k v)) => FromJSON (GMap c k v) where
  parseJSON v = case v of
    Object o -> parse $ Left o
    Array a -> parse $ Right a
    other -> fail ("Expects Object or Array, but got " ++ show other)
    where
      parse = parseToGMap parseJSON parseJSON (parseJSON . Object)

instance (ToJSON k, ToJSON v, IsList (c k v), Item (c k v) ~ (k,v), ToJSON (c k v)) => ToJSON (GMap c k v) where
  toJSON gm = if gmapFlat gm
              then toJSON $ FlattenedMap $ unGMap gm
              else toJSON $ unGMap gm

-- | Map to \"g:Map\".
instance GraphSONTyped (GMap c k v) where
  gsonTypeFor _ = "g:Map"

-- | Get the map implementation from 'GMap'.
unGMap :: GMap c k v -> c k v
unGMap = gmapValue

-- | Haskell representation of @Map.Entry@ type.
--
-- GraphSON encodes Java's @Map.Entry@ type as if it were a @Map@ with
-- a single entry. Thus its encoded form is either a JSON object or a
-- flattened key-values, as explained in 'GMap'.
--
-- >>> Aeson.eitherDecode "{\"1\": \"one\"}" :: Either String (GMapEntry Int Text)
-- Right (GMapEntry {gmapEntryFlat = False, gmapEntryKey = 1, gmapEntryValue = "one"})
-- >>> Aeson.eitherDecode "[1, \"one\"]" :: Either String (GMapEntry Int Text)
-- Right (GMapEntry {gmapEntryFlat = True, gmapEntryKey = 1, gmapEntryValue = "one"})
-- >>> Aeson.encode (GMapEntry False "one" 1 :: GMapEntry Text Int)
-- "{\"one\":1}"
-- >>> Aeson.encode (GMapEntry True "one" 1 :: GMapEntry Text Int)
-- "[\"one\",1]"
data GMapEntry k v =
  GMapEntry
  { gmapEntryFlat :: !Bool,
    gmapEntryKey :: !k,
    gmapEntryValue :: !v
  }
  deriving (Show,Eq,Ord,Foldable,Traversable,Functor)

-- | Map to \"g:Map\".
instance GraphSONTyped (GMapEntry k v) where
  gsonTypeFor _ = "g:Map"

instance (FromJSON k, FromJSONKey k, Ord k, FromJSON v) => FromJSON (GMapEntry k v) where
  parseJSON val = toEntry =<< parseJSON val
    where
      toEntry gm = case M.toList $ gmapValue gm of
        [(k,v)] -> return $ GMapEntry { gmapEntryFlat = gmapFlat gm,
                                        gmapEntryKey = k,
                                        gmapEntryValue = v
                                      }
        l -> fail ("Expects a single entry map, but it has " ++ (show $ length l) ++ " entries.")

instance (ToJSON k, ToJSONKey k, Ord k, ToJSON v) => ToJSON (GMapEntry k v) where
  toJSON e = toJSON $ singleton' e
    where
      singleton' :: (Ord k) => GMapEntry k v -> GMap M.Map k v
      singleton' = singleton
  
-- | Get the key-value pair from 'GMapEntry'.
unGMapEntry :: GMapEntry k v -> (k, v)
unGMapEntry e = (gmapEntryKey e, gmapEntryValue e)

singleton :: (IsList (c k v), Item (c k v) ~ (k,v)) => GMapEntry k v -> GMap c k v
singleton e = GMap { gmapFlat = gmapEntryFlat e,
                     gmapValue = List.fromList [(gmapEntryKey e, gmapEntryValue e)]
                   }

toList :: (IsList (c k v), Item (c k v) ~ (k,v)) => GMap c k v -> [GMapEntry k v]
toList gm = map toEntry $ List.toList $ gmapValue gm
  where
    toEntry (k, v) = GMapEntry (gmapFlat gm) k v
