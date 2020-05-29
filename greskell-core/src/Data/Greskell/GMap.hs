{-# LANGUAGE TypeFamilies, OverloadedStrings, GeneralizedNewtypeDeriving, DeriveTraversable, CPP #-}
-- |
-- Module: Data.Greskell.GMap
-- Description: data type for g:Map
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- @since 0.1.2.0
--
-- This module defines types for parsing a "g:Map" GraphSON
-- object. Usually users only have to use 'GMapEntry', because other
-- types are just used internally to implement GraphSON parsers.
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
         unGMapEntry,
         parseToGMapEntry
       ) where

import Control.Applicative ((<$>), (<*>), (<|>), empty)
import Data.Aeson
  ( FromJSON(..), ToJSON(..), Value(..),
    FromJSONKey, fromJSONKey, FromJSONKeyFunction(..), ToJSONKey
  )
import Data.Aeson.Types (Parser)
import Data.Foldable (length, Foldable)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Text (Text, intercalate, unpack)
import Data.Traversable (Traversable, traverse)
import Data.Vector ((!), Vector)
import qualified Data.Vector as V
import GHC.Exts (IsList(Item))
import qualified GHC.Exts as List (IsList(fromList, toList))

#if MIN_VERSION_aeson(1,5,0)
import Data.Coerce (coerce)
#else
import Unsafe.Coerce (unsafeCoerce)
#endif

import Data.Greskell.GraphSON.GraphSONTyped (GraphSONTyped(..))

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Aeson as Aeson
-- >>> import Data.HashMap.Strict (HashMap)
-- >>> import qualified Data.HashMap.Strict as HashMap
-- >>> import Data.List (sort)
-- >>> import Data.Either (isLeft)

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

-- | Use 'parseToFlattenedMap'.
instance (FromJSON k, FromJSON v, IsList (c k v), Item (c k v) ~ (k,v)) => FromJSON (FlattenedMap c k v) where
  parseJSON (Array v) = parseToFlattenedMap parseJSON parseJSON v
  parseJSON v = fail ("Expects Array, but got " ++ show v)

-- | Parse a flattened key-values to an associative Vector.
parseToAVec :: (s -> Parser k) -> (s -> Parser v) -> Vector s -> Parser (Vector (k,v))
parseToAVec parseKey parseValue v = 
  if odd vlen
  then fail "Fail to parse a list into an associative list because there are odd number of elements."
  else traverse parsePair pairVec
  where
    vlen = length v
    pairVec = fmap (\i -> (v ! (i * 2), v ! (i * 2 + 1))) $ V.fromList [0 .. ((vlen `div` 2) - 1)]
    parsePair (vk, vv) = (,) <$> parseKey vk <*> parseValue vv

-- | General parser for 'FlattenedMap'.
parseToFlattenedMap :: (IsList (c k v), Item (c k v) ~ (k,v))
                    => (s -> Parser k) -- ^ key parser
                    -> (s -> Parser v) -- ^ value parser
                    -> Vector s -- ^ input vector of flattened key-values.
                    -> Parser (FlattenedMap c k v)
parseToFlattenedMap parseKey parseValue v =
  fmap (FlattenedMap . List.fromList . V.toList) $ parseToAVec parseKey parseValue v

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

-- | Use 'parseToGMap'.
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
-- Basically GraphSON encodes Java's @Map.Entry@ type as if it were a
-- @Map@ with a single entry. Thus its encoded form is either a JSON
-- object or a flattened key-values, as explained in 'GMap'.
--
-- >>> Aeson.eitherDecode "{\"1\": \"one\"}" :: Either String (GMapEntry Int Text)
-- Right (GMapEntry {gmapEntryFlat = False, gmapEntryKey = 1, gmapEntryValue = "one"})
-- >>> Aeson.eitherDecode "[1, \"one\"]" :: Either String (GMapEntry Int Text)
-- Right (GMapEntry {gmapEntryFlat = True, gmapEntryKey = 1, gmapEntryValue = "one"})
-- >>> Aeson.encode (GMapEntry False "one" 1 :: GMapEntry Text Int)
-- "{\"one\":1}"
-- >>> Aeson.encode (GMapEntry True "one" 1 :: GMapEntry Text Int)
-- "[\"one\",1]"
--
-- In old versions of TinkerPop, @Map.Entry@ is encoded as a JSON
-- object with \"key\" and \"value\" fields. 'FromJSON' instance of
-- 'GMapEntry' supports this format as well, but 'ToJSON' instance
-- doesn't support it.
--
-- >>> Aeson.eitherDecode "{\"key\":1, \"value\": \"one\"}" :: Either String (GMapEntry Int Text)
-- Right (GMapEntry {gmapEntryFlat = False, gmapEntryKey = 1, gmapEntryValue = "one"})
data GMapEntry k v =
  GMapEntry
  { gmapEntryFlat :: !Bool,
    gmapEntryKey :: !k,
    gmapEntryValue :: !v
  }
  deriving (Show,Eq,Ord,Foldable,Traversable,Functor)

parseKeyValueToEntry :: (s -> Parser k)
                     -> (s -> Parser v)
                     -> HashMap Text s
                     -> Parser (Maybe (GMapEntry k v))
parseKeyValueToEntry kp vp o =
  if length o /= 2
  then return Nothing
  else do
    mk <- parseIfPresent kp $ HM.lookup "key" o
    mv <- parseIfPresent vp $ HM.lookup "value" o
    return $ GMapEntry False <$> mk <*> mv
  where
    parseIfPresent :: (a -> Parser v) -> Maybe a -> Parser (Maybe v)
    parseIfPresent f = maybe (return Nothing) (fmap Just . f)

parseSingleEntryObjectToEntry :: FromJSONKey k
                              => (s -> Parser v)
                              -> HashMap Text s
                              -> Parser (Maybe (GMapEntry k v))
parseSingleEntryObjectToEntry vp o =
  case HM.toList o of
   [(raw_key, raw_val)] -> do
     key <- parseKey raw_key
     val <- vp raw_val
     return $ Just $ GMapEntry False key val
   _ -> return Nothing
  where
    parseKey k = do
      p <- getParser
      p k
    getParser = case fromJSONKey of
      FromJSONKeyText p -> return $ fmap return p
      FromJSONKeyTextParser p -> return p
      FromJSONKeyValue _ -> fail ( "Unexpected FromJSONKeyValue."
                                   ++ " It expects that the entry key is parsed from the text key in JSON Object,"
                                   ++ " but the key type does not support it."
                                 )
#if MIN_VERSION_aeson(1,5,0)
      FromJSONKeyCoerce -> return $ fmap return coerce
#else
      FromJSONKeyCoerce _ -> return $ fmap return unsafeCoerce
#endif

orElseM :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
orElseM act_a act_b = do
  ma <- act_a
  case ma of
   Just a -> return $ Just a
   Nothing -> act_b

-- | General parser for 'GMapEntry'.
parseToGMapEntry :: FromJSONKey k
                 => (s -> Parser k) -- ^ key parser
                 -> (s -> Parser v) -- ^ value parser
                 -> Either (HashMap Text s) (Vector s) -- ^ input object or flattened key-values
                 -> Parser (GMapEntry k v)
parseToGMapEntry kp vp (Right vec) = do
  avec <- parseToAVec kp vp vec
  case V.toList avec of
   [(key, val)] -> return $ GMapEntry True key val
   _ -> fail ("Expects a single entry of key-value pair, but got " ++ (show $ V.length avec) ++ " entries.")
parseToGMapEntry kp vp (Left o) = do
  m_ret <- parseKeyValueToEntry kp vp o `orElseM` parseSingleEntryObjectToEntry vp o
  case m_ret of
   Just ret -> return ret
   Nothing -> fail ("Unexpected structure of Object: got keys: " ++ (unpack $ intercalate ", " $ HM.keys o))

-- | Map to \"g:Map\".
instance GraphSONTyped (GMapEntry k v) where
  gsonTypeFor _ = "g:Map"

-- | Use 'parseToGMapEntry'.
instance (FromJSON k, FromJSONKey k, FromJSON v) => FromJSON (GMapEntry k v) where
  parseJSON val = case val of
    Object o -> parse $ Left o
    Array a -> parse $ Right a
    other -> fail ("Expects Object or Array, but got " ++ show other)
    where
      parse = parseToGMapEntry parseJSON parseJSON

instance (ToJSON k, ToJSONKey k, Ord k, ToJSON v) => ToJSON (GMapEntry k v) where
  toJSON e = toJSON $ singleton' e
    where
      singleton' :: (Ord k) => GMapEntry k v -> GMap M.Map k v
      singleton' = singleton
  
-- | Get the key-value pair from 'GMapEntry'.
unGMapEntry :: GMapEntry k v -> (k, v)
unGMapEntry e = (gmapEntryKey e, gmapEntryValue e)

-- | Create 'GMap' that has the single 'GMapEntry'.
singleton :: (IsList (c k v), Item (c k v) ~ (k,v)) => GMapEntry k v -> GMap c k v
singleton e = GMap { gmapFlat = gmapEntryFlat e,
                     gmapValue = List.fromList [(gmapEntryKey e, gmapEntryValue e)]
                   }

-- | Deconstruct 'GMap' into a list of 'GMapEntry's.
toList :: (IsList (c k v), Item (c k v) ~ (k,v)) => GMap c k v -> [GMapEntry k v]
toList gm = map toEntry $ List.toList $ gmapValue gm
  where
    toEntry (k, v) = GMapEntry (gmapFlat gm) k v
