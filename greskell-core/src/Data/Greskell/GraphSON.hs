{-# LANGUAGE OverloadedStrings, DeriveGeneric, TypeFamilies #-}
-- |
-- Module: Data.Greskell.GraphSON
-- Description: Encoding and decoding GraphSON
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.GraphSON
       ( -- * GraphSON
         GraphSON(..),
         GraphSONTyped(..),
         -- ** constructors
         nonTypedGraphSON,
         typedGraphSON,
         typedGraphSON',
         -- ** parser support
         parseTypedGraphSON,
         -- * GValue
         GValue,
         GValueBody(..),
         -- ** constructors
         nonTypedGValue,
         typedGValue',
         -- * FromGraphSON
         FromGraphSON(..),
         -- ** parser support
         parseEither,
         parseUnwrapAll,
         parseUnwrapList,
         (.:),
         parseJSONViaGValue
       ) where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad (when)
import Data.Aeson
  ( ToJSON(toJSON), FromJSON(parseJSON), FromJSONKey,
    object, (.=), Value(..)
  )
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Types as Aeson (parseEither)
import Data.Foldable (Foldable(foldr))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Lazy as L (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable(..))
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.IntMap.Lazy as L (IntMap)
import qualified Data.IntMap.Lazy as LIntMap
import Data.IntSet (IntSet)
import qualified Data.Map.Lazy as L (Map)
import qualified Data.Map.Lazy as LMap
import Data.Monoid (mempty)
import Data.Ratio (Ratio)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text, unpack)
import qualified Data.Text.Lazy as TL
import Data.Traversable (Traversable(traverse))
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)
import GHC.Exts (IsList(Item))
import qualified GHC.Exts as List (fromList, toList)
import GHC.Generics (Generic)

import Data.Greskell.GMap
  ( GMap, GMapEntry, unGMap,
    FlattenedMap, parseToFlattenedMap, parseToGMap, parseToGMapEntry
  )


-- re-exports
import Data.Greskell.GraphSON.Core
import Data.Greskell.GraphSON.GraphSONTyped (GraphSONTyped(..))
import Data.Greskell.GraphSON.GValue


-- $
-- >>> :set -XOverloadedStrings

-- | Types that can be constructed from 'GValue'. This is analogous to
-- 'FromJSON' class.
--
-- Instances of basic types are implemented based on the following
-- rule.
--
-- - Simple scalar types (e.g. 'Int' and 'Text'): use 'parseUnwrapAll'.
-- - List-like types (e.g. '[]', 'Vector' and 'Set'): use
--   'parseUnwrapList'.
-- - Map-like types (e.g. 'L.HashMap' and 'L.Map'): parse into 'GMap'
--   first, then unwrap the 'GMap' wrapper. That way, all versions of
--   GraphSON formats are handled properly.
-- - Other types: see the individual instance documentation.
--
-- Note that 'Char' does not have 'FromGraphSON' instance. This is
-- intentional. As stated in the document of
-- 'Data.Greskell.AsIterator.AsIterator', using 'String' in greskell
-- is an error in most cases. To prevent you from using 'String',
-- 'Char' (and thus 'String') don't have 'FromGraphSON' instances.
class FromGraphSON a where
  parseGraphSON :: GValue -> Parser a

-- | Unwrap the given 'GValue' with 'unwrapAll', and just parse the
-- result with 'parseJSON'.
--
-- Useful to implement 'FromGraphSON' instances for scalar types.
parseUnwrapAll :: FromJSON a => GValue -> Parser a
parseUnwrapAll gv = parseJSON $ unwrapAll gv

---- Looks like we don't need this.

-- -- | Unwrap the given 'GValue' with 'unwrapOne', parse the result to
-- -- @(t GValue)@, and recursively parse the children with
-- -- 'parseGraphSON'.
-- --
-- -- Useful to implement 'FromGraphSON' instances for 'Traversable'
-- -- types.
-- parseUnwrapTraversable :: (Traversable t, FromJSON (t GValue), FromGraphSON a)
--                        => GValue -> Parser (t a)
-- parseUnwrapTraversable gv = traverse parseGraphSON =<< (parseJSON $ unwrapOne gv)

-- | Extract 'GArray' from the given 'GValue', parse the items in the
-- array, and gather them by 'List.fromList'.
--
-- Useful to implement 'FromGraphSON' instances for 'IsList' types.
parseUnwrapList :: (IsList a, i ~ Item a, FromGraphSON i) => GValue -> Parser a
parseUnwrapList (GValue (GraphSON _ (GArray v))) = fmap List.fromList $ traverse parseGraphSON $ List.toList v
parseUnwrapList (GValue (GraphSON _ body)) = fail ("Expects GArray, but got " ++ show body)

-- | Parse 'GValue' into 'FromGraphSON'.
parseEither :: FromGraphSON a => GValue -> Either String a
parseEither = Aeson.parseEither parseGraphSON

-- | Like Aeson's 'Aeson..:', but for 'FromGraphSON'.
(.:) :: FromGraphSON a => HashMap Text GValue -> Text -> Parser a
go .: label = maybe failure parseGraphSON $ HM.lookup label go
  where
    failure = fail ("Cannot find field " ++ unpack label)

-- | Implementation of 'parseJSON' based on 'parseGraphSON'. The input
-- 'Value' is first converted to 'GValue', and it's parsed to the
-- output type.
parseJSONViaGValue :: FromGraphSON a => Value -> Parser a
parseJSONViaGValue v = parseGraphSON =<< parseJSON v

---- Trivial instances

instance FromGraphSON GValue where
  parseGraphSON = return
instance FromGraphSON Int where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Text where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON TL.Text where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Bool where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Double where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Float where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Int8 where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Int16 where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Int32 where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Int64 where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Integer where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Natural where
  parseGraphSON = parseUnwrapAll
instance (FromJSON a, Integral a) => FromGraphSON (Ratio a) where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Word where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Word8 where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Word16 where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Word32 where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Word64 where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Scientific where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON IntSet where
  parseGraphSON = parseUnwrapAll

---- List instances

instance FromGraphSON a => FromGraphSON [a] where
  parseGraphSON = parseUnwrapList
instance FromGraphSON a => FromGraphSON (Vector a) where
  parseGraphSON = parseUnwrapList
instance FromGraphSON a => FromGraphSON (Seq a) where
  parseGraphSON = parseUnwrapList

---- Set instances

instance (FromGraphSON a, Ord a) => FromGraphSON (Set a) where
  parseGraphSON = parseUnwrapList
instance (FromGraphSON a, Eq a, Hashable a) => FromGraphSON (HashSet a) where
  parseGraphSON = parseUnwrapList


---- GMap and others

-- | Use 'parseToFlattenedMap'.
instance (FromGraphSON k, FromGraphSON v, IsList (c k v), Item (c k v) ~ (k,v)) => FromGraphSON (FlattenedMap c k v) where
  parseGraphSON gv = case gValueBody gv of
    GArray a -> parseToFlattenedMap parseGraphSON parseGraphSON a
    b -> fail ("Expects GArray, but got " ++ show b)

parseGObjectToTraversal :: (Traversable t, FromJSON (t GValue), FromGraphSON v)
                        => HashMap Text GValue
                        -> Parser (t v)
parseGObjectToTraversal o = traverse parseGraphSON =<< (parseJSON $ Object $ fmap toJSON o)

-- | Use 'parseToGMap'.
instance (FromGraphSON k, FromGraphSON v, IsList (c k v), Item (c k v) ~ (k,v), Traversable (c k), FromJSON (c k GValue))
         => FromGraphSON (GMap c k v) where
  parseGraphSON gv = case gValueBody gv of
    GObject o -> parse $ Left o
    GArray a -> parse $ Right a
    other -> fail ("Expects GObject or GArray, but got " ++ show other)
    where
      parse = parseToGMap parseGraphSON parseGraphSON parseObject
      -- parseObject = parseUnwrapTraversable . GValue . nonTypedGraphSON . GObject  --- Too many wrapping and unwrappings!!!
      parseObject = parseGObjectToTraversal

-- | Use 'parseToGMapEntry'.
instance (FromGraphSON k, FromGraphSON v, FromJSONKey k) => FromGraphSON (GMapEntry k v) where
  parseGraphSON val = case gValueBody val of
    GObject o -> parse $ Left o
    GArray a -> parse $ Right a
    other -> fail ("Expects GObject or GArray, but got " ++ show other)
    where
      parse = parseToGMapEntry parseGraphSON parseGraphSON


---- Map instances

instance (FromGraphSON v, Eq k, Hashable k, FromJSONKey k, FromGraphSON k) => FromGraphSON (L.HashMap k v) where
  parseGraphSON = fmap unGMap . parseGraphSON
instance (FromGraphSON v, Ord k, FromJSONKey k, FromGraphSON k) => FromGraphSON (L.Map k v) where
  parseGraphSON = fmap unGMap . parseGraphSON
-- IntMap cannot be used with GMap directly..
instance FromGraphSON v => FromGraphSON (L.IntMap v) where
  parseGraphSON = fmap (mapToIntMap . unGMap) . parseGraphSON
    where
      mapToIntMap :: L.Map Int v -> L.IntMap v
      mapToIntMap = LMap.foldrWithKey LIntMap.insert mempty

---- Maybe and Either

-- | Parse 'GNull' into 'Nothing'.
instance FromGraphSON a => FromGraphSON (Maybe a) where
  parseGraphSON (GValue (GraphSON _ GNull)) = return Nothing
  parseGraphSON gv = fmap Just $ parseGraphSON gv

-- | Try 'Left', then 'Right'.
instance (FromGraphSON a, FromGraphSON b) => FromGraphSON (Either a b) where
  parseGraphSON gv = (fmap Left $ parseGraphSON gv) <|> (fmap Right $ parseGraphSON gv)


---- Others

-- | Call 'unwrapAll' to remove all GraphSON wrappers.
instance FromGraphSON Value where
  parseGraphSON = return . unwrapAll

instance FromGraphSON UUID where
  parseGraphSON gv = case gValueBody gv of
    GString t -> maybe failure return $ UUID.fromText t
      where
        failure = fail ("Failed to parse into UUID: " ++ unpack t)
    b -> fail ("Expected GString, but got " ++ show b)

-- | For any input 'GValue', 'parseGraphSON' returns @()@. For
-- example, you can use it to ignore data you get from the Gremlin
-- server.
instance FromGraphSON () where
  parseGraphSON _ = return ()
