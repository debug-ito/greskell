{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveTraversable, OverloadedStrings #-}
-- |
-- Module: Data.Greskell.Graph.PropertyMap
-- Description: [Deprecated] PropertyMap class and types
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 'PropertyMap' was used in greskell prior than 1.0.0.0, but is now
-- deprecated. Use "Data.Greskell.PMap" instead.
module Data.Greskell.Graph.PropertyMap {-# DEPRECATED "Use PMap instead" #-}
  ( -- ** PropertyMap
    PropertyMap(..),
    PropertyMapSingle,
    PropertyMapList,
    lookupOneValue,
    lookupListValues,
    parseOneValue,
    parseListValues,
    parseNonEmptyValues,
    fromProperties,
    -- * Internal use
    FromGraphSONWithKey,
    -- * Re-exports
    AProperty(..),
    AVertexProperty(..)
  ) where

import Control.Applicative (empty, (<|>))
import Data.Aeson (FromJSON(..))
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import Data.Foldable (Foldable(..), foldlM)
import Data.Greskell.GraphSON
  ( FromGraphSON(..), GValue, GraphSONTyped(..), (.:),
    parseJSONViaGValue
  )
import Data.Greskell.GraphSON.GValue (gValueBody, gValueType, GValueBody(..))
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NL
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup((<>)))
import qualified Data.Semigroup as Semigroup
import Data.Text (Text, unpack)
import Data.Traversable (Traversable(..))
import Data.Vector (Vector)

import Data.Greskell.Graph (Property(..), AProperty(..), AVertexProperty(..))

-- | Common basic operations supported by maps of properties.
class PropertyMap m where
  lookupOne :: Text -> m p v -> Maybe (p v)
  -- ^ Look up a property associated with the given key.
  lookupOne k m = listToMaybe $ lookupList k m
  lookupList :: Text -> m p v -> [p v]
  -- ^ Look up all properties associated with the given key.
  putProperty :: Property p => p v -> m p v -> m p v
  -- ^ Put a property into the map.
  removeProperty :: Text -> m p v -> m p v
  -- ^ Remove all properties associated with the given key.
  allProperties :: m p v -> [p v]
  -- ^ Return all properties in the map.

-- | Lookup a property value from a 'PropertyMap' by key.
lookupOneValue :: (PropertyMap m, Property p) => Text -> m p v -> Maybe v
lookupOneValue k = fmap propertyValue . lookupOne k

-- | Lookup a list of property values from a 'PropertyMap' by key.
lookupListValues :: (PropertyMap m, Property p) => Text -> m p v -> [v]
lookupListValues k = fmap propertyValue . lookupList k

notExistErrorMsg :: Text -> String
notExistErrorMsg k = "Property '" ++ unpack k ++ "' does not exist."

-- | Lookup a property 'GValue' by the given key, and parse it.
-- 
-- In version 0.1.1.0 and before, this function took an argument @m p (GraphSON Value)@.
-- This has changed, because property types for 'AVertex' etc have changed.
parseOneValue :: (PropertyMap m, Property p, FromGraphSON v) => Text -> m p GValue -> Parser v
parseOneValue k pm = maybe (fail err_msg) parseGraphSON $ lookupOneValue k pm
  where
    err_msg = notExistErrorMsg k

-- | Lookup a list of property values from a 'PropertyMap' by the
-- given key, and parse them.
-- 
-- In version 0.1.1.0 and before, this function took an argument @m p (GraphSON Value)@.
-- This has changed, because property types for 'AVertex' etc have changed.
parseListValues :: (PropertyMap m, Property p, FromGraphSON v) => Text -> m p GValue -> Parser [v]
parseListValues k pm = mapM parseGraphSON $ lookupListValues k pm

-- | Like 'parseListValues', but this function 'fail's when there is
-- no property with the given key.
--
-- In version 0.1.1.0 and before, this function took an argument @m p (GraphSON Value)@.
-- This has changed, because property types for 'AVertex' etc have changed.
parseNonEmptyValues :: (PropertyMap m, Property p, FromGraphSON v) => Text -> m p GValue -> Parser (NonEmpty v)
parseNonEmptyValues k pm = toNonEmpty =<< parseListValues k pm
  where
    toNonEmpty [] = fail $ notExistErrorMsg k
    toNonEmpty (x : rest) = return (x :| rest)

-- | Create a 'PropertyMap' from list of 'Property's.
fromProperties :: (PropertyMap m, Property p, Monoid (m p v))
               => [p v]
               -> m p v
fromProperties = foldr putProperty mempty

-- | Generic implementation of 'PropertyMap'. @t@ is the type of
-- cardinality, @p@ is the type of 'Property' class and @v@ is the
-- type of the property value.
newtype PropertyMapGeneric t p v = PropertyMapGeneric (HM.HashMap Text (t (p v)))
                                 deriving (Show,Eq)

instance Semigroup (t (p v)) => Semigroup (PropertyMapGeneric t p v) where
  (PropertyMapGeneric a) <> (PropertyMapGeneric b) = PropertyMapGeneric $ HM.unionWith (<>) a b

instance Semigroup (t (p v)) => Monoid (PropertyMapGeneric t p v) where
  mempty = PropertyMapGeneric mempty
  mappend = (<>)

instance (Functor t, Functor p) => Functor (PropertyMapGeneric t p) where
  fmap f (PropertyMapGeneric hm) = PropertyMapGeneric $ (fmap . fmap . fmap) f hm

instance (Foldable t, Foldable p) => Foldable (PropertyMapGeneric t p) where
  foldr f start (PropertyMapGeneric hm) = foldr f2 start hm
    where
      f2 t start2 = foldr f3 start2 t
      f3 p start3 = foldr f start3 p

instance (Traversable t, Traversable p) => Traversable (PropertyMapGeneric t p) where
  traverse f (PropertyMapGeneric hm) = fmap PropertyMapGeneric $ (traverse . traverse . traverse) f hm

putPropertyGeneric :: (Semigroup (t (p v)), Applicative t, Property p) => p v -> PropertyMapGeneric t p v -> PropertyMapGeneric t p v
putPropertyGeneric prop (PropertyMapGeneric hm) =
  PropertyMapGeneric $ HM.insertWith (<>) (propertyKey prop) (pure prop) hm

removePropertyGeneric :: Text -> PropertyMapGeneric t p v -> PropertyMapGeneric t p v
removePropertyGeneric k (PropertyMapGeneric hm) = PropertyMapGeneric $ HM.delete k hm

allPropertiesGeneric :: Foldable t => PropertyMapGeneric t p v -> [p v]
allPropertiesGeneric (PropertyMapGeneric hm) = concat $ map toList $ HM.elems hm

parsePropertiesGeneric :: (Property p, PropertyMap m, Monoid (m p v), GraphSONTyped (p v), FromGraphSON (p v), FromGraphSONWithKey (p v))
                       => (GValue -> Parser (Vector GValue))
                       -> GValue
                       -> Parser (m p v)
parsePropertiesGeneric normalizeCardinality gv = case gValueBody gv of
  GObject obj -> foldlM folder mempty $ KM.toList obj
  _ -> empty
  where
    folder pm (k, value) = fmap (foldr putProperty pm) $ traverse (parseProperty k) =<< normalizeCardinality value
    parseProperty k value = parseTypedGValue value <|> parseGraphSONWithKey (Key.toText k) value

-- parhaps we might as well place it in GraphSON module and let it export.
parseTypedGValue :: (GraphSONTyped v, FromGraphSON v) => GValue -> Parser v
parseTypedGValue  gv = do
  prop <- parseGraphSON gv
  let exp_type = gsonTypeFor prop
      mgot_type = gValueType gv
  if mgot_type /= Just exp_type
    then fail ("Expected @type field of " ++ unpack exp_type ++ ", but got " ++ show mgot_type)
    else return prop

expectAesonArray :: GValue -> Parser (Vector GValue)
expectAesonArray gv = case gValueBody gv of
  GArray a -> return a
  _ -> empty

-- | A 'PropertyMap' that has a single value per key.
--
-- 'putProperty' replaces the old property by the given property.
--
-- '<>' returns the union of the two given property maps. If the two
-- property maps share some same keys, the value from the left map
-- wins.
newtype PropertyMapSingle p v = PropertyMapSingle (PropertyMapGeneric Semigroup.First p v)
                              deriving (Show,Eq,Semigroup,Monoid,Functor,Foldable,Traversable)

instance PropertyMap PropertyMapSingle where
  lookupOne k (PropertyMapSingle (PropertyMapGeneric hm)) = fmap Semigroup.getFirst $ HM.lookup k hm
  lookupList k m = maybe [] return $ lookupOne k m
  putProperty p (PropertyMapSingle pg) = PropertyMapSingle $ putPropertyGeneric p pg
  removeProperty t (PropertyMapSingle pg) = PropertyMapSingle $ removePropertyGeneric t pg
  allProperties (PropertyMapSingle pg) = allPropertiesGeneric pg

-- | In version 0.1.1.0 and before, the constraint was @FromJSON v@.
-- This has changed.
instance (Property p, GraphSONTyped (p v), FromGraphSON (p v), FromGraphSONWithKey (p v))
         => FromJSON (PropertyMapSingle p v) where
  parseJSON = parseJSONViaGValue

instance (Property p, GraphSONTyped (p v), FromGraphSON (p v), FromGraphSONWithKey (p v))
         => FromGraphSON (PropertyMapSingle p v) where
  parseGraphSON = parsePropertiesGeneric (return . return)

-- | A 'PropertyMap' that can keep more than one values per key.
--
-- 'lookupOne' returns the first property associated with the given
-- key.
--
-- 'putProperty' prepends the given property to the property list.
--
-- '<>' returns the union of the two given property maps. If the two
-- property maps share some same keys, those property lists are
-- concatenated.
newtype PropertyMapList p v = PropertyMapList (PropertyMapGeneric NonEmpty p v)
                            deriving (Show,Eq,Semigroup,Monoid,Functor,Foldable,Traversable)

instance PropertyMap PropertyMapList where
  lookupList k (PropertyMapList (PropertyMapGeneric hm)) = maybe [] NL.toList $ HM.lookup k hm
  putProperty p (PropertyMapList pg) = PropertyMapList $ putPropertyGeneric p pg
  removeProperty t (PropertyMapList pg) = PropertyMapList $ removePropertyGeneric t pg
  allProperties (PropertyMapList pg) = allPropertiesGeneric pg

-- | In version 0.1.1.0 and before, the constraint was @FromJSON v@.
-- This has changed.
instance (Property p, GraphSONTyped (p v), FromGraphSON (p v), FromGraphSONWithKey (p v))
         => FromJSON (PropertyMapList p v) where
  parseJSON = parseJSONViaGValue

instance (Property p, GraphSONTyped (p v), FromGraphSON (p v), FromGraphSONWithKey (p v))
         => FromGraphSON (PropertyMapList p v) where
  parseGraphSON v = parsePropertiesGeneric expectAesonArray v

-- | __This typeclass is for internal use.__
--
-- GraphSON parser with a property key given from outside.
--
-- @since 0.2.0.0
class FromGraphSONWithKey a where
  parseGraphSONWithKey :: Text -> GValue -> Parser a

instance FromGraphSON v => FromGraphSONWithKey (AProperty v) where
  parseGraphSONWithKey k v = AProperty k <$> parseGraphSON v

instance FromGraphSON v => FromGraphSONWithKey (AVertexProperty v) where
  parseGraphSONWithKey k gv = case gValueBody gv of
    GObject o -> AVertexProperty
                 <$> (o .: "id")
                 <*> pure k
                 <*> (o .: "value")
    _ -> empty
