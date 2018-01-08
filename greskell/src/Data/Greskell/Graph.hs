{-# LANGUAGE TypeFamilies, OverloadedStrings, FlexibleInstances, GeneralizedNewtypeDeriving, DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- |
-- Module: Data.Greskell.Graph
-- Description: Haskell counterpart of Gremlin graph structure data types.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.Graph
       ( -- * TinkerPop graph structure API
         Element(..),
         Vertex,
         Edge(..),
         Property(..),
         -- * T Enum
         T,
         tId,
         tKey,
         tLabel,
         tValue,
         -- * Extended API
         Key(..),
         -- * Concrete data types
         --
         -- $concrete_types
         --
         -- ** Vertex
         AesonVertex(..),
         -- ** Edge
         AesonEdge(..),
         -- ** VertexProperty
         AesonVertexProperty(..),
         -- ** Property
         SimpleProperty(..),
         -- ** PropertyMap
         PropertyMap(..),
         lookupOneValue,
         lookupListValues,
         PropertyMapSingle,
         PropertyMapList,
         -- * Parser support
         FromJSONWithKey
       ) where

import Control.Applicative (empty, (<$>), (<*>), (<|>))
import Data.Aeson (Value(..), FromJSON(..), (.:), (.:?), Object)
import Data.Aeson.Types (Parser)
import Data.Foldable (toList, Foldable(foldr), foldlM)
import qualified Data.HashMap.Lazy as HM
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NL
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid)
import Data.Semigroup ((<>), Semigroup)
import qualified Data.Semigroup as Semigroup
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Traversable (Traversable(traverse))

import Data.Greskell.GraphSON (GraphSON(..), GraphSONTyped(..), parseTypedGraphSON)
import Data.Greskell.Greskell
  ( Greskell, unsafeGreskellLazy, string,
    ToGreskell(..)
  )

-- | @Element@ interface in a TinkerPop graph.
class Element e where
  type ElementID e
  -- ^ ID type of the 'Element'
  type ElementProperty e :: * -> *
  -- ^ Property type of the 'Element'. It should be of 'Property'
  -- class.
  elementId :: e -> ElementID e
  elementLabel :: e -> Text

-- | @Vertex@ interface in a TinkerPop graph.
class (Element v) => Vertex v

-- | @Edge@ interface in a TinkerPop graph.
class (Element e) => Edge e where
  type EdgeVertexID e
  -- ^ ID type of the 'Vertex' this edge connects.
  edgeInVertexID :: e -> EdgeVertexID e
  -- ^ ID of this edge's destination (target) Vertex.
  edgeOutVertexID :: e -> EdgeVertexID e
  -- ^ ID of this edge's source Vertex.

-- | @Property@ interface in a TinkerPop graph.
class Property p where
  propertyKey :: p v -> Text
  propertyValue :: p v -> v

-- | @org.apache.tinkerpop.gremlin.structure.T@ enum.
--
-- 'T' is a token to get data @b@ from an Element @a@.
data T a b

instance GraphSONTyped (T a b) where
  gsonTypeFor _ = "g:T"

-- | @T.id@ token.
tId :: Element a => Greskell (T a (ElementID a))
tId = unsafeGreskellLazy "id"

-- | @T.key@ token.
tKey :: (Element (p v), Property p) => Greskell (T (p v) Text)
tKey = unsafeGreskellLazy "key"

-- | @T.label@ token.
tLabel :: Element a => Greskell (T a Text)
tLabel = unsafeGreskellLazy "label"

-- | @T.value@ token.
tValue :: (Element (p v), Property p) => Greskell (T (p v) v)
tValue = unsafeGreskellLazy "value"


-- | A property key accessing value @b@ in an Element @a@. In Gremlin,
-- it's just a String type.
newtype Key a b = Key { unKey :: Greskell Text }
                deriving (Show,Eq)

-- | Unsafely convert the value type @b@.
instance Functor (Key a) where
  fmap _ (Key t) = Key t

-- | Gremlin String literal as a 'Key'.
instance IsString (Key a b) where
  fromString = Key . fromString

-- | Unwrap 'Key' constructor.
instance ToGreskell (Key a b) where
  type GreskellReturn (Key a b) = Text
  toGreskell = unKey


-- $concrete_types
--
-- Concrete data types based on aeson 'Value's.
--
-- Element IDs and property values are all 'Value', because they are
-- highly polymorphic. They are wrapped with 'GraphSON', so that you
-- can inspect 'gsonType' field if present. 'ElementID' and
-- 'EdgeVertexID' are bare 'Value' type for convenience.
--
-- As for properties, you can use 'PropertyMap' and other type-classes
-- to manipulate them.


-- | General vertex type you can use for 'Vertex' class, based on
-- aeson data types.
data AesonVertex =
  AesonVertex
  { avId :: GraphSON Value,
    -- ^ ID of this vertex
    avLabel :: Text,
    -- ^ Label of this vertex
    avProperties :: PropertyMapList AesonVertexProperty (GraphSON Value)
    -- ^ Properties of this vertex.
  }
  deriving (Show,Eq)

instance Element AesonVertex where
  type ElementID AesonVertex = Value
  type ElementProperty AesonVertex = AesonVertexProperty
  elementId = gsonValue . avId
  elementLabel = avLabel

instance Vertex AesonVertex

instance GraphSONTyped AesonVertex where
  gsonTypeFor _ = "g:Vertex"

instance FromJSON AesonVertex where
  parseJSON (Object o) = AesonVertex
                         <$> (o .: "id")
                         <*> (o .: "label")
                         <*> (o `optionalMonoid` "properties")
  parseJSON _ = empty

-- | General edge type you can use for 'Edge' class, based on aeson
-- data types.
data AesonEdge =
  AesonEdge
  { aeId :: GraphSON Value,
    -- ^ ID of this edge.
    aeLabel :: Text,
    -- ^ Label of this edge.
    aeInVLabel :: Text,
    -- ^ Label of this edge's destination vertex.
    aeOutVLabel :: Text,
    -- ^ Label of this edge's source vertex.
    aeInV :: GraphSON Value,
    -- ^ ID of this edge's destination vertex.
    aeOutV :: GraphSON Value,
    -- ^ ID of this edge's source vertex.
    aeProperties :: PropertyMapSingle SimpleProperty (GraphSON Value)
    -- ^ Properties of this edge.
  }
  deriving (Show,Eq)

instance Element AesonEdge where
  type ElementID AesonEdge = Value
  type ElementProperty AesonEdge = SimpleProperty
  elementId = gsonValue . aeId
  elementLabel = aeLabel

instance Edge AesonEdge where
  type EdgeVertexID AesonEdge = Value
  edgeInVertexID = gsonValue . aeInV
  edgeOutVertexID = gsonValue . aeOutV

instance GraphSONTyped AesonEdge where
  gsonTypeFor _ = "g:Edge"

instance FromJSON AesonEdge where
  parseJSON (Object o) =
    AesonEdge
    <$> (o .: "id")
    <*> (o .: "label")
    <*> (o .: "inVLabel")
    <*> (o .: "outVLabel")
    <*> (o .: "inV")
    <*> (o .: "outV")
    <*> (o `optionalMonoid` "properties")
  parseJSON _ = empty

optionalMonoid :: (Monoid m, FromJSON m) => Object -> Text -> Parser m
optionalMonoid obj field_name = fmap (maybe mempty id) (obj .:? field_name)

-- | __This typeclass is for internal use.__
--
-- JSON parser with a property key given from outside.
class FromJSONWithKey a where
  parseJSONWithKey :: Text -> Value -> Parser a


-- | General simple property type you can use for 'Property' class.
data SimpleProperty v =
  SimpleProperty
  { sPropertyKey :: Text,
    sPropertyValue :: v
  }
  deriving (Show,Eq,Ord)

-- | Parse Property of GraphSON 1.0.
instance FromJSON v => FromJSON (SimpleProperty v) where
  parseJSON (Object o) =
    SimpleProperty <$> (o .: "key") <*> (o .: "value")
  parseJSON _ = empty

instance FromJSON v => FromJSONWithKey (SimpleProperty v) where
  parseJSONWithKey key v = SimpleProperty key <$> parseJSON v

instance Property SimpleProperty where
  propertyKey = sPropertyKey
  propertyValue = sPropertyValue

instance GraphSONTyped (SimpleProperty v) where
  gsonTypeFor _ = "g:Property"

instance Functor SimpleProperty where
  fmap f sp = sp { sPropertyValue = f $ sPropertyValue sp }

instance Foldable SimpleProperty where
  foldr f start sp = f (sPropertyValue sp) start

instance Traversable SimpleProperty where
  traverse f sp = fmap (\v -> sp { sPropertyValue = v } ) $ f $ sPropertyValue sp

-- | General vertex property type you can use for VertexProperty,
-- based on aeson data types.
data AesonVertexProperty v =
  AesonVertexProperty
  { avpId :: GraphSON Value,
    -- ^ ID of this vertex property.
    avpLabel :: Text,
    -- ^ Label and key of this vertex property.
    avpValue :: v,
    -- ^ Value of this vertex property.
    avpProperties :: PropertyMapSingle SimpleProperty (GraphSON Value)
    -- ^ (meta)properties of this vertex property.
  }
  deriving (Show,Eq)

instance FromJSON v => FromJSON (AesonVertexProperty v) where
  parseJSON v@(Object o) = do
    label <- o .: "label"
    parseJSONWithKey label v
  parseJSON _ = empty

instance FromJSON v => FromJSONWithKey (AesonVertexProperty v) where
  parseJSONWithKey key (Object o) = AesonVertexProperty
                                    <$> (o .: "id")
                                    <*> pure key
                                    <*> (o .: "value")
                                    <*> (o `optionalMonoid` "properties")
  parseJSONWithKey _ _ = empty


instance GraphSONTyped (AesonVertexProperty v) where
  gsonTypeFor _ = "g:VertexProperty"

instance Element (AesonVertexProperty v) where
  type ElementID (AesonVertexProperty v) = Value
  type ElementProperty (AesonVertexProperty v) = SimpleProperty
  elementId = gsonValue . avpId
  elementLabel = avpLabel

instance Property AesonVertexProperty where
  propertyKey = avpLabel
  propertyValue = avpValue

instance Functor AesonVertexProperty where
  fmap f vp = vp { avpValue = f $ avpValue vp }

instance Foldable AesonVertexProperty where
  foldr f start vp = f (avpValue vp) start

instance Traversable AesonVertexProperty where
  traverse f vp = fmap (\v -> vp { avpValue = v }) $ f $ avpValue vp


-- -- We could define the following constraint synonym with
-- -- ConstraintKinds extension, although its semantics is not exactly
-- -- correct..
-- type VertexProperty p v = (Element (p v), Property p)


-- | Common basic operations supported by maps of properties.
class PropertyMap m where
  lookupOne :: Text -> m p v -> Maybe (p v)
  -- ^ Look up a property associated with the given key.
  lookupOne key m = listToMaybe $ lookupList key m
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
lookupOneValue key = fmap propertyValue . lookupOne key

-- | Lookup property values from a 'PropertyMap' by key.
lookupListValues :: (PropertyMap m, Property p) => Text -> m p v -> [v]
lookupListValues key = fmap propertyValue . lookupList key


-- | Generic implementation of 'PropertyMap'. @t@ is the type of
-- cardinality, @p@ is the type of 'Property' class and @v@ is the
-- type of the property value.
newtype PropertyMapGeneric t p v = PropertyMapGeneric (HM.HashMap Text (t (p v)))
                                 deriving (Show,Eq)

instance Semigroup (t (p v)) => Monoid (PropertyMapGeneric t p v) where
  mempty = PropertyMapGeneric mempty
  mappend (PropertyMapGeneric a) (PropertyMapGeneric b) =
    PropertyMapGeneric $ HM.unionWith (<>) a b

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
removePropertyGeneric key (PropertyMapGeneric hm) = PropertyMapGeneric $ HM.delete key hm

allPropertiesGeneric :: Foldable t => PropertyMapGeneric t p v -> [p v]
allPropertiesGeneric (PropertyMapGeneric hm) = concat $ map toList $ HM.elems hm

parsePropertiesGeneric :: (Property p, PropertyMap m, Monoid (m p v), GraphSONTyped (p v), FromJSON (p v), FromJSONWithKey (p v))
                       => (Value -> Parser [Value])
                       -> Value
                       -> Parser (m p v)
parsePropertiesGeneric normalizeCardinality (Object obj) = foldlM folder mempty $ HM.toList obj
  where
    folder pm (key, value) = fmap (foldr putProperty pm) $ traverse (parseProperty key) =<< normalizeCardinality value
    parseProperty key value = (fmap gsonValue $ parseTypedGraphSON value) <|> parseJSONWithKey key value
parsePropertiesGeneric _ _ = empty

expectAesonArray :: Value -> Parser [Value]
expectAesonArray (Array a) = return $ toList a
expectAesonArray _ = empty

-- | A 'PropertyMap' that has a single value per key.
--
-- 'putProperty' replaces the old property by the given property.
--
-- '<>' returns the union of the two given property maps. If the two
-- property maps share some same keys, the value from the left map
-- wins.
newtype PropertyMapSingle p v = PropertyMapSingle (PropertyMapGeneric Semigroup.First p v)
                              deriving (Show,Eq,Monoid,Functor,Foldable,Traversable)

instance PropertyMap PropertyMapSingle where
  lookupOne key (PropertyMapSingle (PropertyMapGeneric hm)) = fmap Semigroup.getFirst $ HM.lookup key hm
  lookupList key m = maybe [] return $ lookupOne key m
  putProperty p (PropertyMapSingle pg) = PropertyMapSingle $ putPropertyGeneric p pg
  removeProperty t (PropertyMapSingle pg) = PropertyMapSingle $ removePropertyGeneric t pg
  allProperties (PropertyMapSingle pg) = allPropertiesGeneric pg

instance (Property p, GraphSONTyped (p v), FromJSON (p v), FromJSONWithKey (p v))
         => FromJSON (PropertyMapSingle p v) where
  parseJSON = parsePropertiesGeneric (return . return)

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
                            deriving (Show,Eq,Monoid,Functor,Foldable,Traversable)

instance PropertyMap PropertyMapList where
  lookupList key (PropertyMapList (PropertyMapGeneric hm)) = maybe [] NL.toList $ HM.lookup key hm
  putProperty p (PropertyMapList pg) = PropertyMapList $ putPropertyGeneric p pg
  removeProperty t (PropertyMapList pg) = PropertyMapList $ removePropertyGeneric t pg
  allProperties (PropertyMapList pg) = allPropertiesGeneric pg

instance (Property p, GraphSONTyped (p v), FromJSON (p v), FromJSONWithKey (p v))
         => FromJSON (PropertyMapList p v) where
  parseJSON v = parsePropertiesGeneric expectAesonArray v
