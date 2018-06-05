{-# LANGUAGE TypeFamilies, OverloadedStrings, FlexibleInstances, GeneralizedNewtypeDeriving, DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- |
-- Module: Data.Greskell.Graph
-- Description: Haskell counterpart of Gremlin graph structure data types
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This module defines types and functions about TinkerPop graph
-- structure API.
module Data.Greskell.Graph
       ( -- * TinkerPop graph structure types
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
         -- * Cardinality Enum
         Cardinality,
         cList,
         cSet,
         cSingle,
         -- * Typed Key (accessor of a Property)
         Key(..),
         key,
         -- * Concrete data types
         --
         -- $concrete_types
         --
         -- ** Vertex
         AVertex(..),
         -- ** Edge
         AEdge(..),
         -- ** VertexProperty
         AVertexProperty(..),
         -- ** Property
         AProperty(..),
         -- ** PropertyMap
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
         FromGraphSONWithKey
       ) where

import Control.Applicative (empty, (<$>), (<*>), (<|>))
import Data.Aeson (Value(..), FromJSON(..))
import Data.Aeson.Types (Parser)
import Data.Foldable (toList, Foldable(foldr), foldlM)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NL
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid)
import Data.Semigroup ((<>), Semigroup)
import qualified Data.Semigroup as Semigroup
import Data.String (IsString(..))
import Data.Text (Text, unpack)
import Data.Traversable (Traversable(traverse))
import Data.Vector (Vector)

import Data.Greskell.GraphSON
  ( GraphSON(..), GraphSONTyped(..), FromGraphSON(..),
    (.:), GValue, GValueBody(..), gValueBody, gValueType
  )
import Data.Greskell.Greskell
  ( Greskell, unsafeGreskellLazy, string,
    ToGreskell(..)
  )

-- | @org.apache.tinkerpop.gremlin.structure.Element@ interface in a
-- TinkerPop graph.
class Element e where
  type ElementID e
  -- ^ ID type of the 'Element'. This depends on graph database
  -- implementation and its settings.
  type ElementProperty e :: * -> *
  -- ^ Property type of the 'Element'. It should be of 'Property'
  -- class. If you don't care, use 'AVertexProperty' if type @e@ is an
  -- 'Vertex' and use 'AProperty' if type @e@ is an 'Edge' or
  -- VertexProperty.

-- | @org.apache.tinkerpop.gremlin.structure.Vertex@ interface in a
-- TinkerPop graph.
class (Element v) => Vertex v

-- | @org.apache.tinkerpop.gremlin.structure.Edge@ interface in a
-- TinkerPop graph.
class (Element e) => Edge e where
  type EdgeVertexID e
  -- ^ ID type of the 'Vertex' this edge connects.


-- | @org.apache.tinkerpop.gremlin.structure.Property@ interface in a
-- TinkerPop graph.
class Property p where
  propertyKey :: p v -> Text
  -- ^ Get key of this property.
  propertyValue :: p v -> v
  -- ^ Get value of this property.

-- | @org.apache.tinkerpop.gremlin.structure.T@ enum.
--
-- 'T' is a token to get data @b@ from an Element @a@.
data T a b

instance GraphSONTyped (T a b) where
  gsonTypeFor _ = "g:T"


-- | @T.id@ token.
tId :: Element a => Greskell (T a (ElementID a))
tId = unsafeGreskellLazy "T.id"

-- | @T.key@ token.
tKey :: (Element (p v), Property p) => Greskell (T (p v) Text)
tKey = unsafeGreskellLazy "T.key"

-- | @T.label@ token.
tLabel :: Element a => Greskell (T a Text)
tLabel = unsafeGreskellLazy "T.label"

-- | @T.value@ token.
tValue :: (Element (p v), Property p) => Greskell (T (p v) v)
tValue = unsafeGreskellLazy "T.value"

-- | @org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality@ enum.
data Cardinality

-- Developer note: while 'tId' creates a Greskell of "T.id", 'cList'
-- creates just "list", not "VertexProperty.Cardinality.list". This is
-- because Neptune (Amazon's cloud-based graph database) happens to
-- support "list" but not "VertexProperty.Cardinality.list" (it
-- supports "T.id", though.)
-- See https://docs.aws.amazon.com/neptune/latest/userguide/access-graph-gremlin-differences.html
--
-- Future versions of greskell may support some configuration
-- mechanism to control how to format Gremlin symbols such as those in
-- Cardinality, T, Order, P, Direction etc.

-- | @list@ Cardinality.
--
-- >>> toGremlin cList
-- "list"
cList :: Greskell Cardinality
cList = unsafeGreskellLazy "list"

-- | @set@ Cardinality.
cSet :: Greskell Cardinality
cSet = unsafeGreskellLazy "set"

-- | @single@ Cardinality.
cSingle :: Greskell Cardinality
cSingle = unsafeGreskellLazy "single"

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

-- | Create a 'Key' from a literal string.
key :: Text -> Key a b
key = Key . string

-- $concrete_types
--
-- Concrete data types based on aeson values.
--
-- Element IDs and property values are all 'GValue', because they are
-- highly polymorphic. The 'GValue' type allows you to inspect
-- 'gsonType' field if necessary. 'ElementID' and 'EdgeVertexID' are
-- 'GValue', too.
--
-- As for properties, you can use 'PropertyMap' and other type-classes
-- to manipulate them.
--
-- If you want to define your own graph structure types, see
-- [README.md](https://github.com/debug-ito/greskell#make-your-own-graph-structure-types)
-- for detail.


-- | General vertex type you can use for 'Vertex' class, based on
-- aeson data types.
data AVertex =
  AVertex
  { avId :: GValue,
    -- ^ ID of this vertex
    avLabel :: Text,
    -- ^ Label of this vertex
    avProperties :: PropertyMapList AVertexProperty GValue
    -- ^ Properties of this vertex.
  }
  deriving (Show,Eq)

instance Element AVertex where
  type ElementID AVertex = GValue
  type ElementProperty AVertex = AVertexProperty

instance Vertex AVertex

instance GraphSONTyped AVertex where
  gsonTypeFor _ = "g:Vertex"

instance FromJSON AVertex where
  parseJSON v = parseGraphSON =<< parseJSON v

instance FromGraphSON AVertex where
  parseGraphSON gv = case gValueBody gv of
    GObject o -> AVertex
                 <$> (o .: "id")
                 <*> (o .: "label")
                 <*> (o `optionalMonoid` "properties")
    _ -> empty

-- | General edge type you can use for 'Edge' class, based on aeson
-- data types.
data AEdge =
  AEdge
  { aeId :: GValue,
    -- ^ ID of this edge.
    aeLabel :: Text,
    -- ^ Label of this edge.
    aeInVLabel :: Text,
    -- ^ Label of this edge's destination vertex.
    aeOutVLabel :: Text,
    -- ^ Label of this edge's source vertex.
    aeInV :: GValue,
    -- ^ ID of this edge's destination vertex.
    aeOutV :: GValue,
    -- ^ ID of this edge's source vertex.
    aeProperties :: PropertyMapSingle AProperty GValue
    -- ^ Properties of this edge.
  }
  deriving (Show,Eq)

instance Element AEdge where
  type ElementID AEdge = GValue
  type ElementProperty AEdge = AProperty

instance Edge AEdge where
  type EdgeVertexID AEdge = GValue

instance GraphSONTyped AEdge where
  gsonTypeFor _ = "g:Edge"

instance FromJSON AEdge where
  parseJSON v = parseGraphSON =<< parseJSON v

instance FromGraphSON AEdge where
  parseGraphSON gv = case gValueBody gv of
    GObject o -> AEdge
                 <$> (o .: "id")
                 <*> (o .: "label")
                 <*> (o .: "inVLabel")
                 <*> (o .: "outVLabel")
                 <*> (o .: "inV")
                 <*> (o .: "outV")
                 <*> (o `optionalMonoid` "properties")
    _ -> empty

optionalMonoid :: (Monoid m, FromGraphSON m) => HM.HashMap Text GValue -> Text -> Parser m
optionalMonoid obj field_name = maybe (return mempty) parseGraphSON $ nullToNothing =<< HM.lookup field_name obj
  where
    nullToNothing gv = case gValueBody gv of
      GNull -> Nothing
      _ -> Just gv

-- | __This typeclass is for internal use.__
--
-- GraphSON parser with a property key given from outside.
class FromGraphSONWithKey a where
  parseGraphSONWithKey :: Text -> GValue -> Parser a


-- | General simple property type you can use for 'Property' class.
--
-- If you are not sure about the type @v@, just use 'GValue'.
data AProperty v =
  AProperty
  { apKey :: Text,
    apValue :: v
  }
  deriving (Show,Eq,Ord)

-- | Parse Property of GraphSON 1.0.
--
-- In version 0.1.1.0 and before, the constraint was @FromJSON v@.
-- This has changed.
instance FromGraphSON v => FromJSON (AProperty v) where
  parseJSON v = parseGraphSON =<< parseJSON v

-- | Parse Property of GraphSON 1.0.
instance FromGraphSON v => FromGraphSON (AProperty v) where
  parseGraphSON gv = case gValueBody gv of
    GObject o -> AProperty <$> (o .: "key") <*> (o .: "value")
    _ -> empty

instance FromGraphSON v => FromGraphSONWithKey (AProperty v) where
  parseGraphSONWithKey k v = AProperty k <$> parseGraphSON v

instance Property AProperty where
  propertyKey = apKey
  propertyValue = apValue

instance GraphSONTyped (AProperty v) where
  gsonTypeFor _ = "g:Property"

instance Functor AProperty where
  fmap f sp = sp { apValue = f $ apValue sp }

instance Foldable AProperty where
  foldr f start sp = f (apValue sp) start

instance Traversable AProperty where
  traverse f sp = fmap (\v -> sp { apValue = v } ) $ f $ apValue sp

-- | General vertex property type you can use for VertexProperty,
-- based on aeson data types.
--
-- If you are not sure about the type @v@, just use 'GValue'.
data AVertexProperty v =
  AVertexProperty
  { avpId :: GValue,
    -- ^ ID of this vertex property.
    avpLabel :: Text,
    -- ^ Label and key of this vertex property.
    avpValue :: v,
    -- ^ Value of this vertex property.
    avpProperties :: PropertyMapSingle AProperty GValue
    -- ^ (meta)properties of this vertex property.
  }
  deriving (Show,Eq)

-- | In version 0.1.1.0 and before, the constraint was @FromJSON v@.
-- This has changed.
instance FromGraphSON v => FromJSON (AVertexProperty v) where
  parseJSON v = parseGraphSON =<< parseJSON v

instance FromGraphSON v => FromGraphSON (AVertexProperty v) where
  parseGraphSON gv = case gValueBody gv of
    GObject o -> do
      label <- o .: "label"
      parseGraphSONWithKey label gv
    _ -> empty

instance FromGraphSON v => FromGraphSONWithKey (AVertexProperty v) where
  parseGraphSONWithKey k gv = case gValueBody gv of
    GObject o -> AVertexProperty
                 <$> (o .: "id")
                 <*> pure k
                 <*> (o .: "value")
                 <*> (o `optionalMonoid` "properties")
    _ -> empty

instance GraphSONTyped (AVertexProperty v) where
  gsonTypeFor _ = "g:VertexProperty"

instance Element (AVertexProperty v) where
  type ElementID (AVertexProperty v) = GValue
  type ElementProperty (AVertexProperty v) = AProperty

instance Property AVertexProperty where
  propertyKey = avpLabel
  propertyValue = avpValue

instance Functor AVertexProperty where
  fmap f vp = vp { avpValue = f $ avpValue vp }

instance Foldable AVertexProperty where
  foldr f start vp = f (avpValue vp) start

instance Traversable AVertexProperty where
  traverse f vp = fmap (\v -> vp { avpValue = v }) $ f $ avpValue vp


-- -- We could define the following constraint synonym with
-- -- ConstraintKinds extension, although its semantics is not exactly
-- -- correct..
-- type VertexProperty p v = (Element (p v), Property p)


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
  GObject obj -> foldlM folder mempty $ HM.toList obj
  _ -> empty
  where
    folder pm (k, value) = fmap (foldr putProperty pm) $ traverse (parseProperty k) =<< normalizeCardinality value
    parseProperty k value = parseTypedGValue value <|> parseGraphSONWithKey k value

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
  parseJSON v = parseGraphSON =<< parseJSON v

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
  parseJSON v = parseGraphSON =<< parseJSON v

instance (Property p, GraphSONTyped (p v), FromGraphSON (p v), FromGraphSONWithKey (p v))
         => FromGraphSON (PropertyMapList p v) where
  parseGraphSON v = parsePropertiesGeneric expectAesonArray v

