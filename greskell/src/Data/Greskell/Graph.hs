{-# LANGUAGE TypeFamilies, OverloadedStrings, FlexibleInstances, GeneralizedNewtypeDeriving, DeriveTraversable, GADTs, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- |
-- Module: Data.Greskell.Graph
-- Description: Haskell counterpart of Gremlin graph structure data types
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This module defines types and functions about TinkerPop graph
-- structure API.
module Data.Greskell.Graph
       ( -- * Element
         Element(..),
         ElementData(..),
         ElementID(..),
         unsafeCastElementID,
         -- * Property
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
         unsafeCastKey,
         -- ** key-value pair
         KeyValue(..),
         (=:),
         -- * Concrete data types
         -- $concrete_types
         
         -- ** Vertex
         AVertex(..),
         -- ** Edge
         AEdge(..),
         -- ** VertexProperty
         AVertexProperty(..),
         -- ** Property
         AProperty(..)
       ) where

import Control.Applicative (empty, (<$>), (<*>), (<|>))
import Data.Aeson (Value(..), FromJSON(..), ToJSON(..))
import Data.Aeson.Types (Parser)
import Data.Foldable (toList, Foldable(foldr), foldlM)
import Data.Hashable (Hashable)
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
import GHC.Generics (Generic)

import Data.Greskell.GraphSON
  ( GraphSON(..), GraphSONTyped(..), FromGraphSON(..),
    (.:), GValue, GValueBody(..),
    parseJSONViaGValue
  )
import Data.Greskell.GraphSON.GValue (gValueBody, gValueType)
import Data.Greskell.Greskell
  ( Greskell, unsafeGreskellLazy, string,
    ToGreskell(..)
  )

-- $setup
--
-- >>> import Data.Greskell.Greskell (toGremlin)

-- | ID of a graph element @e@ (vertex, edge and vertex
-- property). Data structure of an 'ElementID' depends on graph
-- implementation, so you should not rely on it.
newtype ElementID e = ElementID { unElementID :: GValue }
                    deriving (Show,Eq,Generic, ToJSON, FromJSON, FromGraphSON, Hashable)

-- | Unsafely convert the element type.
instance Functor ElementID where
  fmap _ e = unsafeCastElementID e

-- | Unsafely cast the phantom type of 'ElementID'.
unsafeCastElementID :: ElementID a -> ElementID b
unsafeCastElementID (ElementID e) = ElementID e

-- | Types that keep reference to TinkerPop graph Elements.
class ElementData e where
  -- | ID of this Element.
  elementId :: e -> ElementID e
  -- | Label of this Element.
  elementLabel :: e -> Text

-- | @org.apache.tinkerpop.gremlin.structure.Element@ interface in a
-- TinkerPop graph.
class ElementData e => Element e where
  -- | Property type of the 'Element'. It should be of 'Property'
  -- class.
  type ElementProperty e :: * -> *

-- | @org.apache.tinkerpop.gremlin.structure.Property@ interface in a
-- TinkerPop graph.
class Property p where
  -- | Get key of this property.
  propertyKey :: p v -> Text
  -- | Get value of this property.
  propertyValue :: p v -> v

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
tKey :: Greskell (T (AVertexProperty v) Text)
tKey = unsafeGreskellLazy "T.key"

-- | @T.label@ token.
tLabel :: Element a => Greskell (T a Text)
tLabel = unsafeGreskellLazy "T.label"

-- | @T.value@ token.
tValue :: Greskell (T (AVertexProperty v) v)
tValue = unsafeGreskellLazy "T.value"

-- | @org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality@ enum.
--
-- @since 0.2.0.0
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
--
-- @since 0.2.0.0
cList :: Greskell Cardinality
cList = unsafeGreskellLazy "list"

-- | @set@ Cardinality.
--
-- @since 0.2.0.0
cSet :: Greskell Cardinality
cSet = unsafeGreskellLazy "set"

-- | @single@ Cardinality.
--
-- @since 0.2.0.0
cSingle :: Greskell Cardinality
cSingle = unsafeGreskellLazy "single"

-- | A property key accessing value @b@ in an Element @a@. In Gremlin,
-- it's just a String type.
--
-- >>> toGremlin ("age" :: Key AVertex Int)
-- "\"age\""
-- >>> toGremlin (key "created_at" :: Key AEdge Text)
-- "\"created_at\""
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

-- | Unsafely cast the type signature of the 'Key'.
unsafeCastKey :: Key a1 b1 -> Key a2 b2
unsafeCastKey = Key . unKey

-- | Pair of 'Key' and its value.
--
-- Type @a@ is the type of 'Element' that keeps the 'KeyValue'
-- pair. It drops the type of the value, so that you can construct a
-- heterogeneous list of key-value pairs for a given 'Element'.
--
-- @since 0.2.0.0
data KeyValue a where
  KeyValue :: Key a b -> Greskell b -> KeyValue a

-- | Constructor operator of 'KeyValue'.
--
-- @since 0.2.0.0
(=:) :: Key a b -> Greskell b -> KeyValue a
(=:) = KeyValue

-- $concrete_types
-- Concrete data types based on Aeson data types.
--
-- Element IDs and property values are all 'GValue', because they are
-- highly polymorphic. 'ElementID' and 'EdgeVertexID' are 'GValue',
-- too.
--
-- As for properties, you can use 'PropertyMap' and other type-classes
-- to manipulate them.
--
-- If you want to define your own graph structure types, see
-- [README.md](https://github.com/debug-ito/greskell#make-your-own-graph-structure-types)
-- for detail. Basically you can use 'FromGraphSON' instances of these
-- concrete data types to implement parsers for your own types.
--
-- NOTE: In version 0.1.1.0 and before, these conrete data types were
-- based on @GraphSON Value@. In version 0.2.0.0, this was changed to
-- 'GValue', so that it can parse nested data structures encoded in
-- GraphSON.


-- | General vertex type you can use for 'Vertex' class, based on
-- Aeson data types.
data AVertex =
  AVertex
  { avId :: ElementID AVertex,
    -- ^ ID of this vertex
    avLabel :: Text
    -- ^ Label of this vertex
  }
  deriving (Show,Eq)

instance ElementData AVertex where
  elementId = avId
  elementLabel = avLabel

instance Element AVertex where
  type ElementProperty AVertex = AVertexProperty

instance GraphSONTyped AVertex where
  gsonTypeFor _ = "g:Vertex"

instance FromJSON AVertex where
  parseJSON = parseJSONViaGValue

instance FromGraphSON AVertex where
  parseGraphSON gv = case gValueBody gv of
    GObject o -> AVertex
                 <$> (o .: "id")
                 <*> (o .: "label")
    _ -> empty

-- | General edge type you can use for 'Edge' class, based on Aeson
-- data types.
data AEdge =
  AEdge
  { aeId :: ElementID AEdge,
    -- ^ ID of this edge.
    aeLabel :: Text
    -- ^ Label of this edge.
  }
  deriving (Show,Eq)

instance ElementData AEdge where
  elementId = aeId
  elementLabel = aeLabel

instance Element AEdge where
  type ElementProperty AEdge = AProperty

instance GraphSONTyped AEdge where
  gsonTypeFor _ = "g:Edge"

instance FromJSON AEdge where
  parseJSON = parseJSONViaGValue

instance FromGraphSON AEdge where
  parseGraphSON gv = case gValueBody gv of
    GObject o -> AEdge
                 <$> (o .: "id")
                 <*> (o .: "label")
    _ -> empty

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
  parseJSON = parseJSONViaGValue

-- | Parse Property of GraphSON 1.0.
instance FromGraphSON v => FromGraphSON (AProperty v) where
  parseGraphSON gv = case gValueBody gv of
    GObject o -> AProperty <$> (o .: "key") <*> (o .: "value")
    _ -> empty

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
-- based on Aeson data types.
--
-- If you are not sure about the type @v@, just use 'GValue'.
data AVertexProperty v =
  AVertexProperty
  { avpId :: ElementID (AVertexProperty v),
    -- ^ ID of this vertex property.
    avpLabel :: Text,
    -- ^ Label and key of this vertex property.
    avpValue :: v
    -- ^ Value of this vertex property.
  }
  deriving (Show,Eq)

-- | In version 0.1.1.0 and before, the constraint was @FromJSON v@.
-- This has changed.
instance FromGraphSON v => FromJSON (AVertexProperty v) where
  parseJSON = parseJSONViaGValue

instance FromGraphSON v => FromGraphSON (AVertexProperty v) where
  parseGraphSON gv = case gValueBody gv of
    GObject o -> AVertexProperty
                 <$> (o .: "id")
                 <*> (o .: "label")
                 <*> (o .: "value")
    _ -> empty

instance GraphSONTyped (AVertexProperty v) where
  gsonTypeFor _ = "g:VertexProperty"

instance ElementData (AVertexProperty v) where
  elementId = avpId
  elementLabel = avpLabel

instance Element (AVertexProperty v) where
  type ElementProperty (AVertexProperty v) = AProperty

instance Property AVertexProperty where
  propertyKey = avpLabel
  propertyValue = avpValue

-- | Map the property value.
instance Functor AVertexProperty where
  fmap f vp = vp { avpValue = f $ avpValue vp,
                   avpId = unsafeCastElementID $ avpId vp
                 }

instance Foldable AVertexProperty where
  foldr f start vp = f (avpValue vp) start

-- | Traverse the property value.
instance Traversable AVertexProperty where
  traverse f vp = fmap setValue $ f $ avpValue vp
    where
      setValue v = vp { avpValue = v, avpId = unsafeCastElementID $ avpId vp }

