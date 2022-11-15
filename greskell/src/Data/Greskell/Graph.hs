{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
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
      Element (..)
    , ElementData (..)
    , ElementID (..)
    , unsafeCastElementID
    , Vertex
    , Edge
      -- * Property
    , Property (..)
      -- * T Enum
    , T
    , tId
    , tKey
    , tLabel
    , tValue
      -- * Cardinality Enum
    , Cardinality
    , cList
    , cSet
    , cSingle
      -- * Typed Key (accessor of a Property)
    , Key (..)
    , key
    , unsafeCastKey
      -- ** Key-value pair
    , KeyValue (..)
    , (=:)
      -- ** Heterogeneous list of keys
    , Keys (..)
    , singletonKeys
    , (-:)
      -- * Path
    , Path (..)
    , PathEntry (..)
    , pathToPMap
    , makePathEntry
      -- * Concrete data types
      -- $concrete_types
      -- ** Vertex
    , AVertex (..)
      -- ** Edge
    , AEdge (..)
      -- ** VertexProperty
    , AVertexProperty (..)
      -- ** Property
    , AProperty (..)
      -- * Examples
    , examples
    ) where

import           Control.Applicative           (empty, (<$>), (<*>), (<|>))
import           Control.Monad                 (when)
import           Data.Aeson                    (FromJSON (..), ToJSON (..), Value (..))
import           Data.Aeson.Types              (Parser)
import           Data.Foldable                 (Foldable (foldr), foldlM, toList)
import           Data.Hashable                 (Hashable)
import qualified Data.HashMap.Strict           as HM
import           Data.HashSet                  (HashSet)
import qualified Data.HashSet                  as HS
import           Data.Kind                     (Type)
import           Data.List.NonEmpty            (NonEmpty (..))
import qualified Data.List.NonEmpty            as NL
import           Data.Maybe                    (listToMaybe)
import           Data.Monoid                   (Monoid (..))
import           Data.Semigroup                (Semigroup, (<>))
import qualified Data.Semigroup                as Semigroup
import           Data.String                   (IsString (..))
import           Data.Text                     (Text)
import           Data.Traversable              (Traversable (traverse))
import           Data.Vector                   (Vector)
import           GHC.Generics                  (Generic)

import           Data.Greskell.AsIterator      (AsIterator (..))
import           Data.Greskell.AsLabel         (AsLabel (..), unsafeCastAsLabel)
import           Data.Greskell.GraphSON        (FromGraphSON (..), GValue, GValueBody (..),
                                                GraphSON (..), GraphSONTyped (..),
                                                parseJSONViaGValue, (.:))
import           Data.Greskell.GraphSON.GValue (gValueBody, gValueType)
import           Data.Greskell.Greskell        (Greskell, ToGreskell (..), string, toGremlin,
                                                unsafeGreskellLazy)
import           Data.Greskell.NonEmptyLike    (NonEmptyLike)
import           Data.Greskell.PMap            (Multi, PMap, PMapKey (..), Single, pMapInsert)

-- | ID of a graph element @e@ (vertex, edge and vertex property).
--
-- @since 1.0.0.0
newtype ElementID e
  = ElementID { unElementID :: GValue }
  deriving (Eq, FromGraphSON, FromJSON, Generic, Hashable, Show, ToJSON)

-- | Unsafely convert the element type.
instance Functor ElementID where
  fmap _ e = unsafeCastElementID e

-- | Unsafely cast the phantom type of 'ElementID'.
--
-- @since 1.0.0.0
unsafeCastElementID :: ElementID a -> ElementID b
unsafeCastElementID (ElementID e) = ElementID e

-- | Types that keep reference to TinkerPop graph Elements.
--
-- @since 1.0.0.0
class ElementData e where
  -- | ID of this Element.
  elementId :: e -> ElementID e
  -- | Label of this Element.
  elementLabel :: e -> Text

-- | @org.apache.tinkerpop.gremlin.structure.Element@ interface in a
-- TinkerPop graph.
--
-- Since greskell-1.0.0.0, 'ElementData' is a super-class of
-- 'Element'.
class ElementData e => Element e where
  -- | Property type of the 'Element'. It should be of 'Property'
  -- class.
  type ElementProperty e :: Type -> Type

  -- | Container type of the properties of the 'Element'. It should be
  -- of 'NonEmptyLike' class.
  --
  -- @since 1.0.0.0
  type ElementPropertyContainer e :: Type -> Type

-- | @org.apache.tinkerpop.gremlin.structure.Vertex@ interface in a
-- TinkerPop graph.
class (Element v) => Vertex v

-- | @org.apache.tinkerpop.gremlin.structure.Edge@ interface in a
-- TinkerPop graph.
class (Element e) => Edge e

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
tKey :: (Element (p v), Property p) => Greskell (T (p v) Text)
tKey = unsafeGreskellLazy "T.key"

-- | @T.label@ token.
tLabel :: Element a => Greskell (T a Text)
tLabel = unsafeGreskellLazy "T.label"

-- | @T.value@ token.
tValue :: (Element (p v), Property p) => Greskell (T (p v) v)
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
-- Since greskell-1.0.0.0, 'Key' is newtype of 'Text'. Before that, it
-- was newtype of 'Greskell' 'Text'.
newtype Key a b
  = Key { unKey :: Text }
  deriving (Eq, Show)

-- | Unsafely convert the value type @b@.
instance Functor (Key a) where
  fmap _ (Key t) = Key t

instance IsString (Key a b) where
  fromString = Key . fromString

-- | Return Gremlin String literal.
instance ToGreskell (Key a b) where
  type GreskellReturn (Key a b) = Text
  toGreskell = string . unKey

-- | @since 1.0.0.0
instance PMapKey (Key a b) where
  type PMapValue (Key a b) = b
  keyText (Key t) = t

-- | Create a 'Key' a text.
key :: Text -> Key a b
key = Key

-- | Unsafely cast the type signature of the 'Key'.
--
-- @since 1.0.0.0
unsafeCastKey :: Key a1 b1 -> Key a2 b2
unsafeCastKey = Key . unKey

-- | Pair of 'Key' and its value. Mainly used for writing properties
-- into the database.
--
-- Type @a@ is the type of 'Element' that keeps the 'KeyValue'
-- pair. It drops the type of the value, so that you can construct a
-- heterogeneous list of key-value pairs for a given 'Element'.
--
-- @since 0.2.0.0
data KeyValue a where
  -- | Key and value
  KeyValue :: Key a b -> Greskell b -> KeyValue a
  -- | Key without value
  --
  -- @since 1.0.0.0
  KeyNoValue :: Key a b -> KeyValue a

-- | Constructor operator of 'KeyValue'.
--
-- @since 0.2.0.0
(=:) :: Key a b -> Greskell b -> KeyValue a
(=:) = KeyValue

-- | Heterogeneous list of 'Key's. It keeps the parent type @a@, but
-- discards the value type @b@.
--
-- @since 1.0.0.0
data Keys a where
  -- | Empty 'Keys'.
  KeysNil :: Keys a
  -- | Add a 'Key' to 'Keys'.
  KeysCons :: Key a b -> Keys a -> Keys a

instance Semigroup (Keys a) where
  a <> b =
    case a of
      KeysNil         -> b
      KeysCons x rest -> KeysCons x (rest <> b)

instance Monoid (Keys a) where
  mempty = KeysNil
  mappend = (<>)

-- | 'Keys' with a single 'Key'.
--
-- @since 1.0.0.0
singletonKeys :: Key a b -> Keys a
singletonKeys k = KeysCons k KeysNil

-- | Prepend a 'Key' to 'Keys'.
--
-- @since 1.0.0.0
(-:) :: Key a b -> Keys a -> Keys a
(-:) = KeysCons

infixr 5 -:

-- $concrete_types
-- Graph structure data types based on Aeson.
--
-- You can use those types directly in your programs. You can also
-- define your own graph types by wrapping those with @newtype@. See
-- [README.md](https://github.com/debug-ito/greskell#make-your-own-graph-structure-types)
-- for detail.
--
-- Historical note:
--
-- - Since version 1.0.0.0, the concrete data types don't keep
--   properties, and IDs are of 'ElementID' type.
-- - In version 0.1.1.0 and before, these conrete data types were
--   based on @GraphSON Value@. In version 0.2.0.0, this was changed to
--   'GValue', so that it can parse nested data structures encoded in
--   GraphSON.

-- | General vertex type you can use for 'Vertex' class.
data AVertex
  = AVertex
      { avId    :: ElementID AVertex
        -- ^ ID of this vertex
      , avLabel :: Text
        -- ^ Label of this vertex
      }
  deriving (Eq, Show)

-- | @since 1.0.0.0
instance ElementData AVertex where
  elementId = avId
  elementLabel = avLabel

instance Element AVertex where
  type ElementProperty AVertex = AVertexProperty
  type ElementPropertyContainer AVertex = Multi

instance Vertex AVertex

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

-- | General edge type you can use for 'Edge' class.
data AEdge
  = AEdge
      { aeId    :: ElementID AEdge
        -- ^ ID of this edge.
      , aeLabel :: Text
        -- ^ Label of this edge.
      }
  deriving (Eq, Show)

-- | @since 1.0.0.0
instance ElementData AEdge where
  elementId = aeId
  elementLabel = aeLabel

instance Element AEdge where
  type ElementProperty AEdge = AProperty
  type ElementPropertyContainer AEdge = Single

instance Edge AEdge

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
data AProperty v
  = AProperty
      { apKey   :: Text
      , apValue :: v
      }
  deriving (Eq, Ord, Show)

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
    _         -> empty

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

-- | General vertex property type you can use for VertexProperty.
--
-- If you are not sure about the type @v@, just use 'GValue'.
data AVertexProperty v
  = AVertexProperty
      { avpId    :: ElementID (AVertexProperty v)
        -- ^ ID of this vertex property.
      , avpLabel :: Text
        -- ^ Label and key of this vertex property.
      , avpValue :: v
        -- ^ Value of this vertex property.
      }
  deriving (Eq, Show)

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

-- | @since 1.0.0.0
instance ElementData (AVertexProperty v) where
  elementId = avpId
  elementLabel = avpLabel

instance Element (AVertexProperty v) where
  type ElementProperty (AVertexProperty v) = AProperty
  type ElementPropertyContainer (AVertexProperty v) = Single

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


-- | @org.apache.tinkerpop.gremlin.process.traversal.Path@ interface.
--
-- @since 1.1.0.0
newtype Path a
  = Path { unPath :: [PathEntry a] }
  deriving (Eq, Foldable, Functor, Monoid, Ord, Semigroup, Show, Traversable)

instance GraphSONTyped (Path a) where
  gsonTypeFor _ = "g:Path"

-- | @Path@ is an @Iterable@ that emits its objects of type @a@.
instance AsIterator (Path a) where
  type IteratorItem (Path a) = a

instance FromGraphSON a => FromJSON (Path a) where
  parseJSON = parseJSONViaGValue

instance FromGraphSON a => FromGraphSON (Path a) where
  parseGraphSON gv =
    case gValueBody gv of
      GObject o -> parseObj o
      _         -> empty
    where
      parseObj o = do
        labels <- o .: "labels"
        objects <- o .: "objects"
        let nlabels = length labels
            nobjects = length objects
        when (nlabels /= nobjects) $ do
          fail ( "Different number of labels and objects: "
                 <> show nlabels <> " labels, "
                 <> show nobjects <> " objects."
               )
        return $ Path $ map (uncurry PathEntry) $ zip (map (HS.map AsLabel) labels) objects

-- | An entry in a 'Path'.
--
-- @since 1.1.0.0
data PathEntry a
  = PathEntry
      { peLabels :: HashSet (AsLabel a)
      , peObject :: a
      }
  deriving (Eq, Ord, Show)

instance Functor PathEntry where
  fmap f pe = PathEntry { peLabels = HS.map (fmap f) $ peLabels pe,
                          peObject = f $ peObject pe
                        }

instance Foldable PathEntry where
  foldr f acc pe = f (peObject pe) acc

instance Traversable PathEntry where
  traverse f pe = fmap mkPE $ f $ peObject pe
    where
      mkPE obj =
        PathEntry { peLabels = HS.map unsafeCastAsLabel $ peLabels pe,
                    peObject = obj
                  }

-- | Convert a 'Path' into 'PMap'.
--
-- In the result 'PMap', the keys are the labels in the 'Path', and
-- the values are the objects associated with the labels. The values
-- are stored in the same order in the 'Path'. Objects without any
-- label are discarded.
--
-- @since 1.1.0.0
pathToPMap :: Path a -> PMap Multi a
pathToPMap (Path entries) = foldr fentry mempty entries
  where
    fentry entry pm = foldr (flabel $ peObject entry) pm $ peLabels entry
    flabel obj label pm = pMapInsert (unAsLabel label) obj pm

-- | Make a 'PathEntry'.
--
-- @since 1.1.0.0
makePathEntry :: [AsLabel a] -- ^ labels
              -> a -- ^ object
              -> PathEntry a
makePathEntry ls obj = PathEntry (HS.fromList ls) obj

-- | Examples of using this module. See the source. The 'fst' of the output is the testee, while the
-- 'snd' is the expectation.
examples :: [(Text, Text)]
examples =
  [ (toGremlin cList, "list")
  , (toGremlin ("age" :: Key AVertex Int), "\"age\"")
  , (toGremlin (key "created_at" :: Key AEdge Text), "\"created_at\"")
  , (keyText ("name" :: Key AVertex Text), "name")
  ]
