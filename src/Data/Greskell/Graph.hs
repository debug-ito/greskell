{-# LANGUAGE TypeFamilies, OverloadedStrings, FlexibleInstances, GeneralizedNewtypeDeriving #-}
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
         Edge,
         Property(..),
         T,
         tId,
         tKey,
         tLabel,
         tValue,
         -- * Extended API
         Key(..),
         -- * Concrete data types
         AesonVertex,
         AesonEdge,
         AesonVertexProperty,
         SimpleProperty(..),
         -- ** PropertyMap
         PropertyMap(..),
         PropertyMapGeneric,
         PropertyMapSingle,
         PropertyMapList
       ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (Value(..), FromJSON(..), (.:))
import Data.Foldable (toList)
import qualified Data.HashMap.Lazy as HM
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NL
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid)
import Data.Semigroup ((<>), Semigroup)
import qualified Data.Semigroup as Semigroup
import Data.String (IsString(..))
import Data.Text (Text)

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
class (Element e) => Edge e

-- | @Property@ interface in a TinkerPop graph.
class Property p where
  propertyKey :: p v -> Text
  propertyValue :: p v -> v

-- | @org.apache.tinkerpop.gremlin.structure.T@ enum.
--
-- 'T' is a token to get data @b@ from an Element @a@.
data T a b

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

-- TODO: AesonVertexなどの実装では、propertiesはPropertyMap t p
-- (GraphSON Value)という感じでGraphSONラッパをつけるといい。

-- | General vertex type you can use for 'Vertex' class, based on
-- aeson data types.
data AesonVertex

-- | TODO: 'Element' methods are not implemented yet.
instance Element AesonVertex where
  type ElementID AesonVertex = Value
  type ElementProperty AesonVertex = AesonVertexProperty
  elementId = undefined
  elementLabel = undefined

instance Vertex AesonVertex

-- | General edge type you can use for 'Edge' class, based on aeson
-- data types.
data AesonEdge

-- | TODO: 'Element' methods are not implemented yet.
instance Element AesonEdge where
  type ElementID AesonEdge = Value
  type ElementProperty AesonEdge = SimpleProperty
  elementId = undefined
  elementLabel = undefined

instance Edge AesonEdge

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

instance Property SimpleProperty where
  propertyKey = sPropertyKey
  propertyValue = sPropertyValue


-- | General vertex property type you can use for VertexProperty,
-- based on aeson data types.
data AesonVertexProperty v

-- | TODO: 'Element' methods are not implemented yet.
instance Element (AesonVertexProperty v) where
  type ElementID (AesonVertexProperty v) = Value
  type ElementProperty (AesonVertexProperty v) = SimpleProperty
  elementId = undefined
  elementLabel = undefined

-- | TODO: 'Property' methods are not implemented yet.
instance Property AesonVertexProperty where
  propertyKey = undefined
  propertyValue = undefined

-- -- We could define the following constraint synonym with
-- -- ConstraintKinds extension, although its semantics is not exactly
-- -- correct..
-- type VertexProperty p v = (Element (p v), Property p)


-- | Common basic operations supported by maps of properties.
class PropertyMap m where
  lookupOne :: Text -> m p v -> Maybe (p v)
  lookupOne key m = listToMaybe $ lookupList key m
  lookupList :: Text -> m p v -> [p v]
  putProperty :: Property p => p v -> m p v -> m p v
  removeProperty :: Text -> m p v -> m p v
  allProperties :: m p v -> [p v]

-- | Generic implementation of 'PropertyMap'. @t@ is the type of
-- cardinality, @p@ is the type of 'Property' class and @v@ is the
-- type of the property value.
newtype PropertyMapGeneric t p v = PropertyMapGeneric (HM.HashMap Text (t (p v)))
                                 deriving (Show,Eq,Monoid)

-- TODO: MonoidはtをSemigroupにして連結させるように実装するといいので
-- は。そうするとPropertyMapListはvalue overrideじゃなくてList連結にな
-- る。

-- TODO: PropertyMapをベースにvalueを取り出す関数とか作る。

putPropertyGeneric :: (Semigroup (t (p v)), Applicative t, Property p) => p v -> PropertyMapGeneric t p v -> PropertyMapGeneric t p v
putPropertyGeneric prop (PropertyMapGeneric hm) =
  PropertyMapGeneric $ HM.insertWith (<>) (propertyKey prop) (pure prop) hm

removePropertyGeneric :: Text -> PropertyMapGeneric t p v -> PropertyMapGeneric t p v
removePropertyGeneric key (PropertyMapGeneric hm) = PropertyMapGeneric $ HM.delete key hm

allPropertiesGeneric :: Foldable t => PropertyMapGeneric t p v -> [p v]
allPropertiesGeneric (PropertyMapGeneric hm) = concat $ map toList $ HM.elems hm

-- | A 'PropertyMap' that has a single value per key.
type PropertyMapSingle = PropertyMapGeneric Semigroup.First

instance PropertyMap (PropertyMapGeneric Semigroup.First) where
  lookupOne key (PropertyMapGeneric hm) = fmap Semigroup.getFirst $ HM.lookup key hm
  lookupList key m = maybe [] return $ lookupOne key m
  putProperty = putPropertyGeneric
  removeProperty = removePropertyGeneric
  allProperties = allPropertiesGeneric

-- | A 'PropertyMap' that can keep more than one values per key.
type PropertyMapList = PropertyMapGeneric NonEmpty

instance PropertyMap (PropertyMapGeneric NonEmpty) where
  lookupList key (PropertyMapGeneric hm) = maybe [] NL.toList $ HM.lookup key hm
  putProperty = putPropertyGeneric
  removeProperty = removePropertyGeneric
  allProperties = allPropertiesGeneric

