{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
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
         VertexProperty,
         T,
         tId,
         tKey,
         tLabel,
         tValue,
         -- * Extended API
         Key,
         keyFromText,
         keyToText,
         -- * Concrete data types
         AesonVertex,
         AesonEdge
       ) where

import Control.Applicative (empty)
import Data.Aeson (Value)
import Data.String (IsString(..))
import Data.Text (Text)

import Data.Greskell.Greskell (Greskell, unsafeGreskellLazy, string)

-- | @Element@ interface in a TinkerPop graph.
class Element e where
  type ElementID e
  elementId :: e -> ElementID e
  elementLabel :: e -> Text

-- | @Vertex@ interface in a TinkerPop graph.
class (Element v) => Vertex v

-- | @Edge@ interface in a TinkerPop graph.
class (Element e) => Edge e

-- | @Property@ interface in a TinkerPop graph.
class Property p where
  type PropertyValue p
  propertyKey :: p -> Text
  propertyValue :: p -> PropertyValue p

-- | @VertexProperty@ interface in a TinkerPop graph.
class (Element p, Property p) => VertexProperty p


-- | @org.apache.tinkerpop.gremlin.structure.T@ enum.
--
-- 'T' is a token to get data @b@ from an Element @a@.
data T a b

-- | @T.id@ token.
tId :: Element a => Greskell (T a (ElementID a))
tId = unsafeGreskellLazy "id"

-- | @T.key@ token.
tKey :: VertexProperty a => Greskell (T a Text)
tKey = unsafeGreskellLazy "key"

-- | @T.label@ token.
tLabel :: Element a => Greskell (T a Text)
tLabel = unsafeGreskellLazy "label"

-- | @T.value@ token.
tValue :: VertexProperty a => Greskell (T a (PropertyValue a))
tValue = unsafeGreskellLazy "value"


-- | A property key accessing value @b@ in an Element @a@. In Gremlin,
-- it's just a String type.
newtype Key a b = Key Text
                deriving (Show,Eq)

-- | Treat the String expression as a 'Key'.
keyFromText :: Element a
            => Greskell Text -- ^ property key string
            -> Greskell (Key a b)
keyFromText = fmap Key

-- | Treat the 'Key' as just a String.
keyToText :: Greskell (Key a b) -> Greskell Text
keyToText = fmap (\(Key t) -> t)

-- | Unsafely convert the value type @b@.
instance Functor (Key a) where
  fmap _ (Key t) = Key t

instance Element a => IsString (Key a b) where
  fromString = Key . fromString


-- | General vertex type you can use for 'Vertex' class, based on
-- aeson data types.
data AesonVertex

-- | TODO: 'Element' methods are not implemented yet.
instance Element AesonVertex where
  type ElementID AesonVertex = Value
  elementId = undefined
  elementLabel = undefined

instance Vertex AesonVertex

-- | General edge type you can use for 'Edge' class, based on aeson
-- data types.
data AesonEdge

-- | TODO: 'Element' methods are not implemented yet.
instance Element AesonEdge where
  type ElementID AesonEdge = Value
  elementId = undefined
  elementLabel = undefined

instance Edge AesonEdge
