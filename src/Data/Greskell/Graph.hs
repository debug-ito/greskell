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
         -- ** Types
         Element(..),
         Vertex,
         Edge,
         Property(..),
         VertexProperty,
         -- * Concrete data types
         AesonVertex,
         AesonEdge
       ) where

import Control.Applicative (empty)
import Data.Aeson (Value)
import Data.Text (Text)

import Data.Greskell.Greskell (Greskell, unsafeGreskellLazy)

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
