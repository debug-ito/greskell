{-# LANGUAGE TypeFamilies #-}
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
         -- * Concrete data types
         AesonVertex,
         AesonEdge
       ) where

import Control.Applicative (empty)
import Data.Aeson (FromJSON(..), Value)
import Data.Text (Text)

-- | @Element@ interface in a TinkerPop graph.
class Element e where
  type ElementID e
  elementId :: e -> ElementID e
  elementLabel :: e -> Text

-- | @Vertex@ interface in a TinkerPop graph.
class (Element v, FromJSON v) => Vertex v

-- | @Edge@ interface in a TinkerPop graph.
class (Element e, FromJSON e) => Edge e

-- | General vertex type you can use for 'Vertex' class, based on
-- aeson data types.
data AesonVertex

-- | TODO: 'Element' methods are not implemented yet.
instance Element AesonVertex where
  type ElementID AesonVertex = Value
  elementId = undefined
  elementLabel = undefined

-- | TODO: 'FromJSON' methods are not implemented yet.
instance FromJSON AesonVertex where
  parseJSON _ = empty

instance Vertex AesonVertex

-- | General edge type you can use for 'Edge' class, based on aeson
-- data types.
data AesonEdge

-- | TODO: 'Element' methods are not implemented yet.
instance Element AesonEdge where
  type ElementID AesonEdge = Value
  elementId = undefined
  elementLabel = undefined

-- | TODO: 'FromJSON' methods are not implemented yet.
instance FromJSON AesonEdge where
  parseJSON _ = empty

instance Edge AesonEdge
