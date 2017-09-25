-- |
-- Module: Data.Greskell.Graph
-- Description: Haskell counterpart of Gremlin graph structure data types.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.Graph
       ( ElementID,
         Element,
         Vertex,
         Edge,
         GVertex,
         GEdge
       ) where

import Control.Applicative (empty)
import Data.Aeson (FromJSON(..))
import Data.Text (Text)

type ElementID = Integer

-- | @Element@ interface in a TinkerPop graph.
class Element e where
  elementId :: e -> ElementID
  elementLabel :: e -> Text

-- | @Vertex@ interface in a TinkerPop graph.
class (Element v, FromJSON v) => Vertex v

-- | @Edge@ interface in a TinkerPop graph.
class (Element e, FromJSON e) => Edge e

-- | General vertex type you can use for 'Vertex' class.
data GVertex

-- | TODO: 'Element' methods are not implemented yet.
instance Element GVertex where
  elementId = undefined
  elementLabel = undefined

-- | TODO: 'FromJSON' methods are not implemented yet.
instance FromJSON GVertex where
  parseJSON _ = empty

instance Vertex GVertex

-- | General edge type you can use for 'Edge' class.
data GEdge

-- | TODO: 'Element' methods are not implemented yet.
instance Element GEdge where
  elementId = undefined
  elementLabel = undefined

-- | TODO: 'FromJSON' methods are not implemented yet.
instance FromJSON GEdge where
  parseJSON _ = empty

instance Edge GEdge

