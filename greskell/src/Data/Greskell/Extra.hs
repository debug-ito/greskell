-- |
-- Module: Data.Greskell.Extra
-- Description: Extra utility functions implemented by Greskell
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- Extra utility functions implemented by Greskell.
--
-- @since 0.2.3.0
module Data.Greskell.Extra
  ( writePropertyKeyValues
  ) where

import Data.Aeson (ToJSON)
import Data.Greskell.Binder (Binder, newBind)
import Data.Greskell.Graph
  ( Property(..), Element
  )
import qualified Data.Greskell.Graph as Graph
import Data.Greskell.GTraversal (Walk, SideEffect, gProperty)
import Data.Monoid (mconcat)
import Data.Text (Text)

-- | Make a series of @.property@ steps to write the given key-value
-- pairs as properties.
--
-- @since 0.2.3.0
writePropertyKeyValues :: (ToJSON v, Element e) => [(Text, v)] -> Binder (Walk SideEffect e e)
writePropertyKeyValues pairs = fmap mconcat $ mapM toPropStep pairs
  where
    toPropStep (key, value) = fmap (gProperty $ Graph.key key) $ newBind value

---- -- | Make a series of @.property@ steps to write all properties in the
---- -- given 'PropertyMap'.
---- --
---- -- @since 0.2.3.0
---- writeAllProperties :: (PropertyMap m, Property p, ToJSON v, Element e)
----                    => m p v -> Binder (Walk SideEffect e e)
---- writeAllProperties ps = writePropertyKeyValues $ map toPair $ allProperties ps
----   where
----     toPair prop = (propertyKey prop, propertyValue prop)
