-- |
-- Module: Data.Greskell.Extra
-- Description: Extra utility functions implemented by Greskell
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- Extra utility functions implemented by Greskell.
--
-- @since 0.2.3.0
module Data.Greskell.Extra
  ( writeKeyValues,
    (<=:>),
    writePropertyKeyValues,
    writePMapProperties
  ) where

import Data.Aeson (ToJSON)
import Data.Foldable (Foldable)
import Data.Greskell.Binder (Binder, newBind)
import Data.Greskell.Graph
  ( Property(..), Element, KeyValue(..), (=:), Key
  )
import qualified Data.Greskell.Graph as Graph
import Data.Greskell.GTraversal (Walk, SideEffect, gProperty)
import Data.Greskell.PMap (PMap, pMapToList)
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

-- | Make a series of @.property@ steps to write the given key-value
-- pairs as properties. Use '(<=:>)' to make a 'KeyValue' within
-- 'Binder'.
writeKeyValues :: Element e => [KeyValue e] -> Walk SideEffect e e
writeKeyValues pairs = mconcat $ map toPropStep pairs
  where
    toPropStep (KeyValue k v) = gProperty k v

-- | Make a series of @.property@ steps to write all properties in the
-- given 'PMap'.
writePMapProperties :: (Foldable c, ToJSON v, Element e)
                    => PMap c v -> Binder (Walk SideEffect e e)
writePMapProperties = writePropertyKeyValues . pMapToList

-- | Like '(=:)', but this one takes a real value, binds it into a
-- 'Greskell' value and returns 'KeyValue'.
(<=:>) :: ToJSON b => Key a b -> b -> Binder (KeyValue a)
(<=:>) k v = (=:) k <$> newBind v
