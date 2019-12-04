{-# LANGUAGE OverloadedStrings #-}
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

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Greskell.Binder (runBinder)
-- >>> import Data.Greskell.Greskell (toGremlin)
-- >>> import Data.Greskell.Graph (AVertex)
-- >>> import Data.List (sortBy)
-- >>> import Data.Ord (comparing)
-- >>> import qualified Data.HashMap.Strict as HashMap

-- | Make a series of @.property@ steps to write the given key-value
-- pairs as properties.
--
-- @since 0.2.3.0
--
-- >>> let binder = (writePropertyKeyValues [("age", (21 :: Int))] :: Binder (Walk SideEffect AVertex AVertex))
-- >>> let (walk, binding) = runBinder binder
-- >>> toGremlin walk
-- "__.property(\"age\",__v0).identity()"
-- >>> sortBy (comparing fst) $ HashMap.toList binding
-- [("__v0",Number 21.0)]
writePropertyKeyValues :: (ToJSON v, Element e) => [(Text, v)] -> Binder (Walk SideEffect e e)
writePropertyKeyValues pairs = fmap mconcat $ mapM toPropStep pairs
  where
    toPropStep (key, value) = fmap (gProperty $ Graph.key key) $ newBind value

-- | Make a series of @.property@ steps to write the given key-value
-- pairs as properties. Use '(<=:>)' to make a 'KeyValue' within
-- 'Binder'.
--
-- >>> let keyAge = ("age" :: Key AVertex Int)
-- >>> let keyName = ("name" :: Key AVertex Text)
-- >>> let (walk, binding) = runBinder $ writeKeyValues <$> sequence [keyAge <=:> 21, keyName <=:> "Josh"]
-- >>> toGremlin walk
-- "__.property(\"age\",__v0).property(\"name\",__v1).identity()"
-- >>> sortBy (comparing fst) $ HashMap.toList binding
-- [("__v0",Number 21.0),("__v1",String "Josh")]
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
