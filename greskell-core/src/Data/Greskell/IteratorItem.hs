{-# LANGUAGE TypeFamilies #-}
-- |
-- Module: Data.Greskell.IteratorItem
-- Description: Conversion from Object to Iterator in Gremlin
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.IteratorItem
       ( IteratorItem
       ) where

import Data.HashMap.Strict (HashMap)

import Data.Greskell.GMap (GMap, GMapEntry)

-- | 'IteratorItem' type family is association of type @a@ and the
-- type of its item when type @a@ is seen as an iterator.
--
-- In Gremlin, the conversion is done by
-- @org.apache.tinkerpop.gremlin.util.iterator.IteratorUtils.asIterator@
-- method.
type family IteratorItem a

type instance IteratorItem Int = Int

type instance IteratorItem [a] = a

-- | @asIterator@ converts a @Map@ to @Iterator<Map.Entry>@.
type instance IteratorItem (GMap k v) = GMapEntry k v

type instance IteratorItem (HashMap k v) = GMapEntry k v

-- TODO: add more type instances (mostly for scalar types)
