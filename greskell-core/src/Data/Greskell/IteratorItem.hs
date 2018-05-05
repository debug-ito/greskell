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

-- | 'IteratorItem' type family is association of type @a@ and the
-- type of its item when type @a@ is seen as an iterator.
--
-- In Gremlin, the conversion is done by
-- @org.apache.tinkerpop.gremlin.util.iterator.asIterator@ method.
type family IteratorItem a

type instance IteratorItem Int = Int
