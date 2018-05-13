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
import Data.Text (Text)
import Data.Vector (Vector)

import Data.Greskell.GMap (GMap, GMapEntry)

-- | 'IteratorItem' type family is association of type @a@ and the
-- type of its item when type @a@ is seen as an iterator.
--
-- In Gremlin, the conversion is done by
-- @org.apache.tinkerpop.gremlin.util.iterator.IteratorUtils.asIterator@
-- method.
--
-- Conversion rule of thumb:
--
-- - @Iterator@ and @Iterable@ types like @List@, @Stream@ and
--   @GraphTraversal@ are converted to their element types.
-- - @Map@ type is converted to its @Map.Entry@.
-- - Other types are converted to themselves.
--
-- Caveat:
--
-- - Because Haskell's 'String' is '[Char]', @IteratorItem String@
--   returns 'Char', which is incorrect. Use 'Text' if you want to
--   deal with @String@s in Gremlin.
type family IteratorItem a

type instance IteratorItem Int = Int

type instance IteratorItem Text = Text

type instance IteratorItem [a] = a

type instance IteratorItem (Vector a) = a

-- | @asIterator@ converts a @Map@ to @Iterator<Map.Entry>@.
type instance IteratorItem (GMap k v) = GMapEntry k v

type instance IteratorItem (HashMap k v) = GMapEntry k v

-- About encoding of Map.Entry
--
-- It seems that GraphSON encodes a Map.Entry as if it were a
-- single-entry Map, but who is responsble for that? jackson?
--
-- Maybe these topics are related?
--
-- - https://github.com/fasterxml/jackson-databind/issues/565
-- - https://fasterxml.github.io/jackson-databind/javadoc/2.8/com/fasterxml/jackson/databind/ser/impl/MapEntrySerializer.html

-- TODO: add more type instances (mostly for scalar types)
