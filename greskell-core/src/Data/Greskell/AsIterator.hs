{-# LANGUAGE TypeFamilies #-}
-- |
-- Module: Data.Greskell.AsIterator
-- Description: Conversion from Object to Iterator in Gremlin
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.AsIterator
       ( AsIterator(..)
       ) where

import qualified Data.HashMap.Lazy as L (HashMap)
import Data.HashSet (HashSet)
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.IntMap.Lazy as L (IntMap)
import Data.IntSet (IntSet)
import qualified Data.Map.Lazy as L (Map)
import Data.Ratio (Ratio)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)

import Data.Greskell.GMap (GMap, GMapEntry)

-- | Types that are converted to an iterator by
-- @org.apache.tinkerpop.gremlin.util.iterator.IteratorUtils.asIterator@
-- method. In fact, that method can convert any type to an iterator.
--
-- Associated with this type-class is 'IteratorItem'. 'IteratorItem'
-- type family is association of type @a@ and the type of its item
-- when type @a@ is converted to an iterator.
--
-- 'IteratorItem' rule of thumb:
--
-- - @Iterator@ and @Iterable@ types like @List@, @Stream@ and
--   @GraphTraversal@ are converted to their element types.
-- - @Map@ type is converted to its @Map.Entry@. In greskell,
--   @Map.Entry@ is expressed as 'GMapEntry'.
-- - Other types are converted to themselves.
--
-- Caveat:
--
-- - Because Haskell's 'String' is '[Char]', @IteratorItem String@
--   returns 'Char', which is incorrect. Use 'Text' if you want to
--   deal with @String@s in Gremlin.
class AsIterator a where
  type IteratorItem a

instance AsIterator Int where
  type IteratorItem Int = Int
instance AsIterator Text where
  type IteratorItem Text = Text
instance AsIterator TL.Text where
  type IteratorItem TL.Text = TL.Text
instance AsIterator Bool where
  type IteratorItem Bool = Bool
instance AsIterator Char where
  type IteratorItem Char = Char
instance AsIterator Double where
  type IteratorItem Double = Double
instance AsIterator Float where
  type IteratorItem Float = Float
instance AsIterator Int8 where
  type IteratorItem Int8 = Int8
instance AsIterator Int16 where
  type IteratorItem Int16 = Int16
instance AsIterator Int32 where
  type IteratorItem Int32 = Int32
instance AsIterator Int64 where
  type IteratorItem Int64 = Int64
instance AsIterator Integer where
  type IteratorItem Integer = Integer
instance AsIterator Natural where
  type IteratorItem Natural = Natural
instance Integral a => AsIterator (Ratio a) where
  type IteratorItem (Ratio a) = Ratio a
instance AsIterator Word where
  type IteratorItem Word = Word
instance AsIterator Word8 where
  type IteratorItem Word8 = Word8
instance AsIterator Word16 where
  type IteratorItem Word16 = Word16
instance AsIterator Word32 where
  type IteratorItem Word32 = Word32
instance AsIterator Word64 where
  type IteratorItem Word64 = Word64
instance AsIterator Scientific where
  type IteratorItem Scientific = Scientific

instance AsIterator [a] where
  type IteratorItem [a] = a
instance AsIterator (Vector a) where
  type IteratorItem (Vector a) = a
instance AsIterator (HashSet a) where
  type IteratorItem (HashSet a) = a
instance AsIterator (Seq a) where
  type IteratorItem (Seq a) = a
instance AsIterator (Set a) where
  type IteratorItem (Set a) = a
instance AsIterator IntSet where
  type IteratorItem IntSet = Int

instance AsIterator (GMap c k v) where
  type IteratorItem (GMap c k v) = GMapEntry k v
instance AsIterator (GMapEntry k v) where
  type IteratorItem (GMapEntry k v) = GMapEntry k v
instance AsIterator (L.HashMap k v) where
  type IteratorItem (L.HashMap k v) = GMapEntry k v
instance AsIterator (L.Map k v) where
  type IteratorItem (L.Map k v) = GMapEntry k v
instance AsIterator (L.IntMap v) where
  type IteratorItem (L.IntMap v) = GMapEntry Int v

instance AsIterator a => AsIterator (Maybe a) where
  type IteratorItem (Maybe a) = Maybe (IteratorItem a)

-- About encoding of Map.Entry
--
-- It seems that GraphSON encodes a Map.Entry as if it were a
-- single-entry Map, but who is responsble for that? jackson?
--
-- Maybe these topics are related?
--
-- - https://github.com/fasterxml/jackson-databind/issues/565
-- - https://fasterxml.github.io/jackson-databind/javadoc/2.8/com/fasterxml/jackson/databind/ser/impl/MapEntrySerializer.html
