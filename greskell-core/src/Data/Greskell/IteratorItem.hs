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
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
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

type instance IteratorItem TL.Text = TL.Text
type instance IteratorItem Bool = Bool
type instance IteratorItem Char = Char
type instance IteratorItem Double = Double
type instance IteratorItem Float = Float
type instance IteratorItem Int8 = Int8
type instance IteratorItem Int16 = Int16
type instance IteratorItem Int32 = Int32
type instance IteratorItem Int64 = Int64
type instance IteratorItem Integer = Integer
type instance IteratorItem Natural = Natural
type instance IteratorItem Word = Word
type instance IteratorItem Word8 = Word8
type instance IteratorItem Word16 = Word16
type instance IteratorItem Word32 = Word32
type instance IteratorItem Word64 = Word64
type instance IteratorItem Scientific = Scientific

-- Integral a => type instance IteratorItem (Ratio a) = Ratio a
-- Maybe?


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
