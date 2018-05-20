{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Data.Greskell.GraphSON.GraphSONTyped
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __Internal module.__ Just to resolve cyclic dependency between
-- GraphSON and GMap.
module Data.Greskell.GraphSON.GraphSONTyped
       ( GraphSONTyped(..)
       ) where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.HashMap.Lazy as L (HashMap)
import qualified Data.HashMap.Strict as S (HashMap)
import Data.HashSet (HashSet)
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.IntMap.Lazy as L (IntMap)
import qualified Data.IntMap.Strict as S (IntMap)
import Data.IntSet (IntSet)
import qualified Data.Map.Lazy as L (Map)
import qualified Data.Map.Strict as S (Map)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Set (Set)


-- | Types that have an intrinsic type ID for 'gsonType' field.
class GraphSONTyped a where
  gsonTypeFor :: a -> Text
  -- ^ Type ID for 'gsonType'.

instance GraphSONTyped Char where
  gsonTypeFor _ = "gx:Char"

-- | Map to \"gx:Byte\". Note that Java's Byte is signed.
instance GraphSONTyped Int8 where
  gsonTypeFor _ = "gx:Byte"

instance GraphSONTyped Int16 where
  gsonTypeFor _ = "gx:Int16"

instance GraphSONTyped Int32 where
  gsonTypeFor _ = "g:Int32"

instance GraphSONTyped Int64 where
  gsonTypeFor _ = "g:Int64"

instance GraphSONTyped Float where
  gsonTypeFor _ = "g:Float"

instance GraphSONTyped Double where
  gsonTypeFor _ = "g:Double"

instance GraphSONTyped [a] where
  gsonTypeFor _ = "g:List"

instance GraphSONTyped (Vector a) where
  gsonTypeFor _ = "g:List"

instance GraphSONTyped (Seq a) where
  gsonTypeFor _ = "g:List"

-- | Map to \"g:Double\".
instance GraphSONTyped Scientific where
  gsonTypeFor _ = "g:Double"

instance GraphSONTyped (HashSet a) where
  gsonTypeFor _ = "g:Set"

instance GraphSONTyped IntSet where
  gsonTypeFor _ = "g:Set"

instance GraphSONTyped (Set a) where
  gsonTypeFor _ = "g:Set"

instance GraphSONTyped (L.HashMap k v) where
  gsonTypeFor _ = "g:Map"

instance GraphSONTyped (L.Map k v) where
  gsonTypeFor _= "g:Map"

instance GraphSONTyped (L.IntMap v) where
  gsonTypeFor _= "g:Map"

-- -- Implementation of Lazy and Strict types are the same.
-- 
-- instance GraphSONTyped (S.HashMap k v) where
--   gsonTypeFor _ = "g:Map"
-- 
-- instance GraphSONTyped (S.Map k v) where
--   gsonTypeFor _= "g:Map"
-- 
-- instance GraphSONTyped (S.IntMap v) where
--   gsonTypeFor _= "g:Map"


instance (GraphSONTyped a, GraphSONTyped b) => GraphSONTyped (Either a b) where
  gsonTypeFor e = either gsonTypeFor gsonTypeFor e
