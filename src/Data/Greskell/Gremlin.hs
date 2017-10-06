{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Data.Greskell.Gremlin
-- Description: Basic Gremlin (Groovy/Java) data types
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.Gremlin
       ( -- * Comparator
         Comparator,
         -- ** org.apache.tinkerpop.gremlin.process.traversal.Order
         oDecr,
         oIncr,
         oShuffle
       ) where

import Data.Greskell.Greskell (Greskell, unsafeGreskellLazy)

-- | @java.util.Comparator@ class.
type Comparator a = a -> a -> Int

-- | @decr@ order.
oDecr :: Greskell (Comparator a)
oDecr = unsafeGreskellLazy "decr"

-- | @incr@ order.
oIncr :: Greskell (Comparator a)
oIncr = unsafeGreskellLazy "incr"

-- | @shuffle@ order.
oShuffle :: Greskell (Comparator a)
oShuffle = unsafeGreskellLazy "shuffle"
