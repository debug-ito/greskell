-- |
-- Module: Data.Greskell.Gremlin
-- Description: Basic Gremlin (Groovy/Java) data types
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.Gremlin
       (Comparator) where

-- | @java.util.Comparator@ class.
type Comparator a = a -> a -> Int
