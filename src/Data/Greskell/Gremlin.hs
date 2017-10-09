{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
-- |
-- Module: Data.Greskell.Gremlin
-- Description: Basic Gremlin (Groovy/Java) data types
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.Gremlin
       ( -- * Comparator
         Comparator(..),
         -- ** org.apache.tinkerpop.gremlin.process.traversal.Order
         Order,
         oDecr,
         oDecr',
         oIncr,
         oIncr',
         oShuffle,
         oShuffle'
       ) where

import Data.Aeson (Value)
import Data.Monoid ((<>))
import Data.Greskell.Greskell
  ( Greskell, unsafeGreskellLazy,
    toGremlinLazy
  )

-- | @java.util.Comparator@ interface.
class Comparator c where
  type CompareArg c
  cCompare :: Greskell c -> Greskell (CompareArg c) -> Greskell (CompareArg c) -> Greskell Int
  cCompare cmp a b = unsafeGreskellLazy
                     ( "(" <> toGremlinLazy cmp <> ").compare("
                       <> toGremlinLazy a <> "," <> toGremlinLazy b <> ")"
                     )
                     
-- | org.apache.tinkerpop.gremlin.process.traversal.Order enum.
data Order a

instance Comparator (Order a) where
  type CompareArg (Order a) = a

-- | @decr@ order.
oDecr :: Greskell (Order a)
oDecr = unsafeGreskellLazy "decr"

-- | Monomorphic version of 'oDecr'.
oDecr' :: Greskell (Order Value)
oDecr' = oDecr

-- | @incr@ order.
oIncr :: Greskell (Order a)
oIncr = unsafeGreskellLazy "incr"

-- | Monomorphic version of 'oIncr'
oIncr' :: Greskell (Order Value)
oIncr' = oIncr

-- | @shuffle@ order.
oShuffle :: Greskell (Order a)
oShuffle = unsafeGreskellLazy "shuffle"

-- | Monomorphic version of 'oShuffle''
oShuffle' :: Greskell (Order Value)
oShuffle' = oShuffle
