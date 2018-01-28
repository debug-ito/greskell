{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
-- |
-- Module: Data.Greskell.Gremlin
-- Description: Basic Gremlin (Groovy/Java) data types
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.Gremlin
       ( -- * Predicate
         Predicate(..),
         PredicateA(..),
         -- ** org.apache.tinkerpop.gremlin.process.traversal.P
         P,
         pNot,
         pEq,
         pNeq,
         pLt,
         pLte,
         pGt,
         pGte,
         pInside,
         pOutside,
         pBetween,
         pWithin,
         pWithout,
         -- * Comparator
         Comparator(..),
         ComparatorA(..),
         -- ** org.apache.tinkerpop.gremlin.process.traversal.Order
         Order,
         oDecr,
         oIncr,
         oShuffle,
       ) where

import Data.Aeson (Value)
import Data.Monoid ((<>))
import Data.Greskell.GraphSON (GraphSONTyped(..))
import Data.Greskell.Greskell
  ( Greskell, unsafeGreskellLazy,
    toGremlin, toGremlinLazy, unsafeMethodCall, unsafeFunCall
  )

-- | @java.util.function.Predicate@ interface.
class Predicate p where
  type PredicateArg p
  pAnd :: Greskell p -> Greskell p -> Greskell p
  pAnd p1 p2 = unsafeMethodCall p1 "and" [toGremlin p2]
  pOr :: Greskell p -> Greskell p -> Greskell p
  pOr o1 o2 = unsafeMethodCall o1 "or" [toGremlin o2]
  pTest :: Greskell p -> Greskell (PredicateArg p) -> Greskell Bool
  pTest p arg = unsafeMethodCall p "test" [toGremlin arg]
  pNegate :: Greskell p -> Greskell p
  pNegate p = unsafeMethodCall p "negate" []

-- | Type for anonymous class of @Predicate@ interface.
newtype PredicateA a = PredicateA { unPredicateA :: a -> Bool }

instance Predicate (PredicateA a) where
  type PredicateArg (PredicateA a) = a

-- | @org.apache.tinkerpop.gremlin.process.traversal.P@ class.
--
-- @P a@ keeps data of type @a@ and compare it with data of type @a@
-- given as the Predicate argument.
data P a

instance Predicate (P a) where
  type PredicateArg (P a) = a

instance GraphSONTyped (P a) where
  gsonTypeFor _ = "g:P"

-- | @P.not@ static method.
pNot :: Greskell (P a) -> Greskell (P a)
pNot a = unsafeFunCall "not" [toGremlin a]

-- | @P.eq@ static method.
pEq :: Greskell a -> Greskell (P a)
pEq arg = unsafeFunCall "eq" [toGremlin arg]

-- | @P.neq@ static method.
pNeq :: Greskell a -> Greskell (P a)
pNeq arg = unsafeFunCall "neq" [toGremlin arg]

-- | @P.lt@ static method.
pLt :: Greskell a -> Greskell (P a)
pLt arg = unsafeFunCall "lt" [toGremlin arg]

-- | @P.lte@ static method.
pLte :: Greskell a -> Greskell (P a)
pLte arg = unsafeFunCall "lte" [toGremlin arg]

-- | @P.gt@ static method.
pGt :: Greskell a -> Greskell (P a)
pGt arg = unsafeFunCall "gt" [toGremlin arg]

-- | @P.gte@ static method.
pGte :: Greskell a -> Greskell (P a)
pGte arg = unsafeFunCall "gte" [toGremlin arg]

-- | @P.inside@ static method.
pInside :: Greskell a -> Greskell a -> Greskell (P a)
pInside a b = unsafeFunCall "inside" $ map toGremlin [a, b]

-- | @P.outside@ static method.
pOutside :: Greskell a -> Greskell a -> Greskell (P a)
pOutside a b = unsafeFunCall "outside" $ map toGremlin [a, b]

-- | @P.between@ static method.
pBetween :: Greskell a -> Greskell a -> Greskell (P a)
pBetween a b = unsafeFunCall "between" $ map toGremlin [a, b]

-- | @P.within@ static method.
pWithin :: [Greskell a] -> Greskell (P a)
pWithin = unsafeFunCall "within" . map toGremlin

-- | @P.without@ static method.
pWithout :: [Greskell a] -> Greskell (P a)
pWithout = unsafeFunCall "without" . map toGremlin

-- | @java.util.Comparator@ interface.
class Comparator c where
  type CompareArg c
  cCompare :: Greskell c -> Greskell (CompareArg c) -> Greskell (CompareArg c) -> Greskell Int
  cCompare cmp a b = unsafeMethodCall cmp "compare" $ map toGremlin [a, b]
  cReversed :: Greskell c -> Greskell c
  cReversed cmp = unsafeMethodCall cmp "reversed" []
  cThenComparing :: Greskell c -> Greskell c -> Greskell c
  cThenComparing cmp1 cmp2 = unsafeMethodCall cmp1 "thenComparing" [toGremlin cmp2]

-- | Type for anonymous class of @Comparator@ interface.
newtype ComparatorA a = ComparatorA { unComparatorA :: a -> a -> Int }

instance Comparator (ComparatorA a) where
  type CompareArg (ComparatorA a) = a

-- | org.apache.tinkerpop.gremlin.process.traversal.Order enum.
data Order a

-- | @Order a@ compares the type @a@.
instance Comparator (Order a) where
  type CompareArg (Order a) = a

instance GraphSONTyped (Order a) where
  gsonTypeFor _ = "g:Order"

-- | @decr@ order.
oDecr :: Greskell (Order a)
oDecr = unsafeGreskellLazy "decr"

-- | @incr@ order.
oIncr :: Greskell (Order a)
oIncr = unsafeGreskellLazy "incr"

-- | @shuffle@ order.
oShuffle :: Greskell (Order a)
oShuffle = unsafeGreskellLazy "shuffle"
