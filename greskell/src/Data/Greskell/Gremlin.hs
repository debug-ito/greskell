{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
-- |
-- Module: Data.Greskell.Gremlin
-- Description: Gremlin (Groovy/Java) utility classes
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This modules defines types and functions for utility classes in
-- Gremlin.
module Data.Greskell.Gremlin
       ( -- * Predicate
         Predicate(..),
         PredicateA(..),
         -- ** P class
         P,
         PParameter(..),
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
         -- ** Order enum
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
    toGremlin, toGremlinLazy, unsafeMethodCall, unsafeFunCall,
    ToGreskell
  )

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Greskell.Greskell (number, string)

-- | @java.util.function.Predicate@ interface.
--
-- A 'Predicate' @p@ is a function that takes 'PredicateArg' @p@ and
-- returns 'Bool'.
class Predicate p where
  type PredicateArg p
  -- | @.and@ method.
  pAnd :: Greskell p -> Greskell p -> Greskell p
  pAnd p1 p2 = unsafeMethodCall p1 "and" [toGremlin p2]
  -- | @.or@ method.
  pOr :: Greskell p -> Greskell p -> Greskell p
  pOr o1 o2 = unsafeMethodCall o1 "or" [toGremlin o2]
  -- | @.test@ method.
  pTest :: Greskell p -> Greskell (PredicateArg p) -> Greskell Bool
  pTest p arg = unsafeMethodCall p "test" [toGremlin arg]
  -- | @.nagate@ method.
  pNegate :: Greskell p -> Greskell p
  pNegate p = unsafeMethodCall p "negate" []

-- | Type for anonymous class of @Predicate@ interface.
newtype PredicateA a = PredicateA { unPredicateA :: a -> Bool }

instance Predicate (PredicateA a) where
  type PredicateArg (PredicateA a) = a

-- | @org.apache.tinkerpop.gremlin.process.traversal.P@ class.
--
-- @P a@ keeps data of type @a@ and compares it with data of type @a@
-- given as the Predicate argument.
data P a

instance Predicate (P a) where
  type PredicateArg (P a) = a

instance GraphSONTyped (P a) where
  gsonTypeFor _ = "g:P"

-- | Type of parameters used to construct a 'P' type. By passing
-- values of type @p@, you can construct a value of @Greskell (P (PPredicateArg p))@,
-- where 'PPredicateArg' is a type family of @p@.
class ToGreskell p => PParameter p where
  type PPredicateArg p

instance PParameter (Greskell a) where
  type PPredicateArg (Greskell a) = a


-- | @P.not@ static method.
--
-- >>> toGremlin $ pNot $ pEq $ number 10
-- "P.not(P.eq(10.0))"
pNot :: Greskell (P a) -> Greskell (P a)
pNot a = unsafeFunCall "P.not" [toGremlin a]

-- | @P.eq@ static method.
--
-- >>> toGremlin $ pEq $ string "hoge"
-- "P.eq(\"hoge\")"
pEq :: PParameter p => p -> Greskell (P (PPredicateArg p))
pEq arg = unsafeFunCall "P.eq" [toGremlin arg]

-- | @P.neq@ static method.
pNeq :: PParameter p => p -> Greskell (P (PPredicateArg p))
pNeq arg = unsafeFunCall "P.neq" [toGremlin arg]

-- | @P.lt@ static method.
pLt :: PParameter p => p -> Greskell (P (PPredicateArg p))
pLt arg = unsafeFunCall "P.lt" [toGremlin arg]

-- | @P.lte@ static method.
pLte :: PParameter p => p -> Greskell (P (PPredicateArg p))
pLte arg = unsafeFunCall "P.lte" [toGremlin arg]

-- | @P.gt@ static method.
pGt :: PParameter p => p -> Greskell (P (PPredicateArg p))
pGt arg = unsafeFunCall "P.gt" [toGremlin arg]

-- | @P.gte@ static method.
pGte :: PParameter p => p -> Greskell (P (PPredicateArg p))
pGte arg = unsafeFunCall "P.gte" [toGremlin arg]

-- | @P.inside@ static method.
--
-- >>> toGremlin $ pInside (number 10) (number 20)
-- "P.inside(10.0,20.0)"
pInside :: PParameter p => p -> p -> Greskell (P (PPredicateArg p))
pInside a b = unsafeFunCall "P.inside" $ map toGremlin [a, b]

-- | @P.outside@ static method.
pOutside :: PParameter p => p -> p -> Greskell (P (PPredicateArg p))
pOutside a b = unsafeFunCall "P.outside" $ map toGremlin [a, b]

-- | @P.between@ static method.
pBetween :: PParameter p => p -> p -> Greskell (P (PPredicateArg p))
pBetween a b = unsafeFunCall "P.between" $ map toGremlin [a, b]

-- | @P.within@ static method.
--
-- >>> toGremlin $ pWithin (["foo", "bar", "hoge"] :: [Greskell String])
-- "P.within(\"foo\",\"bar\",\"hoge\")"
pWithin :: PParameter p => [p] -> Greskell (P (PPredicateArg p))
pWithin = unsafeFunCall "P.within" . map toGremlin

-- | @P.without@ static method.
pWithout :: PParameter p => [p] -> Greskell (P (PPredicateArg p))
pWithout = unsafeFunCall "P.without" . map toGremlin

-- | @java.util.Comparator@ interface.
--
-- 'Comparator' compares two data of type 'CompareArg' @c@.
class Comparator c where
  type CompareArg c
  -- | @.compare@ method.
  cCompare :: Greskell c -> Greskell (CompareArg c) -> Greskell (CompareArg c) -> Greskell Int
  cCompare cmp a b = unsafeMethodCall cmp "compare" $ map toGremlin [a, b]
  -- | @.reverse@ method.
  cReversed :: Greskell c -> Greskell c
  cReversed cmp = unsafeMethodCall cmp "reversed" []
  -- | @.thenComparing@ method.
  cThenComparing :: Greskell c -> Greskell c -> Greskell c
  cThenComparing cmp1 cmp2 = unsafeMethodCall cmp1 "thenComparing" [toGremlin cmp2]

-- | Type for anonymous class of @Comparator@ interface.
newtype ComparatorA a = ComparatorA { unComparatorA :: a -> a -> Int }

instance Comparator (ComparatorA a) where
  type CompareArg (ComparatorA a) = a

-- | @org.apache.tinkerpop.gremlin.process.traversal.Order@ enum.
data Order a

-- | @Order a@ compares the type @a@.
instance Comparator (Order a) where
  type CompareArg (Order a) = a

instance GraphSONTyped (Order a) where
  gsonTypeFor _ = "g:Order"

-- | @decr@ order.
--
-- >>> toGremlin oDecr
-- "Order.decr"
oDecr :: Greskell (Order a)
oDecr = unsafeGreskellLazy "Order.decr"

-- | @incr@ order.
oIncr :: Greskell (Order a)
oIncr = unsafeGreskellLazy "Order.incr"

-- | @shuffle@ order.
oShuffle :: Greskell (Order a)
oShuffle = unsafeGreskellLazy "Order.shuffle"
