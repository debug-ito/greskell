{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts #-}
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
         PLike(..),
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
         testExamples_Gremlin
       ) where

import Data.Aeson (Value)
import Data.Greskell.GraphSON (GraphSONTyped(..))
import Data.Greskell.Greskell
  ( Greskell, unsafeGreskellLazy, string,
    toGremlin, toGremlinLazy, unsafeMethodCall, unsafeFunCall,
    ToGreskell
  )
import Data.Monoid ((<>))
import Data.Text (Text)

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

-- | Type that is compatible with 'P'. You can construct a value of
-- type @Greskell p@ using values of @PParameter p@.
--
-- Note that the type of constuctor arguments (i.e. @GreskellReturn (PParameter p)@)
-- should implement Java's @Comparable@ interface. This is true for most types,
-- so greskell doesn't have any explicit constraint about it.
--
-- @since 1.2.0.0
class (ToGreskell (PParameter p)) => PLike p where
  type PParameter p

-- | You can construct @Greskell (P a)@ from @Greskell a@.
instance PLike (P a) where
  type PParameter (P a) = Greskell a

-- | @P.not@ static method.
pNot :: PLike p => Greskell p -> Greskell p
pNot a = unsafeFunCall "P.not" [toGremlin a]

-- | @P.eq@ static method.
pEq :: PLike p => PParameter p -> Greskell p
pEq arg = unsafeFunCall "P.eq" [toGremlin arg]

-- | @P.neq@ static method.
pNeq :: PLike p => PParameter p -> Greskell p
pNeq arg = unsafeFunCall "P.neq" [toGremlin arg]

-- | @P.lt@ static method.
pLt :: PLike p => PParameter p -> Greskell p
pLt arg = unsafeFunCall "P.lt" [toGremlin arg]

-- | @P.lte@ static method.
pLte :: PLike p => PParameter p -> Greskell p
pLte arg = unsafeFunCall "P.lte" [toGremlin arg]

-- | @P.gt@ static method.
pGt :: PLike p => PParameter p -> Greskell p
pGt arg = unsafeFunCall "P.gt" [toGremlin arg]

-- | @P.gte@ static method.
pGte :: PLike p => PParameter p -> Greskell p
pGte arg = unsafeFunCall "P.gte" [toGremlin arg]

-- | @P.inside@ static method.
pInside :: PLike p => PParameter p -> PParameter p -> Greskell p
pInside a b = unsafeFunCall "P.inside" $ map toGremlin [a, b]

-- | @P.outside@ static method.
pOutside :: PLike p => PParameter p -> PParameter p -> Greskell p
pOutside a b = unsafeFunCall "P.outside" $ map toGremlin [a, b]

-- | @P.between@ static method.
pBetween :: PLike p => PParameter p -> PParameter p -> Greskell p
pBetween a b = unsafeFunCall "P.between" $ map toGremlin [a, b]

-- | @P.within@ static method.
pWithin :: PLike p => [PParameter p] -> Greskell p
pWithin = unsafeFunCall "P.within" . map toGremlin

-- | @P.without@ static method.
pWithout :: PLike p => [PParameter p] -> Greskell p
pWithout = unsafeFunCall "P.without" . map toGremlin

-- | @java.util.Comparator@ interface.
--
-- 'Comparator' compares two data of type 'CompareArg' @c@.
class Comparator c where
  type CompareArg c
  -- | @.compare@ method.
  cCompare :: Greskell c -> Greskell (CompareArg c) -> Greskell (CompareArg c) -> Greskell Int
  cCompare cmp a b = unsafeMethodCall cmp "compare" $ map toGremlin [a, b]
  -- | @.reversed@ method.
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
oDecr :: Greskell (Order a)
oDecr = unsafeGreskellLazy "Order.decr"

-- | @incr@ order.
oIncr :: Greskell (Order a)
oIncr = unsafeGreskellLazy "Order.incr"

-- | @shuffle@ order.
oShuffle :: Greskell (Order a)
oShuffle = unsafeGreskellLazy "Order.shuffle"

-- | Examples of using this module. See the source. The 'fst' of the output is the testee, while the
-- 'snd' is the expectation.
testExamples_Gremlin :: [(Text, Text)]
testExamples_Gremlin =
  [ (toGremlin (pNot $ pEq $ 10 :: Greskell (P Int)), "P.not(P.eq(10))")
  , (toGremlin (pEq $ string "hoge" :: Greskell (P Text)), "P.eq(\"hoge\")")
  , (toGremlin (pInside 10 20 :: Greskell (P Int)), "P.inside(10,20)")
  , (toGremlin (pWithin ["foo", "bar", "hoge"] :: Greskell (P Text)), "P.within(\"foo\",\"bar\",\"hoge\")")
  , (toGremlin oDecr, "Order.decr")
  ]
