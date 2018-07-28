{-# LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeFamilies, GADTs #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- |
-- Module: Data.Greskell.GTraversal
-- Description: Gremlin traversal/step types.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This module defines 'GTraversal', greskell counterpart of
-- @GraphTraversal@ class object, and a DSL of composing graph
-- traversal steps.
module Data.Greskell.GTraversal
       ( -- * Types
         -- ** GraphTraversal and others
         GTraversal(..),
         GraphTraversal,
         ToGTraversal(..),
         Walk,
         GraphTraversalSource,
         -- ** Walk types
         WalkType,
         Filter,
         Transform,
         SideEffect,
         Lift,
         Split,
         -- * GraphTraversalSource
         source,
         sV,
         sV',
         sE,
         sE',
         sAddV,
         sAddV',
         -- * GTraversal
         (&.),
         ($.),
         unsafeGTraversal,
         -- * Walk/Steps

         -- |
         -- Functions for TinkerPop graph traversal steps.
         -- __For now greskell does not cover all graph traversal steps.__
         -- If you want some steps added, just open an issue.
         --
         -- There may be multiple versions of Haskell functions for a
         -- single step. This is because Gremlin steps are too
         -- polymorphic for Haskell. greskell should be type-safe so
         -- that incorrect combination of steps is detected in compile
         -- time.

         -- ** Low-level functions
         unsafeWalk,
         modulateWith,
         -- ** Filter steps
         gIdentity,
         gIdentity',
         gFilter,
         -- ** Has steps
         gHas1,
         gHas1',
         gHas2,
         gHas2',
         gHas2P,
         gHas2P',
         gHasLabel,
         gHasLabel',
         gHasLabelP,
         gHasLabelP',
         gHasId,
         gHasId',
         gHasIdP,
         gHasIdP',
         gHasKey,
         gHasKey',
         gHasKeyP,
         gHasKeyP',
         gHasValue,
         gHasValue',
         gHasValueP,
         gHasValueP',
         -- ** Logic steps
         gAnd,
         gOr,
         gNot,
         -- ** Sorting steps
         gOrder,
         -- ** Paging steps
         gRange,
         -- ** Transformation steps
         gFlatMap,
         gV,
         gV',
         -- ** Accessor steps
         gValues,
         gProperties,
         gId,
         gLabel,
         -- ** Summarizing steps
         gFold,
         gCount,
         -- ** Graph traversal steps
         gOut,
         gOut',
         gOutE,
         gOutE',
         gIn,
         gIn',
         gInE,
         gInE',
         -- ** Side-effect steps
         gSideEffect,
         gSideEffect',
         -- ** Graph manipulation steps
         gAddV,
         gAddV',
         gAddE,
         gAddE',
         AddAnchor,
         gFrom,
         gTo,
         gDrop,
         gDropP,
         gProperty,
         gPropertyV,
         -- ** @.by@ steps
         
         -- | @.by@ steps are not 'Walk' on their own because they are
         -- always used in conjunction with other steps like 'gOrder'.
         ByProjection(..),
         ProjectionLike(..),
         ByComparator(..),
         gBy,
         gBy1,
         gBy2
       ) where

import Control.Category (Category, (>>>))
-- (below) to import Category methods without conflict with Prelude
import qualified Control.Category as Category
import Data.Aeson (Value)
import Data.Bifunctor (Bifunctor(bimap))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid ((<>), mconcat, Monoid(..))
import Data.Semigroup (Semigroup, sconcat)
import qualified Data.Semigroup as Semigroup
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Greskell.Graph
  ( Element(..), Vertex, Edge, Property(..),
    AVertex, AEdge,
    T, Key, Cardinality,
    KeyValue(..)
  )
import Data.Greskell.GraphSON (GValue)
import Data.Greskell.Gremlin
  ( Comparator(..),
    P
  )
import Data.Greskell.Greskell
  ( Greskell, ToGreskell(..), unsafeGreskellLazy, unsafeGreskell, unsafeFunCall,
    toGremlinLazy, toGremlin
  )
import Data.Greskell.AsIterator (AsIterator(IteratorItem))

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Function ((&))
-- >>> import Data.Greskell.Greskell (gvalueInt)
-- >>> import Data.Greskell.Gremlin (pBetween, pEq, pLte, oDecr, oIncr)
-- >>> import Data.Greskell.Graph (tId, cList, (=:), AVertex, AVertexProperty)
-- >>> import Data.Greskell.GraphSON (GValueBody(..))

-- | @GraphTraversal@ class object of TinkerPop. It takes data @s@
-- from upstream and emits data @e@ to downstream. Type @c@ is called
-- \"walk type\", a marker to describe the effect of the traversal.
--
-- 'GTraversal' is NOT a 'Category'. Because a @GraphTraversal@ object
-- keeps some context data, the starting (left-most) @GraphTraversal@
-- object controls most of the behavior of entire composition of
-- traversals and steps. This violates 'Category' law.
newtype GTraversal c s e = GTraversal { unGTraversal :: Greskell (GraphTraversal c s e) }
                         deriving (Show)

-- | Unsafely convert output type.
instance Functor (GTraversal c s) where
  fmap f (GTraversal g) = GTraversal $ fmap (fmap f) g

-- | Unsafely convert input and output types.
instance Bifunctor (GTraversal c) where
  bimap f1 f2 (GTraversal g) = GTraversal $ fmap (bimap f1 f2) g

-- | Unwrap 'GTraversal' data constructor.
instance ToGreskell (GTraversal c s e) where
  type GreskellReturn (GTraversal c s e) = GraphTraversal c s e
  toGreskell = unGTraversal

-- | Phantom type for @GraphTraversal@ class. In greskell, we usually
-- use 'GTraversal' instead of 'Greskell' 'GraphTraversal'.
data GraphTraversal c s e = GraphTraversal
                          deriving (Show)

-- | 'GraphTraversal' is an Iterator.
instance AsIterator (GraphTraversal c s e) where
  type IteratorItem (GraphTraversal c s e) = e

-- | Unsafely convert output type.
instance Functor (GraphTraversal c s) where
  fmap _ GraphTraversal = GraphTraversal

-- | Unsafely convert input and output types.
instance Bifunctor (GraphTraversal c) where
  bimap _ _ GraphTraversal = GraphTraversal

-- | Types that can convert to 'GTraversal'.
class ToGTraversal g where
  toGTraversal :: WalkType c => g c s e -> GTraversal c s e
  liftWalk :: (WalkType from, WalkType to, Lift from to) => g from s e -> g to s e
  -- ^ Lift 'WalkType' @from@ to @to@. Use this for type matching.

instance ToGTraversal GTraversal where
  toGTraversal = id
  liftWalk (GTraversal g) = GTraversal $ unsafeGreskellLazy $ toGremlinLazy g


-- | A chain of one or more Gremlin steps. Like 'GTraversal', type @s@
-- is the input, type @e@ is the output, and type @c@ is a marker to
-- describe the step.
--
-- 'Walk' represents a chain of method calls such as
-- @.has(x).outE()@. Because this is not a Gremlin (Groovy)
-- expression, we use bare 'Walk', not 'Greskell' 'Walk'.
--
-- 'Walk' is a 'Category'. You can use functions from
-- "Control.Category" to compose 'Walk's. This is equivalent to making
-- a chain of method calls in Gremlin.
--
-- 'Walk' is not an 'Eq', because it's difficult to define true
-- equality between Gremlin method calls. If we define it naively, it
-- might have conflict with 'Category' law.
newtype Walk c s e = Walk TL.Text
                    deriving (Show)

-- | 'id' is 'gIdentity'.
instance WalkType c => Category (Walk c) where
  id = gIdentity
  (Walk bc) . (Walk ab) = Walk (ab <> bc)

-- | Based on 'Category'. 'Semigroup.<>' is 'Category.>>>'.
instance WalkType c => Semigroup (Walk c s s) where
  (<>) = (Category.>>>)

-- | Based on 'Category' and 'Semigroup'. 'mempty' is 'Category.id'.
instance WalkType c => Monoid (Walk c s s) where
  mempty = Category.id
  mappend = (Semigroup.<>)

-- | Unsafely convert output type
instance Functor (Walk c s) where
  fmap _ (Walk t) = Walk t

-- | Unsafely convert input and output types.
instance Bifunctor (Walk c) where
  bimap _ _ (Walk t) = Walk t

-- | To convert a 'Walk' to 'GTraversal', it calls its static method
-- version on @__@ class.
instance ToGTraversal Walk where
  toGTraversal (Walk t) = GTraversal $ unsafeGreskellLazy ("__" <> t)
  liftWalk (Walk t) = Walk t

-- | The 'Walk' is first converted to 'GTraversal', and it's converted
-- to 'Greskell'.
instance WalkType c => ToGreskell (Walk c s e) where
  type GreskellReturn (Walk c s e) = GraphTraversal c s e
  toGreskell = toGreskell . toGTraversal

-- | Class of phantom type markers to describe the effect of the
-- walk/traversals.
class WalkType t

-- | WalkType for filtering steps.
--
-- A filtering step is a step that does filtering only. It takes input
-- and emits some of them without any modification, reordering,
-- traversal actions, or side-effects. Filtering decision must be
-- solely based on each element.
--
-- A 'Walk' @w@ is 'Filter' type iff:
--
-- > (gSideEffect w == gIdentity) AND (gFilter w == w)
--
-- If 'Walk's @w1@ and @w2@ are 'Filter' type, then
-- 
-- > gAnd [w1, w2] == w1 >>> w2 == w2 >>> w1
data Filter

instance WalkType Filter

-- | WalkType for steps without any side-effects. This includes
-- transformations, reordring, injections and graph traversal actions.
--
-- A 'Walk' @w@ is 'Transform' type iff:
--
-- > gSideEffect w == gIdentity
--
-- Obviously, every 'Filter' type 'Walk's are also 'Transform' type.
data Transform

instance WalkType Transform

-- | WalkType for steps that may have side-effects.
--
-- A side-effect here means manipulation of the \"sideEffect\" in
-- Gremlin context (i.e. the stash of data kept in a Traversal
-- object), as well as interaction with the world outside the
-- Traversal object.
--
-- For example, the following steps (in Gremlin) all have
-- side-effects.
--
-- > .addE('label')
-- > .aggregate('x')
-- > .sideEffect(System.out.&println)
-- > .map { some_variable += 1 }
data SideEffect

instance WalkType SideEffect

-- | Relation of 'WalkType's where one includes the other. @from@ can
-- be lifted to @to@, because @to@ is more powerful than @from@.
class Lift from to

instance (WalkType c) => Lift Filter c
instance Lift Transform Transform
instance Lift Transform SideEffect
instance Lift SideEffect SideEffect

-- | Relation of 'WalkType's where the child walk @c@ is split from
-- the parent walk @p@.
--
-- When splitting, transformation effect done in the child walk is
-- rolled back (canceled) in the parent walk.
class Split c p

instance (WalkType p) => Split Filter p
instance (WalkType p) => Split Transform p
-- ^ 'Transform' effect in the child walk is rolled back in the parent
-- walk.
instance Split SideEffect SideEffect
-- ^ 'SideEffect' in the child walk remains in the parent walk.


-- | @GraphTraversalSource@ class object of TinkerPop. It is a factory
-- object of 'GraphTraversal's.
data GraphTraversalSource = GraphTraversalSource
                          deriving (Show)


-- | Create 'GraphTraversalSource' from a varible name in Gremlin
--
-- >>> toGremlin $ source "g"
-- "g"
source :: Text -- ^ variable name of 'GraphTraversalSource'
       -> Greskell GraphTraversalSource
source = unsafeGreskell

sourceMethod :: Text -> [Greskell a] -> Greskell GraphTraversalSource -> Greskell b
sourceMethod method_name args src =
  unsafeGreskellLazy $ (toGremlinLazy src <> methodCallText method_name (map toGremlin args))

-- | @.V()@ method on 'GraphTraversalSource'.
sV :: Vertex v
   => [Greskell (ElementID v)] -- ^ vertex IDs
   -> Greskell GraphTraversalSource
   -> GTraversal Transform () v
sV ids src = GTraversal $ sourceMethod "V" ids src

-- | Monomorphic version of 'sV'.
--
-- >>> toGremlin (source "g" & sV' (map gvalueInt ([1,2,3] :: [Int])))
-- "g.V(1,2,3)"
sV' :: [Greskell GValue]
    -> Greskell GraphTraversalSource
    -> GTraversal Transform () AVertex
sV' = sV

-- | @.E()@ method on 'GraphTraversalSource'.
sE :: Edge e
   => [Greskell (ElementID e)] -- ^ edge IDs
   -> Greskell GraphTraversalSource
   -> GTraversal Transform () e
sE ids src = GTraversal $ sourceMethod "E" ids src

-- | Monomorphic version of 'sE'.
--
-- >>> toGremlin (source "g" & sE' (map gvalueInt ([1] :: [Int])))
-- "g.E(1)"
sE' :: [Greskell GValue]
       -> Greskell GraphTraversalSource
       -> GTraversal Transform () AEdge
sE' = sE

-- | @.addV()@ method on 'GraphTraversalSource'.
--
-- @since 0.2.0.0
sAddV :: Vertex v
      => Greskell Text -- ^ vertex label
      -> Greskell GraphTraversalSource
      -> GTraversal SideEffect () v
sAddV label src = GTraversal $ sourceMethod "addV" [label] src

-- | Monomorphic version of 'sAddV'.
--
-- >>> toGremlin (source "g" & sAddV' "person")
-- "g.addV(\"person\")"
--
-- @since 0.2.0.0
sAddV' :: Greskell Text -> Greskell GraphTraversalSource -> GTraversal SideEffect () AVertex
sAddV' = sAddV

-- | Unsafely create 'GTraversal' from the given raw Gremlin script.
--
-- >>> toGremlin $ unsafeGTraversal "g.V().count()"
-- "g.V().count()"
unsafeGTraversal :: Text -> GTraversal c s e
unsafeGTraversal = GTraversal . unsafeGreskell

infixl 1 &.

-- | Apply the 'Walk' to the 'GTraversal'. In Gremlin, this means
-- calling a chain of methods on the Traversal object.
--
-- >>> toGremlin (source "g" & sV' [] &. gValues ["age"])
-- "g.V().values(\"age\")"
(&.) :: GTraversal c a b -> Walk c b d -> GTraversal c a d
(GTraversal gt) &. (Walk twalk) = GTraversal $ unsafeGreskellLazy (toGremlinLazy gt <> twalk)

infixr 0 $.

-- | Same as '&.' with arguments flipped.
--
-- >>> toGremlin (gValues ["age"] $. sV' [] $ source "g")
-- "g.V().values(\"age\")"
($.) :: Walk c b d -> GTraversal c a b -> GTraversal c a d
gs $. gt = gt &. gs

-- -- $walk-steps
-- --

methodCallText :: Text -- ^ method name
               -> [Text] -- ^ args
               -> TL.Text
methodCallText name args = ("." <>) $ toGremlinLazy $ unsafeFunCall name args

-- | Unsafely create a 'Walk' that represents a single method call on
-- a @GraphTraversal@.
--
-- >>> toGremlin (source "g" & sV' [] &. unsafeWalk "valueMap" ["'foo'", "'bar'"])
-- "g.V().valueMap('foo','bar')"
unsafeWalk :: WalkType c
           => Text -- ^ step method name (e.g. "outE")
           -> [Text] -- ^ step method arguments
           -> Walk c s e
unsafeWalk name args = Walk $ methodCallText name args

-- | Optionally modulate the main 'Walk' with some modulating 'Walk's.
--
-- >>> toGremlin (source "g" & sV' [] &. modulateWith (unsafeWalk "path" []) [unsafeWalk "by" ["'name'"], unsafeWalk "by" ["'age'"]])
-- "g.V().path().by('name').by('age')"
modulateWith :: (WalkType c)
             => Walk c s e -- ^ the main 'Walk'
             -> [Walk c e e] -- ^ the modulating 'Walk's
             -> Walk c s e
modulateWith w [] = w
modulateWith w (m:rest) = w >>> sconcat (m :| rest)

-- | @.identity@ step.
gIdentity :: WalkType c => Walk c s s
gIdentity = liftWalk $ gIdentity'

-- | Monomorphic version of 'gIdentity'.
gIdentity' :: Walk Filter s s
gIdentity' = unsafeWalk "identity" []

travToG :: (ToGTraversal g, WalkType c) => g c s e -> Text
travToG = toGremlin . unGTraversal . toGTraversal

-- | @.filter@ step that takes a traversal.
--
-- >>> toGremlin (source "g" & sV' [] &. gFilter (gOut' ["knows"]))
-- "g.V().filter(__.out(\"knows\"))"
gFilter :: (ToGTraversal g, WalkType c, WalkType p, Split c p) => g c s e -> Walk p s s
gFilter walk = unsafeWalk "filter" [travToG walk]


-- TODO
-- gValueMap, gProperty etc. should use Key type as an argument.
-- Note that .property step is very tricky. Read the doc carefully.
-- 

-- | @.has@ step with one argument.
--
-- >>> toGremlin (source "g" & sV' [] &. gHas1 "age")
-- "g.V().has(\"age\")"
gHas1 :: (WalkType c, Element s)
      => Key s v -- ^ property key
      -> Walk c s s
gHas1 = liftWalk . gHas1'

-- | Monomorphic version of 'gHas1'.
gHas1' :: (Element s) => Key s v -> Walk Filter s s
gHas1' key = unsafeWalk "has" [toGremlin key]

-- | @.has@ step with two arguments.
--
-- >>> toGremlin (source "g" & sV' [] &. gHas2 "age" (31 :: Greskell Int))
-- "g.V().has(\"age\",31)"
gHas2 :: (WalkType c, Element s) => Key s v -> Greskell v -> Walk c s s
gHas2 k v = liftWalk $ gHas2' k v

-- | Monomorphic verson of 'gHas2'.
gHas2' :: (Element s) => Key s v -> Greskell v -> Walk Filter s s
gHas2' k v = unsafeWalk "has" [toGremlin k, toGremlin v]

-- | @.has@ step with two arguments and 'P' type.
--
-- >>> toGremlin (source "g" & sV' [] &. gHas2P "age" (pBetween (30 :: Greskell Int) 40))
-- "g.V().has(\"age\",P.between(30,40))"
gHas2P :: (WalkType c, Element s)
       => Key s v -- ^ property key
       -> Greskell (P v) -- ^ predicate on the property value
       -> Walk c s s
gHas2P k p = liftWalk $ gHas2P' k p

-- | Monomorphic version of 'gHas2P'.
gHas2P' :: (Element s) => Key s v -> Greskell (P v) -> Walk Filter s s
gHas2P' key p = unsafeWalk "has" [toGremlin key, toGremlin p]

-- TODO: has(Key,Traversal), has(Label,Key,P)

-- | @.hasLabel@ step.
--
-- >>> toGremlin (source "g" & sV' [] &. gHasLabel "person")
-- "g.V().hasLabel(\"person\")"
gHasLabel :: (Element s, WalkType c) => Greskell Text -> Walk c s s
gHasLabel = liftWalk . gHasLabel'

-- | Monomorphic version of 'gHasLabel'.
gHasLabel' :: (Element s) => Greskell Text -> Walk Filter s s
gHasLabel' l = unsafeWalk "hasLabel" [toGremlin l]

-- | @.hasLabel@ step with 'P' type. Supported since TinkerPop 3.2.7.
--
-- >>> toGremlin (source "g" & sV' [] &. gHasLabelP (pEq "person"))
-- "g.V().hasLabel(P.eq(\"person\"))"
gHasLabelP :: (Element s, WalkType c)
           => Greskell (P Text) -- ^ predicate on Element label.
           -> Walk c s s
gHasLabelP = liftWalk . gHasLabelP'

-- | Monomorphic version of 'gHasLabelP'.
gHasLabelP' :: Element s
            => Greskell (P Text)
            -> Walk Filter s s
gHasLabelP' p = unsafeWalk "hasLabel" [toGremlin p]

-- | @.hasId@ step.
--
-- >>> toGremlin (source "g" & sV' [] &. gHasId (gvalueInt $ (7 :: Int)))
-- "g.V().hasId(7)"
gHasId :: (Element s, WalkType c) => Greskell (ElementID s) -> Walk c s s
gHasId = liftWalk . gHasId'

-- | Monomorphic version of 'gHasId'.
gHasId' :: Element s => Greskell (ElementID s) -> Walk Filter s s
gHasId' i = unsafeWalk "hasId" [toGremlin i]

-- | @.hasId@ step with 'P' type. Supported since TinkerPop 3.2.7.
--
-- >>> toGremlin (source "g" & sV' [] &. gHasIdP (pLte $ gvalueInt (100 :: Int)))
-- "g.V().hasId(P.lte(100))"
gHasIdP :: (Element s, WalkType c)
        => Greskell (P (ElementID s))
        -> Walk c s s
gHasIdP = liftWalk . gHasIdP'

-- | Monomorphic version of 'gHasIdP'.
gHasIdP' :: Element s
         => Greskell (P (ElementID s))
         -> Walk Filter s s
gHasIdP' p = unsafeWalk "hasId" [toGremlin p]

-- | @.hasKey@ step. The input type should be a VertexProperty.
--
-- >>> toGremlin (source "g" & sV' [] &. gProperties [] &. gHasKey "age")
-- "g.V().properties().hasKey(\"age\")"
gHasKey :: (Element (p v), Property p, WalkType c) => Greskell Text -> Walk c (p v) (p v)
gHasKey = liftWalk . gHasKey'

-- | Monomorphic version of 'gHasKey'.
gHasKey' :: (Element (p v), Property p) => Greskell Text -> Walk Filter (p v) (p v)
gHasKey' k = unsafeWalk "hasKey" [toGremlin k]

-- | @.hasKey@ step with 'P' type. Supported since TinkerPop 3.2.7.
gHasKeyP :: (Element (p v), Property p, WalkType c)
         => Greskell (P Text) -- ^ predicate on the VertexProperty's key.
         -> Walk c (p v) (p v)
gHasKeyP = liftWalk . gHasKeyP'

-- | Monomorphic version of 'gHasKeyP'.
gHasKeyP' :: (Element (p v), Property p) => Greskell (P Text) -> Walk Filter (p v) (p v)
gHasKeyP' p = unsafeWalk "hasKey" [toGremlin p]

-- | @.hasValue@ step. The input type should be a VertexProperty.
--
-- >>> toGremlin (source "g" & sV' [] &. gProperties ["age"] &. gHasValue (32 :: Greskell Int))
-- "g.V().properties(\"age\").hasValue(32)"
gHasValue :: (Element (p v), Property p, WalkType c) => Greskell v -> Walk c (p v) (p v)
gHasValue = liftWalk . gHasValue'

-- | Monomorphic version of 'gHasValue'.
gHasValue' :: (Element (p v), Property p) => Greskell v -> Walk Filter (p v) (p v)
gHasValue' v = unsafeWalk "hasValue" [toGremlin v]

-- | @.hasValue@ step with 'P' type. Supported since TinkerPop 3.2.7.
--
-- >>> toGremlin (source "g" & sV' [] &. gProperties ["age"] &. gHasValueP (pBetween (30 :: Greskell Int) 40))
-- "g.V().properties(\"age\").hasValue(P.between(30,40))"
gHasValueP :: (Element (p v), Property p, WalkType c)
           => Greskell (P v) -- ^ predicate on the VertexProperty's value
           -> Walk c (p v) (p v)
gHasValueP = liftWalk . gHasValueP'

-- | Monomorphic version of 'gHasValueP'.
gHasValueP' :: (Element (p v), Property p) => Greskell (P v) -> Walk Filter (p v) (p v)
gHasValueP' p = unsafeWalk "hasValue" [toGremlin p]

multiLogic :: (ToGTraversal g, WalkType c, WalkType p, Split c p)
           => Text -- ^ method name
           -> [g c s e]
           -> Walk p s s
multiLogic method_name = unsafeWalk method_name . map travToG

-- | @.and@ step.
--
-- >>> toGremlin (source "g" & sV' [] &. gAnd [gOut' ["knows"], gHas1 "age"])
-- "g.V().and(__.out(\"knows\"),__.has(\"age\"))"
gAnd :: (ToGTraversal g, WalkType c, WalkType p, Split c p) => [g c s e] -> Walk p s s
gAnd = multiLogic "and"

-- | @.or@ step.
--
-- >>> toGremlin (source "g" & sV' [] &. gOr [gOut' ["knows"], gHas1 "age"])
-- "g.V().or(__.out(\"knows\"),__.has(\"age\"))"
gOr :: (ToGTraversal g, WalkType c, WalkType p, Split c p) => [g c s e] -> Walk p s s
gOr = multiLogic "or"

-- | @.not@ step.
--
-- >>> toGremlin (source "g" & sV' [] &. gNot (gOut' ["knows"]))
-- "g.V().not(__.out(\"knows\"))"
gNot :: (ToGTraversal g, WalkType c, WalkType p, Split c p) => g c s e -> Walk p s s
gNot cond = unsafeWalk "not" [travToG cond]

-- | @.range@ step. This step is not a 'Filter', because the filtering
-- decision by this step is based on position of each element, not the
-- element itself. This violates 'Filter' law.
--
-- >>> toGremlin (source "g" & sV' [] &. gRange 0 100)
-- "g.V().range(0,100)"
gRange :: Greskell Int
       -- ^ min
       -> Greskell Int
       -- ^ max
       -> Walk Transform s s
gRange min_g max_g = unsafeWalk "range" $ map toGremlin [min_g, max_g]

-- | Data types that mean a projection from one type to another.
class ProjectionLike p where
  type ProjectionLikeStart p
  -- ^ The start type of the projection.
  type ProjectionLikeEnd p
  -- ^ The end type of the projection.

instance ProjectionLike (Walk Filter s e) where
  type ProjectionLikeStart (Walk Filter s e) = s
  type ProjectionLikeEnd (Walk Filter s e) = e

instance ProjectionLike (GTraversal Filter s e) where
  type ProjectionLikeStart (GTraversal Filter s e) = s
  type ProjectionLikeEnd (GTraversal Filter s e) = e

instance ProjectionLike (Greskell (GraphTraversal Filter s e)) where
  type ProjectionLikeStart (Greskell (GraphTraversal Filter s e)) = s
  type ProjectionLikeEnd (Greskell (GraphTraversal Filter s e)) = e

instance ProjectionLike (Walk Transform s e) where
  type ProjectionLikeStart (Walk Transform s e) = s
  type ProjectionLikeEnd (Walk Transform s e) = e

instance ProjectionLike (GTraversal Transform s e) where
  type ProjectionLikeStart (GTraversal Transform s e) = s
  type ProjectionLikeEnd (GTraversal Transform s e) = e

instance ProjectionLike (Greskell (GraphTraversal Transform s e)) where
  type ProjectionLikeStart (Greskell (GraphTraversal Transform s e)) = s
  type ProjectionLikeEnd (Greskell (GraphTraversal Transform s e)) = e

instance ProjectionLike (Key s e) where
  type ProjectionLikeStart (Key s e) = s
  type ProjectionLikeEnd (Key s e) = e

instance ProjectionLike (Greskell (T s e)) where
  type ProjectionLikeStart (Greskell (T s e)) = s
  type ProjectionLikeEnd (Greskell (T s e)) = e

instance ProjectionLike (Greskell (s -> e)) where
  type ProjectionLikeStart (Greskell (s -> e)) = s
  type ProjectionLikeEnd (Greskell (s -> e)) = e

instance ProjectionLike (ByProjection s e) where
  type ProjectionLikeStart (ByProjection s e) = s
  type ProjectionLikeEnd (ByProjection s e) = e

-- | Projection from type @s@ to type @e@ used in @.by@ step. You can
-- also use 'gBy' to construct 'ByProjection'.
data ByProjection s e where
  ByProjection :: (ProjectionLike p, ToGreskell p) => p -> ByProjection (ProjectionLikeStart p) (ProjectionLikeEnd p)

-- | Projection by literal property key.
instance IsString (ByProjection s e) where
  fromString = ByProjection . toKey
    where
      toKey :: String -> Key s e
      toKey = fromString

-- | @.by@ step with 1 argument, used for projection.
gBy :: (ProjectionLike p, ToGreskell p) => p -> ByProjection (ProjectionLikeStart p) (ProjectionLikeEnd p)
gBy = ByProjection 

-- | Comparison of type @s@ used in @.by@ step. You can also use
-- 'gBy1' and 'gBy2' to construct 'ByComparator'.
data ByComparator s where
  -- | Type @s@ is projected to type @e@, and compared by the natural
  -- comparator of type @e@.
  ByComparatorProj :: ByProjection s e -> ByComparator s
  
  -- | Type @s@ is compared by the 'Comparator' @comp@.
  ByComparatorComp :: Comparator comp => Greskell comp -> ByComparator (CompareArg comp)
  
  -- | Type @s@ is projected to type @CompareArg comp@, and compared
  -- by the 'Comparator' @comp@.
  ByComparatorProjComp :: Comparator comp => ByProjection s (CompareArg comp) -> Greskell comp -> ByComparator s

-- | 'ByComparatorProj' by literal property key.
instance IsString (ByComparator s) where
  fromString = ByComparatorProj . fromString

-- | @.by@ step with 1 argument, used for comparison.
gBy1 :: (ProjectionLike p, ToGreskell p) => p -> ByComparator (ProjectionLikeStart p)
gBy1 = ByComparatorProj . gBy

-- | @.by@ step with 2 arguments, used for comparison.
gBy2 :: (ProjectionLike p, ToGreskell p, Comparator comp, ProjectionLikeEnd p ~ CompareArg comp)
     => p
     -> Greskell comp
     -> ByComparator (ProjectionLikeStart p)
gBy2 p c = ByComparatorProjComp (gBy p) c

-- | @.order@ step.
--
-- >>> let key_age = ("age" :: Key AVertex Int)
-- >>> toGremlin (source "g" & sV' [] &. gOrder [gBy1 key_age])
-- "g.V().order().by(\"age\")"
-- >>> toGremlin (source "g" & sV' [] &. gOrder [gBy2 key_age oDecr, gBy1 tId])
-- "g.V().order().by(\"age\",Order.decr).by(T.id)"
-- >>> toGremlin (source "g" & sV' [] &. gOrder [gBy2 (gOut' ["knows"] >>> gCount) oIncr, gBy2 tId oIncr])
-- "g.V().order().by(__.out(\"knows\").count(),Order.incr).by(T.id,Order.incr)"
--
-- 'ByComparator' is an 'IsString', meaning projection by the given
-- key.
--
-- >>> toGremlin (source "g" & sV' [] &. gOrder ["age"])
-- "g.V().order().by(\"age\")"
gOrder :: [ByComparator s] -- ^ following @.by@ steps.
       -> Walk Transform s s
gOrder bys = modulateWith order_step by_steps
  where
    order_step = unsafeWalk "order" []
    by_steps = map (unsafeWalk "by" . toByArgs) bys
    toByArgs :: ByComparator s -> [Text]
    toByArgs bc = case bc of
      ByComparatorProj (ByProjection p) -> [toGremlin p]
      ByComparatorComp comp -> [toGremlin comp]
      ByComparatorProjComp (ByProjection p) comp -> [toGremlin p, toGremlin comp]

-- | @.flatMap@ step.
--
-- @.flatMap@ step is a 'Transform' step even if the child walk is
-- 'Filter' type. This is because @.flatMap@ step always modifies the
-- path of the Traverser.
--
-- >>> toGremlin (source "g" & sV' [] &. gFlatMap (gOut' ["knows"] >>> gOut' ["created"]))
-- "g.V().flatMap(__.out(\"knows\").out(\"created\"))"
gFlatMap :: (ToGTraversal g) => g Transform s e -> Walk Transform s e
gFlatMap gt = unsafeWalk "flatMap" [travToG gt]

-- -- | Polymorphic version of 'gFlatMap'. The following constraint is
-- -- accurate and semantic, but it's not allowed even if
-- -- FlexibleContexts is enabled. Probably it's because the type @m@ is
-- -- left ambiguous.
-- gFlatMap' :: (ToGTraversal g, Split c m, Lift Transform p, Lift m p) => g c s e -> Walk p s e
-- gFlatMap = undefined

-- | @.V@ step.
--
-- For each input item, @.V@ step emits vertices selected by the
-- argument (or all vertices if the empty list is passed.)
--
-- @since 0.2.0.0
gV :: Vertex v => [Greskell (ElementID v)] -> Walk Transform s v
gV ids = unsafeWalk "V" $ map toGremlin ids

-- | Monomorphic version of 'gV'.
--
-- @since 0.2.0.0
gV' :: [Greskell GValue] -> Walk Transform s AVertex
gV' = gV

-- | @.values@ step.
--
-- >>> toGremlin (source "g" & sV' [] &. gValues ["name", "age"])
-- "g.V().values(\"name\",\"age\")"
gValues :: Element s
        => [Key s e]
        -- ^ property keys
        -> Walk Transform s e
gValues = unsafeWalk "values" . map toGremlin

-- | @.properties@ step.
--
-- >>> toGremlin (source "g" & sV' [] &. gProperties ["age"])
-- "g.V().properties(\"age\")"
gProperties :: (Element s, Property p, ElementProperty s ~ p)
            => [Key s v]
            -> Walk Transform s (p v)
gProperties = unsafeWalk "properties" . map toGremlin

-- | @.id@ step.
--
-- @since 0.2.1.0
gId :: Element s => Walk Transform s (ElementID s)
gId = unsafeWalk "id" []

-- | @.label@ step.
--
-- @since 0.2.1.0
gLabel :: Element s => Walk Transform s Text
gLabel = unsafeWalk "label" []

-- | @.fold@ step.
gFold :: Walk Transform a [a]
gFold = unsafeWalk "fold" []

-- | @.count@ step.
gCount :: Walk Transform a Int
gCount = unsafeWalk "count" []

genericTraversalWalk :: Vertex v => Text -> [Greskell Text] -> Walk Transform v e
genericTraversalWalk method_name = unsafeWalk method_name . map toGremlin

-- | @.out@ step
gOut :: (Vertex v1, Vertex v2)
     => [Greskell Text] -- ^ edge labels
     -> Walk Transform v1 v2
gOut = genericTraversalWalk "out"

-- | Monomorphic version of 'gOut'.
--
-- >>> toGremlin (source "g" & sV' [gvalueInt (8 :: Int)] &. gOut' ["knows"])
-- "g.V(8).out(\"knows\")"
gOut' :: (Vertex v)
      => [Greskell Text]
      -> Walk Transform v AVertex
gOut' = gOut

-- | @.outE@ step
gOutE :: (Vertex v, Edge e)
      => [Greskell Text] -- ^ edge labels
      -> Walk Transform v e
gOutE = genericTraversalWalk "outE"

-- | Monomorphic version of 'gOutE'
gOutE' :: (Vertex v)
       => [Greskell Text]
       -> Walk Transform v AEdge
gOutE' = gOutE

-- | @.in@ step
gIn :: (Vertex v1, Vertex v2)
    => [Greskell Text] -- ^ edge labels
    -> Walk Transform v1 v2
gIn = genericTraversalWalk "in"

-- | Monomorphic version of 'gIn'.
gIn' :: (Vertex v)
     => [Greskell Text]
     -> Walk Transform v AVertex
gIn' = gIn

-- | @.inE@ step.
gInE :: (Vertex v, Edge e)
     => [Greskell Text] -- ^ edge labels
     -> Walk Transform v e
gInE = genericTraversalWalk "inE"

-- | Monomorphic version of 'gInE'.
gInE' :: (Vertex v)
      => [Greskell Text] -- ^ edge labels
      -> Walk Transform v AEdge
gInE' = gInE

-- | @.sideEffect@ step that takes a traversal.
gSideEffect :: (ToGTraversal g, WalkType c, WalkType p, Split c p) => g c s e -> Walk p s s
gSideEffect walk = unsafeWalk "sideEffect" [travToG walk]

-- | Monomorphic version of 'gSideEffect'. The result walk is always
-- 'SideEffect' type.
--
-- >>> toGremlin (source "g" & sV' [] & liftWalk &. gHas2 "name" "marko" &. gSideEffect' (gAddV' "toshio"))
-- "g.V().has(\"name\",\"marko\").sideEffect(__.addV(\"toshio\"))"
gSideEffect' :: (ToGTraversal g, WalkType c, Split c SideEffect) => g c s e -> Walk SideEffect s s
gSideEffect' w = gSideEffect w

---- -- probably we can implement .as() step like this. GBuilder generates
---- -- some 'Label', which is passed to .as() step and can be passed later
---- -- to .select() step etc.
---- gAs :: GBuilder (Label, Walk Filter s s)
---- gAs = undefined

-- | @.addV@ step with a label.
gAddV :: Vertex v => Greskell Text -> Walk SideEffect a v
gAddV label = unsafeWalk "addV" [toGremlin label]

-- | Monomorphic version of 'gAddV'.
gAddV' :: Greskell Text -> Walk SideEffect a AVertex
gAddV' = gAddV

-- | @.drop@ step on 'Element'.
-- 
-- >>> toGremlin (source "g" & sV' [] &. gHas2 "name" "marko" & liftWalk &. gDrop)
-- "g.V().has(\"name\",\"marko\").drop()"
gDrop :: Element e => Walk SideEffect e e
gDrop = unsafeWalk "drop" []

-- | @.drop@ step on 'Property'.
--
-- >>> toGremlin (source "g" & sE' [] &. gProperties ["weight"] & liftWalk &. gDropP)
-- "g.E().properties(\"weight\").drop()"
gDropP :: Property p => Walk SideEffect (p a) (p a)
gDropP = unsafeWalk "drop" []

-- | simple @.property@ step. It adds a value to the property.
--
-- >>> toGremlin (source "g" & sV' [] & liftWalk &. gProperty "age" (20 :: Greskell Int))
-- "g.V().property(\"age\",20)"
--
-- @since 0.2.0.0
gProperty :: Element e
          => Key e v -- ^ key of the property
          -> Greskell v -- ^ value of the property
          -> Walk SideEffect e e
gProperty key val = unsafeWalk "property" [toGremlin key, toGremlin val]

-- | @.property@ step for 'Vertex'.
--
-- >>> let key_location = "location" :: Key AVertex Text
-- >>> let key_since = "since" :: Key (AVertexProperty Text) Text
-- >>> let key_score = "score" :: Key (AVertexProperty Text) Int
-- >>> toGremlin (source "g" & sV' [] & liftWalk &. gPropertyV (Just cList) key_location "New York" [key_since =: "2012-09-23", key_score =: 8])
-- "g.V().property(list,\"location\",\"New York\",\"since\",\"2012-09-23\",\"score\",8)"
--
-- @since 0.2.0.0
gPropertyV :: (Vertex e, vp ~ ElementProperty e, Property vp, Element (vp v))
           => Maybe (Greskell Cardinality) -- ^ optional cardinality of the vertex property.
           -> Key e v -- ^ key of the vertex property
           -> Greskell v -- ^ value of the vertex property
           -> [KeyValue (vp v)] -- ^ optional meta-properties for the vertex property.
           -> Walk SideEffect e e
gPropertyV mcard key val metaprops = unsafeWalk "property" (arg_card ++ arg_keyval ++ arg_metaprops)
  where
    arg_card = maybe [] (\card -> [toGremlin card]) mcard
    arg_keyval = [toGremlin key, toGremlin val]
    arg_metaprops = expand =<< metaprops
      where
        expand (KeyValue meta_key meta_val) = [toGremlin meta_key, toGremlin meta_val]

-- | Vertex anchor for 'gAddE'. It corresponds to @.from@ or @.to@
-- step following an @.addE@ step.
--
-- Type @s@ is the input Vertex for the @.addE@ step. Type @e@ is the
-- type of the anchor Vertex that the 'AddAnchor' yields. So, @.addE@
-- step creates an edge between @s@ and @e@.
--
-- @since 0.2.0.0
data AddAnchor s e = AddAnchor Text (GTraversal Transform s e)

anchorStep :: WalkType c => AddAnchor s e -> Walk c edge edge
anchorStep (AddAnchor step_name subtraversal) = unsafeWalk step_name [toGremlin subtraversal]

-- | @.from@ step with a traversal.
-- 
-- @since 0.2.0.0
gFrom :: (ToGTraversal g) => g Transform s e -> AddAnchor s e
gFrom = AddAnchor "from" . toGTraversal

-- | @.to@ step with a traversal.
--
-- @since 0.2.0.0
gTo :: (ToGTraversal g) => g Transform s e -> AddAnchor s e
gTo = AddAnchor "to" . toGTraversal

-- | @.addE@ step. Supported since TinkerPop 3.1.0.
--
-- >>> let key_name = "name" :: Key AVertex Text
-- >>> toGremlin (source "g" & sV' [] & liftWalk &. gAddE' "knows" (gFrom $ gV' [] >>> gHas2 key_name "marko"))
-- "g.V().addE(\"knows\").from(__.V().has(\"name\",\"marko\"))"
-- >>> toGremlin (source "g" & sV' [] &. gHas2 key_name "marko" & liftWalk &. gAddE' "knows" (gTo $ gV' []))
-- "g.V().has(\"name\",\"marko\").addE(\"knows\").to(__.V())"
-- 
-- @since 0.2.0.0
gAddE :: (Vertex vs, Vertex ve, Edge e)
      => Greskell Text
      -> AddAnchor vs ve
      -> Walk SideEffect vs e
gAddE label anch = (unsafeWalk "addE" [toGremlin label]) >>> anchorStep anch

-- | Monomorphic version of 'gAddE'
-- 
-- @since 0.2.0.0
gAddE' :: Greskell Text -> AddAnchor AVertex AVertex -> Walk SideEffect AVertex AEdge
gAddE' = gAddE

