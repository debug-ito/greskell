{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, GADTs #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- |
-- Module: Data.Greskell.GTraversal
-- Description: Gremlin traversal/step types.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
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
         vertices,
         vertices',
         edges,
         edges',
         -- * GTraversal
         (&.),
         ($.),
         unsafeGTraversal,
         -- * Walk/Steps
         unsafeWalk,
         -- ** Filter steps
         gIdentity,
         gIdentity',
         gFilter,
         -- gHas,
         -- gHas',
         gHasLabel,
         gHasLabel',
         gHasId,
         gHasId',
         gOr,
         gAnd,
         gNot,
         -- ** Size limitation steps
         -- gOrderBy,
         gRange,
         -- ** Transformation steps
         gFlatMap,
         gValues,
         -- ** Graph traversal steps
         gOut,
         gOut',
         gOutE,
         gOutE',
         gIn,
         gIn',
         gInE,
         gInE',
         -- * Types for @.by@ step
         ByProjection,
         pjValue,
         pjValue',
         ByComparator(ByComp)
       ) where

import Prelude hiding (or, filter, not)
import Control.Category (Category)
-- (below) to import Category methods without conflict with Prelude
import qualified Control.Category as Category
import Data.Aeson (Value)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Monoid ((<>), mconcat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Void (Void)
import Data.Greskell.Graph
  ( Element(..), Vertex, Edge, AesonVertex, AesonEdge
  )
import Data.Greskell.Gremlin (Comparator)
import Data.Greskell.Greskell
  ( Greskell, ToGreskell(..), unsafeGreskellLazy, unsafeGreskell, unsafeFunCall,
    toGremlinLazy, toGremlin
  )


-- | newtype wrapper of 'Greskell' 'GraphTraversal'.
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

-- | @GraphTraversal@ class object of TinkerPop. It takes data @s@
-- from upstream and emits data @e@ to downstream. Type @c@ is a
-- marker to describe the effect of the traversal.
--
-- 'GraphTraversal' is NOT a 'Category'. Because a @GraphTraversal@
-- object keeps some context data, the starting (left-most)
-- @GraphTraversal@ object controls most of the behavior of entire
-- composition of traversals and steps. This violates 'Category' law.
data GraphTraversal c s e = GraphTraversal
                          deriving (Show)
                                  
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
-- expression, 'Walk' is not a 'Greskell'.
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

-- | 'id' is 'gIdentity''.
instance WalkType c => Category (Walk c) where
  id = gIdentity'
  (Walk bc) . (Walk ab) = Walk (ab <> bc)

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



-- | Class of phantom type markers to describe the feature of the
-- walk/traversals.
class WalkType t

-- | WalkType for filtering steps.
--
-- A filtering step is a step that does filtering only. It takes input
-- and outputs some of them without any modification, reordering,
-- traversal actions, or side-effects. Filtering decision must be
-- solely based on each element.
--
-- A 'Walk' @s@ is 'Filter' type iff:
--
-- > (gSideEffect s == gIdentity) AND (gFilter s == s)
--
-- If 'Walk's @s1@ and @s2@ are 'Filter' type, then
-- 
-- > gAnd [s1, s2] == s1 >>> s2 == s2 >>> s1
data Filter

instance WalkType Filter

-- | WalkType for steps without any side-effects. This includes
-- transformations, reordring, injections and graph traversal actions.
--
-- A 'Walk' @s@ is 'Transform' type iff:
--
-- > gSideEffect s == gIdentity
--
-- Obviously, every 'Filter' type 'Walk's are also 'Transform' type.
data Transform

instance WalkType Transform

-- | WalkType for steps may have side-effects.
--
-- A side-effect here means manipulation of the \"sideEffect\" in
-- Gremlin context (i.e. a stash of data kept in a Traversal object),
-- as well as interaction with the world outside the Traversal object.
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
source :: Text -- ^ variable name of 'GraphTraversalSource'
       -> Greskell GraphTraversalSource
source = unsafeGreskell

sourceMethod :: Text -> [Greskell a] -> Greskell GraphTraversalSource -> Greskell b
sourceMethod method_name args src =
  unsafeGreskellLazy $ (toGremlinLazy src <> methodCallText method_name (map toGremlin args))

-- | @.V()@ method on 'GraphTraversalSource'.
vertices :: [Greskell Value] -- ^ vertex IDs
         -> Greskell GraphTraversalSource
         -> GTraversal Transform Void AesonVertex
vertices = vertices'

-- | Polymorphic version of 'vertices'.
vertices' :: Vertex v
          => [Greskell (ElementID v)]
          -> Greskell GraphTraversalSource
          -> GTraversal Transform Void v
vertices' ids src = GTraversal $ sourceMethod "V" ids src

-- | @.E()@ method on 'GraphTraversalSource'.
edges :: [Greskell Value] -- ^ edge IDs
      -> Greskell GraphTraversalSource
      -> GTraversal Transform Void AesonEdge
edges = edges'

-- | Polymorphic version of 'edges'.
edges' :: Edge e
       => [Greskell (ElementID e)]
       -> Greskell GraphTraversalSource
       -> GTraversal Transform Void e
edges' ids src = GTraversal $ sourceMethod "E" ids src

unsafeGTraversal :: Text -> GTraversal c s e
unsafeGTraversal = GTraversal . unsafeGreskell

infixl 1 &.

-- | Apply the 'Walk' to the 'GTraversal'. In Gremlin, this means
-- calling a chain of methods on the Traversal object.
(&.) :: GTraversal c a b -> Walk c b d -> GTraversal c a d
(GTraversal gt) &. (Walk twalk) = GTraversal $ unsafeGreskellLazy (toGremlinLazy gt <> twalk)

infixr 0 $.

-- | Same as '(&.)' with arguments flipped.
($.) :: Walk c b d -> GTraversal c a b -> GTraversal c a d
gs $. gt = gt &. gs

methodCallText :: Text -- ^ method name
               -> [Text] -- ^ args
               -> TL.Text
methodCallText name args = ("." <>) $ toGremlinLazy $ unsafeFunCall name args

-- | Unsafely create a 'Walk' that represents a single method call on
-- a @GraphTraversal@.
unsafeWalk :: WalkType c
           => Text -- ^ step method name (e.g. "outE")
           -> [Text] -- ^ step method arguments
           -> Walk c s e
unsafeWalk name args = Walk $ methodCallText name args

-- | @.identity@ step.
gIdentity :: Walk Filter s s
gIdentity = unsafeWalk "identity" []

-- | Polymorphic version of 'gIdentity'.
gIdentity' :: WalkType c => Walk c s s
gIdentity' = liftWalk $ gIdentity

travToG :: (ToGTraversal g, WalkType c) => g c s e -> Text
travToG = toGremlin . unGTraversal . toGTraversal

-- | @.filter@ step with steps(traversal).
gFilter :: (ToGTraversal g, WalkType c, WalkType p, Split c p) => g c s e -> Walk p s s
gFilter walk = unsafeWalk "filter" [travToG walk]

-- TODO: ElementID typeとPredicateをなんとか考えるべき。

-- -- | @.has@ step.
-- --
-- -- TODO: @.has@ step has some overloaded behaviors.
-- gHas :: (Element s)
--      => Greskell -- ^ key
--      -> Greskell -- ^ expectation (value or predicate expression)
--      -> Walk Filter s s
-- gHas target expec = unsafeWalk "has" [target, expec]

-- -- | Polymorphic version of 'gHas'.
-- gHas' :: (Element s, WalkType c) => Greskell -> Greskell -> Walk c s s
-- gHas' t e = liftWalk $ gHas t e

-- | @.hasLabel@ step
gHasLabel :: Element s
          => [Greskell Text] -- ^ expected label names
          -> Walk Filter s s
gHasLabel = unsafeWalk "hasLabel" . map toGremlin

-- | Polymorphic version of 'gHasLabel'.
gHasLabel' :: (Element s, WalkType c) => [Greskell Text] -> Walk c s s
gHasLabel' = liftWalk . gHasLabel

-- | @.hasId@ step
gHasId :: Element s
       => [Greskell (ElementID s)] -- ^ expected IDs
       -> Walk Filter s s
gHasId = unsafeWalk "hasId" . map toGremlin

-- | Polymorphic version of 'gHasId'.
gHasId' :: (Element s, WalkType c) => [Greskell (ElementID s)] -> Walk c s s
gHasId' = liftWalk . gHasId

multiLogic :: (ToGTraversal g, WalkType c, WalkType p, Split c p)
           => Text -- ^ method name
           -> [g c s e]
           -> Walk p s s
multiLogic method_name = unsafeWalk method_name . map travToG

-- | @.and@ step.
gAnd :: (ToGTraversal g, WalkType c, WalkType p, Split c p) => [g c s e] -> Walk p s s
gAnd = multiLogic "and"

-- | @.or@ step.
gOr :: (ToGTraversal g, WalkType c, WalkType p, Split c p) => [g c s e] -> Walk p s s
gOr = multiLogic "or"

-- | @.not@ step.
gNot :: (ToGTraversal g, WalkType c, WalkType p, Split c p) => g c s e -> Walk p s s
gNot cond = unsafeWalk "not" [travToG cond]

-- | @.range@ step.
gRange :: Greskell Int
       -- ^ min
       -> Greskell Int
       -- ^ max
       -> Walk Transform s s
gRange min_g max_g = unsafeWalk "range" $ map toGremlin [min_g, max_g]


-- | TBW.
data ByProjection s e where
  BPEmpty :: ByProjection s s
  BPTraversal :: (ToGTraversal g) => g Transform s e -> ByProjection s e
  BPValue :: (Element e) => Greskell Text -> ByProjection e a
  BPFunction :: Greskell (a -> b) -> ByProjection a b

pjValue :: (Element e) => Greskell Text -> ByProjection e Value
pjValue = BPValue

pjValue' :: (Element e) => Greskell Text -> ByProjection e a
pjValue' = BPValue

-- | TBW.
data ByComparator s where
  ByComp :: ByProjection s e -> Greskell (Comparator e) -> ByComparator s



-- -- | @.order@ and @.by@ steps
-- gOrderBy :: (ToGTraversal g)
--          => [(g Transform s e, Greskell)]
--          -- ^ (accessor step, comparator) of each @.by@ step
--          -> Walk Transform s s
-- gOrderBy bys = unsafeWalk (methodCall "order" [] <> bys_g)
--   where
--     bys_g = mconcat $ map toG bys
--     toG (accessor, comparator) =
--       methodCall "by" [(toGreskell $ toGTraversal accessor), comparator]

-- | @.flatMap@ step.
--
-- @.flatMap@ step is a 'Transform' step even if the child walk is
-- 'Filter' type. This is because @.flatMap@ step always modifies the
-- path of the Traverser.
gFlatMap :: (ToGTraversal g) => g Transform s e -> Walk Transform s e
gFlatMap gt = unsafeWalk "flatMap" [travToG gt]

-- -- | Polymorphic version of 'gFlatMap'. The following constraint is
-- -- accurate and semantic, but it's not allowed even if
-- -- FlexibleContexts is enabled. Probably it's because the type @m@ is
-- -- left ambiguous.
-- gFlatMap' :: (ToGTraversal g, Split c m, Lift Transform p, Lift m p) => g c s e -> Walk p s e
-- gFlatMap = undefined


-- | @.values@ step.
gValues :: Element s
        => [Greskell Text]
        -- ^ property keys
        -> Walk Transform s e
gValues = unsafeWalk "values" . map toGremlin

genericTraversalWalk :: Vertex v => Text -> [Greskell Text] -> Walk Transform v e
genericTraversalWalk method_name = unsafeWalk method_name . map toGremlin

-- | @.out@ step
gOut :: (Vertex v)
     => [Greskell Text] -- ^ edge labels
     -> Walk Transform v AesonVertex
gOut = gOut'

-- | Polymorphic version of 'gOut'.
gOut' :: (Vertex v1, Vertex v2) => [Greskell Text] -> Walk Transform v1 v2
gOut' = genericTraversalWalk "out"

-- | @.outE@ step
gOutE :: (Vertex v)
      => [Greskell Text] -- ^ edge labels
      -> Walk Transform v AesonEdge
gOutE = gOutE'

-- | Polymorphic version of 'gOutE'
gOutE' :: (Vertex v, Edge e) => [Greskell Text] -> Walk Transform v e
gOutE' = genericTraversalWalk "outE"

-- | @.in@ step
gIn :: (Vertex v)
    => [Greskell Text] -- ^ edge labels
    -> Walk Transform v AesonVertex
gIn = gIn'

-- | Polymorphic version of 'gIn'.
gIn' :: (Vertex v1, Vertex v2) => [Greskell Text] -> Walk Transform v1 v2
gIn' = genericTraversalWalk "in"

-- | @.inE@ step.
gInE :: (Vertex v)
     => [Greskell Text] -- ^ edge labels
     -> Walk Transform v AesonEdge
gInE = gInE

-- | Polymorphic version of 'gInE'.
gInE' :: (Vertex v, Edge e) => [Greskell Text] -> Walk Transform v e
gInE' = genericTraversalWalk "inE"

---- -- probably we can implement .as() step like this. GBuilder generates
---- -- some 'Label', which is passed to .as() step and can be passed later
---- -- to .select() step etc.
---- gAs :: GBuilder (Label, Walk Filter s s)
---- gAs = undefined
