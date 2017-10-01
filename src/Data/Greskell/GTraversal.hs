{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- |
-- Module: Data.Greskell.GTraversal
-- Description: Gremlin traversal/step types.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.GTraversal
       ( -- * Types
         -- ** GraphTraversals and Steps
         GTraversal(..),
         GraphTraversal,
         ToGTraversal(..),
         Step,
         GraphTraversalSource,
         -- ** Step types
         StepType,
         Filter,
         Transform,
         SideEffect,
         Lift,
         Split,
         -- * GraphTraversalSource
         source,
         -- vertices,
         -- vertices',
         -- edges,
         -- edges',
         -- * GTraversal
         (&.),
         ($.),
         unsafeGTraversal,
         -- * Step
         unsafeStep,
         -- ** Filter step
         gIdentity,
         gIdentity',
         gFilter,
         -- gHas,
         -- gHas',
         gHasLabel,
         gHasLabel',
         -- gHasId,
         -- gHasId',
         gOr,
         gAnd,
         gNot,
         -- ** Transformation step
         -- gOrderBy,
         gRange,
         gFlatMap,
         gValues,
         -- ** Graph traversal step
         gOut,
         gOut',
         gOutE,
         gOutE',
         gIn,
         gIn',
         gInE,
         gInE'
       ) where

import Prelude hiding (or, filter, not)
import Control.Category (Category)
-- (below) to import Category methods without conflict with Prelude
import qualified Control.Category as Category
import Data.Bifunctor (Bifunctor(bimap))
import Data.Monoid ((<>), mconcat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Void (Void)
import Data.Greskell.Graph
  ( Element, Vertex, Edge, GVertex, GEdge
  )
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
  toGTraversal :: StepType c => g c s e -> GTraversal c s e
  liftType :: (StepType from, StepType to, Lift from to) => g from s e -> g to s e
  -- ^ Lift 'StepType' @from@ to @to@. Use this for type matching.

instance ToGTraversal GTraversal where
  toGTraversal = id
  liftType (GTraversal g) = GTraversal $ unsafeGreskellLazy $ toGremlinLazy g


-- | A Gremlin step. Like 'GTraversal', type @s@ is the input, type
-- @e@ is the output, and type @c@ is a marker to describe the step.
--
-- This is NOT the Gremlin's @Step@ class object, but it's a method
-- call such as @.has(x)@ and @.outE()@.
--
-- 'Step' is a 'Category'. You can use functions from
-- "Control.Category" to compose 'Step's. This is equivalent to making
-- a chain of method calls in Gremlin.
--
-- 'Step' is not an 'Eq', because it's difficult to define true
-- equality between Gremlin method calls. If we define it naively, it
-- might have conflict with 'Category' law.
newtype Step c s e = Step TL.Text
                    deriving (Show)

-- | 'id' is 'gIdentity''.
instance StepType c => Category (Step c) where
  id = gIdentity'
  (Step bc) . (Step ab) = Step (ab <> bc)

-- | Unsafely convert output type
instance Functor (Step c s) where
  fmap _ (Step t) = Step t

-- | Unsafely convert input and output types.
instance Bifunctor (Step c) where
  bimap _ _ (Step t) = Step t

-- | To convert a 'Step' to 'GTraversal', it calls its static method
-- version on @__@ class.
instance ToGTraversal Step where
  toGTraversal (Step t) = GTraversal $ unsafeGreskellLazy ("__" <> t)
  liftType (Step t) = Step t



-- | Class of phantom type markers to describe the feature of the
-- step/traversal.
class StepType t

-- | StepType for filtering steps.
--
-- A filtering step is a step that does filtering only. It takes input
-- and outputs some of them without any modification, reordering,
-- traversal actions, or side-effects. Filtering decision must be
-- solely based on each element.
--
-- A 'Step' @s@ is 'Filter' type iff:
--
-- > (gSideEffect s == gIdentity) AND (gFilter s == s)
--
-- If 'Step's @s1@ and @s2@ are 'Filter' type, then
-- 
-- > gAnd [s1, s2] == s1 >>> s2 == s2 >>> s1
data Filter

instance StepType Filter

-- | StepType for steps without any side-effects. This includes
-- transformations, reordring, injections and graph traversal actions.
--
-- A 'Step' @s@ is 'Transform' type iff:
--
-- > gSideEffect s == gIdentity
--
-- Obviously, every 'Filter' type 'Step's are also 'Transform' type.
data Transform

instance StepType Transform

-- | StepType for steps may have side-effects.
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

instance StepType SideEffect

-- | Relation of 'StepType's where one includes the other. @from@ can
-- be lifted to @to@, because @to@ is more powerful than @from@.
class Lift from to

instance (StepType c) => Lift Filter c
instance Lift Transform Transform
instance Lift Transform SideEffect
instance Lift SideEffect SideEffect

-- | Relation of 'StepType's where the child step @c@ is split from
-- the parent step @p@.
class Split c p

instance (StepType p) => Split Filter p
instance (StepType p) => Split Transform p
-- ^ 'Transform' effect in the child step is rolled back in the parent
-- step.
instance Split SideEffect SideEffect
-- ^ 'SideEffect' in the child step remains in the parent step.


-- | @GraphTraversalSource@ class object of TinkerPop. It is a factory
-- object of 'GraphTraversal's.
data GraphTraversalSource = GraphTraversalSource
                          deriving (Show)


-- | Create 'GTraversalSource' from a varible name in Gremlin
source :: Text -- ^ variable name of @GraphTraversalSource@
       -> Greskell GraphTraversalSource
source = unsafeGreskell

-- TODO: IDの型を制約しないといけない。

-- -- | @.V()@ method on @GraphTraversalSource@.
-- vertices :: [Greskell] -- ^ vertex IDs
--          -> GTraversalSource
--          -> GTraversal Transform Void GVertex
-- vertices = vertices'
-- 
-- -- | Polymorphic version of 'vertices'.
-- vertices' :: Vertex v
--           => [Greskell]
--           -> GTraversalSource
--           -> GTraversal Transform Void v
-- vertices' ids src = GTraversal (unGTraversalSource src <> methodCall "V" ids)
-- 
-- -- | @.E()@ method on @GraphTraversalSource@.
-- edges :: [Greskell] -- ^ edge IDs
--       -> GTraversalSource -> GTraversal Transform Void GEdge
-- edges = edges'
-- 
-- -- | Polymorphic version of 'edges'.
-- edges' :: Edge e => [Greskell] -> GTraversalSource -> GTraversal Transform Void e
-- edges' ids src = GTraversal (unGTraversalSource src <> methodCall "E" ids)

unsafeGTraversal :: Text -> GTraversal c s e
unsafeGTraversal = GTraversal . unsafeGreskell

infixl 1 &.

-- | Apply the 'Step' to the 'GTraversal'. In Gremlin, this means
-- calling a chain of methods on the Traversal object.
(&.) :: GTraversal c a b -> Step c b d -> GTraversal c a d
(GTraversal gt) &. (Step tstep) = GTraversal $ unsafeGreskellLazy (toGremlinLazy gt <> tstep)

infixr 0 $.

-- | Same as '(&.)' with arguments flipped.
($.) :: Step c b d -> GTraversal c a b -> GTraversal c a d
gs $. gt = gt &. gs

-- | Unsafely create a step.
unsafeStep :: StepType c
           => Text -- ^ step method name (e.g. "outE")
           -> [Text] -- ^ step method arguments
           -> Step c s e
unsafeStep name args = Step $ ("." <>) $ toGremlinLazy $ unsafeFunCall name args

-- | @.identity@ step.
gIdentity :: Step Filter s s
gIdentity = unsafeStep "identity" []

-- | Polymorphic version of 'gIdentity'.
gIdentity' :: StepType c => Step c s s
gIdentity' = liftType $ gIdentity

travToG :: (ToGTraversal g, StepType c) => g c s e -> Text
travToG = toGremlin . unGTraversal . toGTraversal

-- | @.filter@ step with steps(traversal).
gFilter :: (ToGTraversal g, StepType c, StepType p, Split c p) => g c s e -> Step p s s
gFilter step = unsafeStep "filter" [travToG step]

-- TODO: ElementID typeとPredicateをなんとか考えるべき。

-- -- | @.has@ step.
-- --
-- -- TODO: @.has@ step has some overloaded behaviors.
-- gHas :: (Element s)
--      => Greskell -- ^ key
--      -> Greskell -- ^ expectation (value or predicate expression)
--      -> Step Filter s s
-- gHas target expec = unsafeStep "has" [target, expec]

-- -- | Polymorphic version of 'gHas'.
-- gHas' :: (Element s, StepType c) => Greskell -> Greskell -> Step c s s
-- gHas' t e = liftType $ gHas t e

-- | @.hasLabel@ step
gHasLabel :: Element s
          => [Greskell Text] -- ^ expected label names
          -> Step Filter s s
gHasLabel = unsafeStep "hasLabel" . map toGremlin

-- | Polymorphic version of 'gHasLabel'.
gHasLabel' :: (Element s, StepType c) => [Greskell Text] -> Step c s s
gHasLabel' = liftType . gHasLabel

-- TODO: ID type

-- -- | @.hasId@ step
-- gHasId :: Element s
--        => [Greskell] -- ^ expected IDs
--        -> Step Filter s s
-- gHasId = unsafeStep . methodCall "hasId"

-- -- | Polymorphic version of 'gHasId'.
-- gHasId' :: (Element s, StepType c) => [Greskell] -> Step c s s
-- gHasId' = liftType . gHasId

multiLogic :: (ToGTraversal g, StepType c, StepType p, Split c p)
           => Text -- ^ method name
           -> [g c s e]
           -> Step p s s
multiLogic method_name = unsafeStep method_name . map travToG

-- | @.and@ step.
gAnd :: (ToGTraversal g, StepType c, StepType p, Split c p) => [g c s e] -> Step p s s
gAnd = multiLogic "and"

-- | @.or@ step.
gOr :: (ToGTraversal g, StepType c, StepType p, Split c p) => [g c s e] -> Step p s s
gOr = multiLogic "or"

-- | @.not@ step.
gNot :: (ToGTraversal g, StepType c, StepType p, Split c p) => g c s e -> Step p s s
gNot cond = unsafeStep "not" [travToG cond]

-- | @.range@ step.
gRange :: Greskell Int
       -- ^ min
       -> Greskell Int
       -- ^ max
       -> Step Transform s s
gRange min_g max_g = unsafeStep "range" $ map toGremlin [min_g, max_g]

-- TODO: Comparator type

-- -- | @.order@ and @.by@ steps
-- gOrderBy :: (ToGTraversal g)
--          => [(g Transform s e, Greskell)]
--          -- ^ (accessor steps, comparator) of each @.by@
--          -> Step Transform s s
-- gOrderBy bys = unsafeStep (methodCall "order" [] <> bys_g)
--   where
--     bys_g = mconcat $ map toG bys
--     toG (accessor, comparator) =
--       methodCall "by" [(toGreskell $ toGTraversal accessor), comparator]

-- | @.flatMap@ step.
--
-- @.flatMap@ step is a 'Transform' step even if the child step is
-- 'Filter' type. This is because @.flatMap@ step always modifies the
-- path of the Traverser.
gFlatMap :: (ToGTraversal g) => g Transform s e -> Step Transform s e
gFlatMap gt = unsafeStep "flatMap" [travToG gt]

-- -- | Polymorphic version of 'gFlatMap'. The following constraint is
-- -- accurate and semantic, but it's not allowed even if
-- -- FlexibleContexts is enabled. Probably it's because the type @m@ is
-- -- left ambiguous.
-- gFlatMap' :: (ToGTraversal g, Split c m, Lift Transform p, Lift m p) => g c s e -> Step p s e
-- gFlatMap = undefined


-- | @.values@ step.
gValues :: Element s
        => [Greskell Text]
        -- ^ property keys
        -> Step Transform s e
gValues = unsafeStep "values" . map toGremlin

genericTraversalStep :: Vertex v => Text -> [Greskell Text] -> Step Transform v e
genericTraversalStep method_name = unsafeStep method_name . map toGremlin

-- | @.out@ step
gOut :: (Vertex v)
     => [Greskell Text] -- ^ edge labels
     -> Step Transform v GVertex
gOut = gOut'

-- | Polymorphic version of 'gOut'.
gOut' :: (Vertex v1, Vertex v2) => [Greskell Text] -> Step Transform v1 v2
gOut' = genericTraversalStep "out"

-- | @.outE@ step
gOutE :: (Vertex v)
      => [Greskell Text] -- ^ edge labels
      -> Step Transform v GEdge
gOutE = gOutE'

-- | Polymorphic version of 'gOutE'
gOutE' :: (Vertex v, Edge e) => [Greskell Text] -> Step Transform v e
gOutE' = genericTraversalStep "outE"

-- | @.in@ step
gIn :: (Vertex v)
    => [Greskell Text] -- ^ edge labels
    -> Step Transform v GVertex
gIn = gIn'

-- | Polymorphic version of 'gIn'.
gIn' :: (Vertex v1, Vertex v2) => [Greskell Text] -> Step Transform v1 v2
gIn' = genericTraversalStep "in"

-- | @.inE@ step.
gInE :: (Vertex v)
     => [Greskell Text] -- ^ edge labels
     -> Step Transform v GEdge
gInE = gInE

-- | Polymorphic version of 'gInE'.
gInE' :: (Vertex v, Edge e) => [Greskell Text] -> Step Transform v e
gInE' = genericTraversalStep "inE"

---- -- probably we can implement .as() step like this. GBuilder generates
---- -- some 'Label', which is passed to .as() step and can be passed later
---- -- to .select() step etc.
---- gAs :: GBuilder (Label, Step Filter s s)
---- gAs = undefined
