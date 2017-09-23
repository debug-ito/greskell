{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- |
-- Module: Data.Greskell.GTraversal
-- Description: Gremlin traversal/step types.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.GTraversal
       ( -- * Types
         -- ** Gremlin Traversals and Steps
         GTraversal,
         ToGTraversal(..),
         Step,
         -- ** Step types
         StepType,
         Filter,
         Transform,
         SideEffect,
         Lift,
         Split,
         -- ** Types in Gremlin
         Element,
         Vertex,
         Edge,
         GVertex,
         GEdge,
         PropertyValue,
         ElementID,
         -- * GTraversal
         (@.),
         allVertices,
         allVertices',
         vertexByID,
         vertexByID',
         unsafeGTraversal,
         -- * Step
         unsafeStep,
         -- ** Filter step
         gIdentity,
         gIdentity',
         gFilter,
         gHas,
         gHas',
         gHasLabel,
         gHasLabel',
         gHasId,
         gHasId',
         gOr,
         gAnd,
         gNot,
         -- ** Transformation step
         gOrderBy,
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
import Data.Void (Void)
import Data.Greskell.Greskell
  ( Greskell, raw, methodCall,
    GreskellLike(..)
  )


-- | @GraphTraversal@ class object of TinkerPop. It takes data @s@
-- from upstream and emits data @e@ to downstream. Type @c@ is a
-- marker to describe the effect of the traversal.
--
-- 'GTraversal' is NOT a 'Category'. Because a @GraphTraversal@ object
-- keeps some context data, the starting (left-most) @GraphTraversal@
-- object controls most of the behavior of entire composition of
-- traversals and steps. This violates 'Category' law.
newtype GTraversal c s e = GTraversal { unGTraversal :: Greskell }
                         deriving (Show)
                                  
-- | Unsafely convert output type.
instance Functor (GTraversal c s) where
  fmap _ = GTraversal . unGTraversal

-- | Unsafely convert input and output types.
instance Bifunctor (GTraversal c) where
  bimap _ _ = GTraversal . unGTraversal

instance GreskellLike (GTraversal c s e) where
  unsafeFromGreskell = GTraversal
  toGreskell = unGTraversal

-- | Types that can convert to 'GTraversal'.
class ToGTraversal g where
  toGTraversal :: StepType c => g c s e -> GTraversal c s e
  liftType :: (StepType from, StepType to, Lift from to) => g from s e -> g to s e
  -- ^ Lift 'StepType' @from@ to @to@. Use this for type matching.

instance ToGTraversal GTraversal where
  toGTraversal = id
  liftType = GTraversal . unGTraversal


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
newtype Step c s e = Step { unStep :: Greskell }
                    deriving (Show)

-- | 'id' is 'gIdentity''.
instance StepType c => Category (Step c) where
  id = gIdentity'
  bc . ab = unsafeStep (unStep ab <> unStep bc)

-- | Unsafely convert output type
instance Functor (Step c s) where
  fmap _ = Step . unStep

-- | Unsafely convert input and output types.
instance Bifunctor (Step c) where
  bimap _ _ = Step . unStep

-- | To convert a 'Step' to 'GTraversal', it calls its static method
-- version on @__@ class.
instance ToGTraversal Step where
  toGTraversal step = unsafeGTraversal (raw "__" <> toGreskell step)
  liftType = Step . unStep

instance GreskellLike (Step c s e) where
  unsafeFromGreskell = Step
  toGreskell = unStep




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


unsafeGTraversal :: Greskell -> GTraversal c s e
unsafeGTraversal = GTraversal

-- | TinkerPop traversal to get all vertices.
allVertices :: GTraversal Transform Void GVertex
allVertices = allVertices'

-- | Polymorphic version of 'allVertices'.
allVertices' :: Vertex v => GTraversal Transform Void v
allVertices' = unsafeGTraversal $ raw "g.V()"

vertexByID :: Greskell
              -- ^ Gremlin code for vertex ID.
           -> GTraversal Transform Void GVertex
vertexByID = vertexByID'

-- | Polymorphic version of 'vertexByID'.
vertexByID' :: Vertex v => Greskell -> GTraversal Transform Void v
vertexByID' vid = unsafeGTraversal (raw "g" <> methodCall "V" [vid])

infixl 5 @.

-- | Apply the 'Step' to the 'GTraversal'. In Gremlin, this means
-- calling a chain of methods on the Traversal object.
(@.) :: GTraversal c a b -> Step c b d -> GTraversal c a d
gt @. gs = unsafeGTraversal (toGreskell gt <> toGreskell gs)


-- | Element interface in a TinkerPop graph.
class Element e 

-- | Vertex interface in a TinkerPop graph.
class Element v => Vertex v

-- | Edge interface in a TinkerPop graph.
class Element e => Edge e

-- | General vertex type you can use for 'Vertex' class.
data GVertex

instance Element GVertex
instance Vertex GVertex

-- | General edge type you can use for 'Edge' class.
data GEdge

instance Element GEdge
instance Edge GEdge

-- | Value object in a TinkerPop graph.
data PropertyValue

-- | ID object type for Elements
data ElementID

unsafeStep :: StepType c => Greskell -> Step c s e
unsafeStep = Step

-- | @.identity@ step.
gIdentity :: Step Filter s s
gIdentity = unsafeStep $ methodCall "identity" []

-- | Polymorphic version of 'gIdentity'.
gIdentity' :: StepType c => Step c s s
gIdentity' = liftType $ gIdentity

-- | @.filter@ step with steps(traversal).
gFilter :: (ToGTraversal g, StepType c, StepType p, Split c p) => g c s e -> Step p s s
gFilter step = unsafeStep (methodCall "filter" [toGreskell $ toGTraversal step])

-- | @.has@ step.
gHas :: (Element s)
     => Greskell -- ^ target
     -> Greskell -- ^ expectation
     -> Step Filter s s
gHas target expec = unsafeStep $ methodCall "has" [target, expec]

-- | Polymorphic version of 'gHas'.
gHas' :: (Element s, StepType c) => Greskell -> Greskell -> Step c s s
gHas' t e = liftType $ gHas t e

-- | @.hasLabel@ step
gHasLabel :: Element s
          => [Greskell] -- ^ expected label names
          -> Step Filter s s
gHasLabel = unsafeStep . methodCall "hasLabel"

-- | Polymorphic version of 'gHasLabel'.
gHasLabel' :: (Element s, StepType c) => [Greskell] -> Step c s s
gHasLabel' = liftType . gHasLabel

-- | @.hasId@ step
gHasId :: Element s
       => [Greskell] -- ^ expected IDs
       -> Step Filter s s
gHasId = unsafeStep . methodCall "hasId"

-- | Polymorphic version of 'gHasId'.
gHasId' :: (Element s, StepType c) => [Greskell] -> Step c s s
gHasId' = liftType . gHasId

multiLogic :: (ToGTraversal g, StepType c, StepType p, Split c p)
           => Text -- ^ method name
           -> [g c s e]
           -> Step p s s
multiLogic method_name conds = unsafeStep (methodCall method_name $ map toG conds)
  where
    toG cond = toGreskell $ toGTraversal cond

-- | @.and@ step.
gAnd :: (ToGTraversal g, StepType c, StepType p, Split c p) => [g c s e] -> Step p s s
gAnd = multiLogic "and"

-- | @.or@ step.
gOr :: (ToGTraversal g, StepType c, StepType p, Split c p) => [g c s e] -> Step p s s
gOr = multiLogic "or"

-- | @.not@ step.
gNot :: (ToGTraversal g, StepType c, StepType p, Split c p) => g c s e -> Step p s s
gNot cond = unsafeStep (methodCall "not" [toGreskell $ toGTraversal cond])

-- | @.range@ step.
gRange :: Greskell
       -- ^ min
       -> Greskell
       -- ^ max
       -> Step Transform s s
gRange min_g max_g = unsafeStep (methodCall "range" [min_g, max_g])

-- | @.order@ and @.by@ steps
gOrderBy :: (ToGTraversal g)
         => [(g Transform s e, Greskell)]
         -- ^ (accessor steps, comparator) of each @.by@
         -> Step Transform s s
gOrderBy bys = unsafeStep (methodCall "order" [] <> bys_g)
  where
    bys_g = mconcat $ map toG bys
    toG (accessor, comparator) =
      methodCall "by" [(toGreskell $ toGTraversal accessor), comparator]

-- | @.flatMap@ step
gFlatMap :: (ToGTraversal g, StepType c) => g c s e -> Step c s e
gFlatMap gt = unsafeStep (methodCall "flatMap" [toGreskell $ toGTraversal gt])

-- | @.values@ step.
gValues :: Element s
        => [Greskell]
        -- ^ property keys
        -> Step Transform s PropertyValue
gValues = unsafeStep . methodCall "values"

genericTraversalStep :: Vertex v => Text -> [Greskell] -> Step Transform v e
genericTraversalStep method_name edge_labels =
  unsafeStep (methodCall method_name edge_labels)

-- | @.out@ step
gOut :: (Vertex v)
     => [Greskell] -- ^ edge labels
     -> Step Transform v GVertex
gOut = gOut'

-- | Polymorphic version of 'gOut'.
gOut' :: (Vertex v1, Vertex v2) => [Greskell] -> Step Transform v1 v2
gOut' = genericTraversalStep "out"

-- | @.outE@ step
gOutE :: (Vertex v)
      => [Greskell] -- ^ edge labels
      -> Step Transform v GEdge
gOutE = gOutE'

-- | Polymorphic version of 'gOutE'
gOutE' :: (Vertex v, Edge e) => [Greskell] -> Step Transform v e
gOutE' = genericTraversalStep "outE"

-- | @.in@ step
gIn :: (Vertex v)
    => [Greskell] -- ^ edge labels
    -> Step Transform v GVertex
gIn = gIn'

-- | Polymorphic version of 'gIn'.
gIn' :: (Vertex v1, Vertex v2) => [Greskell] -> Step Transform v1 v2
gIn' = genericTraversalStep "in"

-- | @.inE@ step.
gInE :: (Vertex v)
     => [Greskell] -- ^ edge labels
     -> Step Transform v GEdge
gInE = gInE

-- | Polymorphic version of 'gInE'.
gInE' :: (Vertex v, Edge e) => [Greskell] -> Step Transform v e
gInE' = genericTraversalStep "inE"

---- -- probably we can implement .as() step like this. GBuilder generates
---- -- some 'Label', which is passed to .as() step and can be passed later
---- -- to .select() step etc.
---- gAs :: GBuilder (Label, Step Filter s s)
---- gAs = undefined
