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
         GStep,
         GTraversal,
         ToGTraversal(..),
         -- ** Step types
         StepType,
         Filter,
         Transform,
         SideEffect,
         Lift,
         Logic,
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
         -- * GStep
         unsafeGStep,
         -- ** Filter step
         gIdentity,
         gIdentity',
         gFilterL,
         gFilterL',
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


-- | A Gremlin Step (method call) that takes data @s@ from upstream
-- and emits data @e@ to downstream. Type @c@ is a marker to describe
-- the step.
--
-- 'GStep' is a 'Category'. Use its methods to compose them.
--
-- 'GStep' is not an 'Eq', because it's difficult to define true
-- equality between Gremlin method calls. If we define it naively, it
-- might have conflict with 'Category' law.
newtype GStep c s e = GStep { unGStep :: Greskell }
                    deriving (Show)

-- | 'id' is 'identity'.
instance StepType c => Category (GStep c) where
  id = liftType gIdentity
  bc . ab = unsafeGStep (unGStep ab <> unGStep bc)

-- | Unsafely convert output type
instance Functor (GStep c s) where
  fmap _ = GStep . unGStep

-- | Unsafely convert input and output types.
instance Bifunctor (GStep c) where
  bimap _ _ = GStep . unGStep

-- | Call static method versions of the 'GStep' on @__@ class.
instance ToGTraversal GStep where
  toGTraversal step = unsafeGTraversal (raw "__" <> toGreskell step)
  liftType = GStep . unGStep

instance GreskellLike (GStep c s e) where
  unsafeFromGreskell = GStep
  toGreskell = unGStep


-- | GraphTraversal class object of TinkerPop.
--
-- 'GTraversal' is practically the same as 'GStep'. 'GTraversal' is a
-- Java-object in Gremlin domain, while 'GStep' is a chain of method
-- calls.
newtype GTraversal c s e = GTraversal { unGTraversal :: Greskell }
                         deriving (Show)
                                  
-- | 'id' is @__.identity()@. '(.)' compose 'GTraversal's by
-- @.flatMap@ step.
instance StepType c => Category (GTraversal c) where
  id = toGTraversal $ liftType gIdentity
  a . b = b @. gFlatMap a

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


-- | Phantom type markers to describe the feature fo the
-- step/traversal.
class StepType t

-- | StepType for filtering steps.
--
-- A filtering step is a step that does filtering only. It takes input
-- and outputs some of them without any modification, reordering,
-- traversal actions, or side-effects. Filtering decision must be
-- solely based on each element.
--
-- This leads to the following property.
--
-- > s1, s2 :: GStep Filter s s
-- > gFilter s1 == s1
-- > gAnd [s1, s2] == s1 >>> s2 == s2 >>> s1
data Filter

instance StepType Filter

-- | StepType for steps that are not filtering steps and without
-- side-effects. This includes transformations, reordring, injections
-- and graph traversal actions.
data Transform

instance StepType Transform

-- | StepType modifier for steps that has side-effects.
data SideEffect t

-- Needs FlexibleInstances extension.
instance StepType (SideEffect Filter)
instance StepType (SideEffect Transform)

-- | Relation of 'StepType's in which one includes the other. @from@
-- can be lifted to @to@, because @to@ is more powerful than @to@.
class Lift from to

instance (StepType t) => Lift Filter t
instance Lift Transform Transform
instance Lift Transform (SideEffect Transform)
instance (Lift f t) => Lift (SideEffect f) (SideEffect t)

-- | Relation of 'StepType's in logic step/traversals, e.g., 'gFilter'
-- and 'gOr'. @c@ is the 'StepType' of logic operands (children), @p@
-- is the 'StepType' of the result (parent).
class Logic c p

instance (StepType p) => Logic Filter p
instance (StepType p) => Logic Transform p
-- ^ 'Transform' without any side-effect doesn't restrict the logic
-- result.
instance (StepType c, StepType p) => Logic (SideEffect c) (SideEffect p)
-- ^ 'SideEffect' is inherited by the logic result.



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

-- | Apply the 'GStep' to the 'GTraversal'. In Gremlin, this means
-- calling a chain of methods on the Traversal object.
(@.) :: GTraversal c a b -> GStep c b d -> GTraversal c a d
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

unsafeGStep :: StepType c => Greskell -> GStep c s e
unsafeGStep = GStep

-- | @.identity@ step.
gIdentity :: GStep Filter s s
gIdentity = unsafeGStep $ methodCall "identity" []

-- | Polymorphic version of 'gIdentity'.
gIdentity' :: StepType c => GStep c s s
gIdentity' = liftType $ gIdentity

-- | @.filter@ step with lambda block.
gFilterL :: Greskell
         -- ^ Gremlin code inside filter's @{}@ block.
         -> GStep Filter s s
gFilterL block = unsafeGStep (methodCall "filter" [raw "{" <> block <> raw "}"])

-- | Polymorphic version of 'gFilterL'.
gFilterL' :: (StepType c) => Greskell -> GStep c s s
gFilterL' = liftType . gFilterL

-- | @.filter@ step with steps(traversal).
gFilter :: (ToGTraversal g, StepType c, StepType p, Logic c p) => g c s e -> GStep p s s
gFilter step = unsafeGStep (methodCall "filter" [toGreskell $ toGTraversal step])

-- | @.has@ step.
gHas :: (Element s)
     => Greskell -- ^ target
     -> Greskell -- ^ expectation
     -> GStep Filter s s
gHas target expec = unsafeGStep $ methodCall "has" [target, expec]

-- | Polymorphic version of 'gHas'.
gHas' :: (Element s, StepType c) => Greskell -> Greskell -> GStep c s s
gHas' t e = liftType $ gHas t e

-- | @.hasLabel@ step
gHasLabel :: Element s
          => [Greskell] -- ^ expected label names
          -> GStep Filter s s
gHasLabel = unsafeGStep . methodCall "hasLabel"

-- | Polymorphic version of 'gHasLabel'.
gHasLabel' :: (Element s, StepType c) => [Greskell] -> GStep c s s
gHasLabel' = liftType . gHasLabel

-- | @.hasId@ step
gHasId :: Element s
       => [Greskell] -- ^ expected IDs
       -> GStep Filter s s
gHasId = unsafeGStep . methodCall "hasId"

-- | Polymorphic version of 'gHasId'.
gHasId' :: (Element s, StepType c) => [Greskell] -> GStep c s s
gHasId' = liftType . gHasId

multiLogic :: (ToGTraversal g, StepType c, StepType p, Logic c p)
           => Text -- ^ method name
           -> [g c s e]
           -> GStep p s s
multiLogic method_name conds = unsafeGStep (methodCall method_name $ map toG conds)
  where
    toG cond = toGreskell $ toGTraversal cond

-- | @.and@ step.
gAnd :: (ToGTraversal g, StepType c, StepType p, Logic c p) => [g c s e] -> GStep p s s
gAnd = multiLogic "and"

-- | @.or@ step.
gOr :: (ToGTraversal g, StepType c, StepType p, Logic c p) => [g c s e] -> GStep p s s
gOr = multiLogic "or"

-- | @.not@ step.
gNot :: (ToGTraversal g, StepType c, StepType p, Logic c p) => g c s e -> GStep p s s
gNot cond = unsafeGStep (methodCall "not" [toGreskell $ toGTraversal cond])

-- | @.range@ step.
gRange :: Greskell
       -- ^ min
       -> Greskell
       -- ^ max
       -> GStep Transform s s
gRange min_g max_g = unsafeGStep (methodCall "range" [min_g, max_g])

-- | @.order@ and @.by@ steps
gOrderBy :: (ToGTraversal g)
         => [(g Transform s e, Greskell)]
         -- ^ (accessor steps, comparator) of each @.by@
         -> GStep Transform s s
gOrderBy bys = unsafeGStep (methodCall "order" [] <> bys_g)
  where
    bys_g = mconcat $ map toG bys
    toG (accessor, comparator) =
      methodCall "by" [(toGreskell $ toGTraversal accessor), comparator]

-- | @.flatMap@ step
gFlatMap :: (ToGTraversal g, StepType c) => g c s e -> GStep c s e
gFlatMap gt = unsafeGStep (methodCall "flatMap" [toGreskell $ toGTraversal gt])

-- | @.values@ step.
gValues :: Element s
        => [Greskell]
        -- ^ property keys
        -> GStep Transform s PropertyValue
gValues = unsafeGStep . methodCall "values"

genericTraversalStep :: Vertex v => Text -> [Greskell] -> GStep Transform v e
genericTraversalStep method_name edge_labels =
  unsafeGStep (methodCall method_name edge_labels)

-- | @.out@ step
gOut :: (Vertex v)
     => [Greskell] -- ^ edge labels
     -> GStep Transform v GVertex
gOut = gOut'

-- | Polymorphic version of 'gOut'.
gOut' :: (Vertex v1, Vertex v2) => [Greskell] -> GStep Transform v1 v2
gOut' = genericTraversalStep "out"

-- | @.outE@ step
gOutE :: (Vertex v)
      => [Greskell] -- ^ edge labels
      -> GStep Transform v GEdge
gOutE = gOutE'

-- | Polymorphic version of 'gOutE'
gOutE' :: (Vertex v, Edge e) => [Greskell] -> GStep Transform v e
gOutE' = genericTraversalStep "outE"

-- | @.in@ step
gIn :: (Vertex v)
    => [Greskell] -- ^ edge labels
    -> GStep Transform v GVertex
gIn = gIn'

-- | Polymorphic version of 'gIn'.
gIn' :: (Vertex v1, Vertex v2) => [Greskell] -> GStep Transform v1 v2
gIn' = genericTraversalStep "in"

-- | @.inE@ step.
gInE :: (Vertex v)
     => [Greskell] -- ^ edge labels
     -> GStep Transform v GEdge
gInE = gInE

-- | Polymorphic version of 'gInE'.
gInE' :: (Vertex v, Edge e) => [Greskell] -> GStep Transform v e
gInE' = genericTraversalStep "inE"

---- -- probably we can implement .as() step like this. GBuilder generates
---- -- some 'Label', which is passed to .as() step and can be passed later
---- -- to .select() step etc.
---- gAs :: GBuilder (Label, GStep Filter s s)
---- gAs = undefined
