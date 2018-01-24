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
         modulateWith,
         -- ** Filter steps
         gIdentity,
         gIdentity',
         gFilter,
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
         gAnd,
         gOr,
         gNot,
         -- ** Sorting steps
         gOrderBy,
         -- ** Size limitation steps
         gRange,
         -- ** Transformation steps
         gFlatMap,
         gValues,
         gProperties,
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
         -- * Types for @.by@ step
         ByProjection,
         pjEmpty,
         pjTraversal,
         pjT,
         pjKey,
         pjFunction,
         ByComparator(ByComp)
       ) where

import Prelude hiding (or, filter, not)
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
import Data.Void (Void)
import Data.Greskell.Graph
  ( Element(..), Vertex, Edge, Property(..),
    AVertex, AEdge,
    T, Key
  )
import Data.Greskell.Gremlin
  ( Comparator(..),
    P
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



-- | Class of phantom type markers to describe the feature of the
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
vertices :: Vertex v
         => [Greskell (ElementID v)] -- ^ vertex IDs
         -> Greskell GraphTraversalSource
         -> GTraversal Transform Void v
vertices ids src = GTraversal $ sourceMethod "V" ids src

-- | Monomorphic version of 'vertices'.
vertices' :: [Greskell Value]
          -> Greskell GraphTraversalSource
          -> GTraversal Transform Void AVertex
vertices' = vertices

-- | @.E()@ method on 'GraphTraversalSource'.
edges :: Edge e
      => [Greskell (ElementID e)] -- ^ edge IDs
      -> Greskell GraphTraversalSource
      -> GTraversal Transform Void e
edges ids src = GTraversal $ sourceMethod "E" ids src

-- | Monomorphic version of 'edges'.
edges' :: [Greskell Value]
       -> Greskell GraphTraversalSource
       -> GTraversal Transform Void AEdge
edges' = edges

-- | Unsafely create 'GTraversal' from the given raw Gremlin script.
unsafeGTraversal :: Text -> GTraversal c s e
unsafeGTraversal = GTraversal . unsafeGreskell

infixl 1 &.

-- | Apply the 'Walk' to the 'GTraversal'. In Gremlin, this means
-- calling a chain of methods on the Traversal object.
(&.) :: GTraversal c a b -> Walk c b d -> GTraversal c a d
(GTraversal gt) &. (Walk twalk) = GTraversal $ unsafeGreskellLazy (toGremlinLazy gt <> twalk)

infixr 0 $.

-- | Same as '&.' with arguments flipped.
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

-- | Optionally modulate the main 'Walk' with some modulating 'Walk's.
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
gFilter :: (ToGTraversal g, WalkType c, WalkType p, Split c p) => g c s e -> Walk p s s
gFilter walk = unsafeWalk "filter" [travToG walk]


-- TODO
-- gValueMap, gProperty etc. should use Key type as an argument.
-- Note that .property step is very tricky. Read the doc carefully.
-- 

-- | @.has@ step with one argument.
gHas1 :: (WalkType c, Element s)
      => Key s v -- ^ property key
      -> Walk c s s
gHas1 = liftWalk . gHas1'

-- | Monomorphic version of 'gHas1'.
gHas1' :: (Element s) => Key s v -> Walk Filter s s
gHas1' key = unsafeWalk "has" [toGremlin key]

-- | @.has@ step with two arguments.
gHas2 :: (WalkType c, Element s) => Key s v -> Greskell v -> Walk c s s
gHas2 k v = liftWalk $ gHas2' k v

-- | Monomorphic verson of 'gHas2'.
gHas2' :: (Element s) => Key s v -> Greskell v -> Walk Filter s s
gHas2' k v = unsafeWalk "has" [toGremlin k, toGremlin v]

-- | @.has@ step with two arguments and 'P' type.
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
gHasLabel :: (Element s, WalkType c) => Greskell Text -> Walk c s s
gHasLabel = liftWalk . gHasLabel'

-- | Monomorphic version of 'gHasLabel'.
gHasLabel' :: (Element s) => Greskell Text -> Walk Filter s s
gHasLabel' l = unsafeWalk "hasLabel" [toGremlin l]

-- | @.hasLabel@ step with 'P' type. Supported since TinkerPop 3.2.7.
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
gHasId :: (Element s, WalkType c) => Greskell (ElementID s) -> Walk c s s
gHasId = liftWalk . gHasId'

-- | Monomorphic version of 'gHasId'.
gHasId' :: Element s => Greskell (ElementID s) -> Walk Filter s s
gHasId' i = unsafeWalk "hasId" [toGremlin i]

-- | @.hasId@ step with 'P' type. Supported since TinkerPop 3.2.7.
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
gHasKey :: (Element (p v), Property p, WalkType c) => Greskell Text -> Walk c (p v) (p v)
gHasKey = liftWalk . gHasKey'

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
gHasValue :: (Element (p v), Property p, WalkType c) => Greskell v -> Walk c (p v) (p v)
gHasValue = liftWalk . gHasValue'

-- | Monomorphic version of 'gHasValue'.
gHasValue' :: (Element (p v), Property p) => Greskell v -> Walk Filter (p v) (p v)
gHasValue' v = unsafeWalk "hasValue" [toGremlin v]

-- | @.hasValue@ step with 'P' type. Supported since TinkerPop 3.2.7.
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


-- | Projection from type @s@ to type @e@ used in @.by@ step.
data ByProjection s e where
  BPEmpty :: ByProjection s s
  BPTraversal :: (ToGTraversal g) => g Transform s e -> ByProjection s e
  BPT :: Greskell (T s e) -> ByProjection s e
  BPKey :: Key s e -> ByProjection s e
  BPFunction :: Greskell (a -> b) -> ByProjection a b

-- | Use 'pjKey' with the literal property key.
instance IsString (ByProjection s e) where
  fromString = pjKey . fromString

-- | A special 'ByProjection' that means omitting the projection
-- altogether. In this case, the projection does nothing (i.e. it's
-- the identity projection.)
pjEmpty :: ByProjection s s
pjEmpty = BPEmpty

-- | Projection by transforming traversal.
pjTraversal :: (ToGTraversal g) => g Transform s e -> ByProjection s e
pjTraversal = BPTraversal

-- | A projection to get a property value from an Element by 'T'.
pjT :: Greskell (T s e) -> ByProjection s e
pjT = BPT

-- | A projection to get a property value from an Element by property
-- 'Key'.
pjKey :: Key s e -> ByProjection s e
pjKey = BPKey

-- | Projection by function.
pjFunction :: Greskell (a -> b) -> ByProjection a b
pjFunction = BPFunction

-- | Comparator of type @s@ used in @.by@ step.
--
-- The input of type @s@ is first projected to type @e@, and compared
-- by the given comparator.
data ByComparator s where
  ByComp :: Comparator c => ByProjection s (CompareArg c) -> Greskell c -> ByComparator s

-- | @.order@ and @.by@ steps
gOrderBy :: [ByComparator s] -- ^ comparators for each @.by@ step
         -> Walk Transform s s
gOrderBy bys = modulateWith order_step by_steps
  where
    order_step = unsafeWalk "order" []
    by_steps = map (unsafeWalk "by" . toByArgs) bys
    toByArgs (ByComp proj comp) = case proj of
      BPEmpty -> [comp_text]
      BPTraversal gt -> [toGremlin $ toGTraversal gt, comp_text]
      BPT t -> [toGremlin t, comp_text]
      BPKey k -> [toGremlin k, comp_text]
      BPFunction fun -> [toGremlin fun, comp_text]
      where
        comp_text = toGremlin comp

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
        => [Key s e]
        -- ^ property keys
        -> Walk Transform s e
gValues = unsafeWalk "values" . map toGremlin

-- | @.properties@ step.
gProperties :: (Element s, Property p, ElementProperty s ~ p)
            => [Key s v]
            -> Walk Transform s (p v)
gProperties = unsafeWalk "properties" . map toGremlin

genericTraversalWalk :: Vertex v => Text -> [Greskell Text] -> Walk Transform v e
genericTraversalWalk method_name = unsafeWalk method_name . map toGremlin

-- | @.out@ step
gOut :: (Vertex v1, Vertex v2)
     => [Greskell Text] -- ^ edge labels
     -> Walk Transform v1 v2
gOut = genericTraversalWalk "out"

-- | Monomorphic version of 'gOut'.
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

---- -- probably we can implement .as() step like this. GBuilder generates
---- -- some 'Label', which is passed to .as() step and can be passed later
---- -- to .select() step etc.
---- gAs :: GBuilder (Label, Walk Filter s s)
---- gAs = undefined
