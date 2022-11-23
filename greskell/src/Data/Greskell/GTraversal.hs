{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
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
      GTraversal (..)
    , GraphTraversal
    , ToGTraversal (..)
    , Walk
    , GraphTraversalSource
      -- ** Walk types
    , WalkType
    , Filter
    , Transform
    , SideEffect
    , Lift
    , Split
      -- * GraphTraversalSource
    , source
    , sV
    , sV'
    , sE
    , sE'
    , sAddV
    , sAddV'
      -- * GTraversal
    , (&.)
    , ($.)
    , (<$.>)
    , (<*.>)
    , gIterate
    , unsafeGTraversal
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
    , unsafeWalk
    , modulateWith
      -- ** Filter steps
    , gIdentity
    , gIdentity'
    , gFilter
    , gCyclicPath
    , gCyclicPath'
    , gSimplePath
    , gSimplePath'
      -- ** Is step
    , gIs
    , gIs'
    , gIsP
    , gIsP'
      -- ** Has steps
    , gHas1
    , gHas1'
    , gHas2
    , gHas2'
    , gHas2P
    , gHas2P'
    , gHasLabel
    , gHasLabel'
    , gHasLabelP
    , gHasLabelP'
    , gHasId
    , gHasId'
    , gHasIdP
    , gHasIdP'
    , gHasKey
    , gHasKey'
    , gHasKeyP
    , gHasKeyP'
    , gHasValue
    , gHasValue'
    , gHasValueP
    , gHasValueP'
      -- ** Logic steps
    , gAnd
    , gOr
    , gNot
      -- ** Where step
    , gWhereP1
    , gWhereP1'
    , gWhereP2
    , gWhereP2'
      -- ** Sorting steps
    , gOrder
      -- ** Paging steps
    , gRange
    , gLimit
    , gTail
    , gSkip
      -- ** Repeat step
    , gRepeat
    , gTimes
    , gUntilHead
    , gUntilTail
    , gEmitHead
    , gEmitTail
    , gEmitHeadT
    , gEmitTailT
    , gLoops
    , RepeatUntil (..)
    , RepeatEmit (..)
    , RepeatPos (..)
    , RepeatLabel (..)
      -- ** Branching steps
    , gLocal
    , gUnion
    , gCoalesce
    , gChoose3
      -- ** Barrier steps
    , gBarrier
    , gDedup
    , gDedupN
      -- ** Transformation steps
    , gFlatMap
    , gFlatMap'
    , gV
    , gV'
    , gConstant
    , gProject
      -- ** As step
    , gAs
      -- ** Accessor steps
    , gValues
    , gProperties
    , gId
    , gLabel
    , gValueMap
    , gElementMap
    , gSelect1
    , gSelectN
    , gSelectBy1
    , gSelectByN
    , gUnfold
    , gPath
    , gPathBy
      -- ** Summarizing steps
    , gFold
    , gCount
      -- ** Graph traversal steps
    , gOut
    , gOut'
    , gOutE
    , gOutE'
    , gOutV
    , gOutV'
    , gIn
    , gIn'
    , gInE
    , gInE'
    , gInV
    , gInV'
      -- ** Match step
    , gMatch
    , MatchPattern (..)
    , mPattern
    , MatchResult
      -- ** Side-effect steps
    , gSideEffect
    , gSideEffect'
      -- ** Graph manipulation steps
    , gAddV
    , gAddV'
    , gAddE
    , gAddE'
    , AddAnchor
    , gFrom
    , gTo
    , gDrop
    , gDropP
    , gProperty
    , gPropertyV
      -- | @.by@ steps are not 'Walk' on their own because they are
      -- always used in conjunction with other steps like 'gOrder'.
    , ByProjection (..)
    , ProjectionLike (..)
    , ByComparator (..)
    , LabeledByProjection (..)
    , gBy
    , gBy1
    , gBy2
    , gByL
      -- * Examples
    , examples
      -- * Only for tests
    , showWalkType
    , showLift
    , showSplit
    ) where

import           Control.Applicative      ((<$>), (<*>))
import           Control.Category         (Category, (>>>))
-- (below) to import Category methods without conflict with Prelude
import qualified Control.Category         as Category
import           Data.Aeson               (Value)
import           Data.Bifunctor           (Bifunctor (bimap))
import           Data.Foldable            (foldl')
import           Data.Function            ((&))
import           Data.List.NonEmpty       (NonEmpty (..))
import           Data.Monoid              (Monoid (..), mconcat, (<>))
import           Data.Proxy               (Proxy)
import           Data.Semigroup           (Semigroup, sconcat)
import qualified Data.Semigroup           as Semigroup
import           Data.String              (IsString (..))
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL

import           Data.Greskell.AsIterator (AsIterator (IteratorItem))
import           Data.Greskell.AsLabel    (AsLabel, LabeledP, SelectedMap)
import           Data.Greskell.Graph      (AEdge, AVertex, AVertexProperty, Cardinality, Edge,
                                           Element (..), ElementID (..), Key, KeyValue (..),
                                           Keys (..), Path, Property (..), T, Vertex, cList, tId,
                                           toGremlinKeys, (-:), (=:))
import           Data.Greskell.GraphSON   (FromGraphSON, GValue)
import           Data.Greskell.Gremlin    (Comparator (..), P, oDecr, oIncr, pBetween, pEq, pLte)
import           Data.Greskell.Greskell   (Greskell, ToGreskell (..), gvalueInt, toGremlin,
                                           toGremlinLazy, unsafeFunCall, unsafeGreskell,
                                           unsafeGreskellLazy)
import qualified Data.Greskell.Greskell   as Greskell
import           Data.Greskell.Logic      (Logic)
import qualified Data.Greskell.Logic      as Logic
import           Data.Greskell.PMap       (PMap, Single)

-- | @GraphTraversal@ class object of TinkerPop. It takes data @s@
-- from upstream and emits data @e@ to downstream. Type @c@ is called
-- \"walk type\", a marker to describe the effect of the traversal.
--
-- 'GTraversal' is NOT a 'Category'. Because a @GraphTraversal@ object
-- keeps some context data, the starting (left-most) @GraphTraversal@
-- object controls most of the behavior of entire composition of
-- traversals and steps. This violates 'Category' law.
newtype GTraversal c s e
  = GTraversal { unGTraversal :: Greskell (GraphTraversal c s e) }
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
data GraphTraversal c s e = GraphTraversal deriving (Show)

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

  unsafeCastStart :: WalkType c => g c s1 e -> g c s2 e
  -- ^ Unsafely cast the start type @s1@ into @s2@.
  --
  -- It is recommended that @s2@ is coercible to @s1@ in terms of
  -- 'FromGraphSON'. That is, if @s2@ can parse a 'GValue', @s1@
  -- should also be able to parse that 'GValue'.
  --
  -- @since 1.0.0.0

  unsafeCastEnd :: WalkType c => g c s e1 -> g c s e2
  -- ^ Unsafely cast the end type @e1@ into @e2@. See
  -- 'unsafeCastStart'.
  --
  -- @since 1.0.0.0

instance ToGTraversal GTraversal where
  toGTraversal = id
  liftWalk (GTraversal g) = GTraversal $ unsafeGreskellLazy $ toGremlinLazy g
  unsafeCastStart (GTraversal g) = GTraversal $ unsafeGreskellLazy $ toGremlinLazy g
  unsafeCastEnd (GTraversal g) = GTraversal $ unsafeGreskellLazy $ toGremlinLazy g

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
newtype Walk c s e
  = Walk TL.Text
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
  unsafeCastStart (Walk t) = Walk t
  unsafeCastEnd (Walk t) = Walk t

-- | The 'Walk' is first converted to 'GTraversal', and it's converted
-- to 'Greskell'.
instance WalkType c => ToGreskell (Walk c s e) where
  type GreskellReturn (Walk c s e) = GraphTraversal c s e
  toGreskell = toGreskell . toGTraversal

-- | Class of phantom type markers to describe the effect of the
-- walk/traversals.
class WalkType t where
  -- | Only for tests.
  showWalkType :: Proxy t -> String

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

instance WalkType Filter where
  showWalkType _ = "Filter"

-- | WalkType for steps without any side-effects. This includes
-- transformations, reordring, injections and graph traversal actions.
--
-- A 'Walk' @w@ is 'Transform' type iff:
--
-- > gSideEffect w == gIdentity
--
-- Obviously, every 'Filter' type 'Walk's are also 'Transform' type.
data Transform

instance WalkType Transform where
  showWalkType _ = "Transform"

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

instance WalkType SideEffect where
  showWalkType _ = "SideEffect"

-- | Relation of 'WalkType's where one includes the other. @from@ can
-- be lifted to @to@, because @to@ is more powerful than @from@.
class Lift from to where
  -- | Only for tests.
  showLift :: Proxy from -> Proxy to -> String

genericShowLift :: (WalkType from, WalkType to) => Proxy from -> Proxy to -> String
genericShowLift f t = "Lift " <> showWalkType f <> " " <> showWalkType t

instance (WalkType c) => Lift Filter c where
  showLift = genericShowLift
instance Lift Transform Transform where
  showLift = genericShowLift
instance Lift Transform SideEffect where
  showLift = genericShowLift
instance Lift SideEffect SideEffect where
  showLift = genericShowLift

-- | Relation of 'WalkType's where the child walk @c@ is split from
-- the parent walk @p@.
--
-- When splitting, transformation effect done in the child walk is
-- rolled back (canceled) in the parent walk.
class Split c p where
  -- | Only for tests.
  showSplit :: Proxy c -> Proxy p -> String

genericShowSplit :: (WalkType c, WalkType p) => Proxy c -> Proxy p -> String
genericShowSplit c p = "Split " <> showWalkType c <> " " <> showWalkType p

instance (WalkType p) => Split Filter p where
  showSplit = genericShowSplit

-- | 'Transform' effect in the child walk is rolled back in the parent
-- walk.
instance (WalkType p) => Split Transform p where
  showSplit = genericShowSplit

-- | 'SideEffect' in the child walk remains in the parent walk.
instance Split SideEffect SideEffect where
  showSplit = genericShowSplit


-- | @GraphTraversalSource@ class object of TinkerPop. It is a factory
-- object of 'GraphTraversal's.
data GraphTraversalSource = GraphTraversalSource deriving (Show)


-- | Create 'GraphTraversalSource' from a varible name in Gremlin
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
sV' :: [Greskell (ElementID AVertex)] -- ^ vertex IDs
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
sE' :: [Greskell (ElementID AEdge)] -- ^ edge IDs
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
-- @since 0.2.0.0
sAddV' :: Greskell Text -> Greskell GraphTraversalSource -> GTraversal SideEffect () AVertex
sAddV' = sAddV

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

infixr 0 <$.>

-- | Similar to '<$>', but for '$.'.
--
-- @since 0.2.1.0
(<$.>) :: Functor f => Walk c b d -> f (GTraversal c a b) -> f (GTraversal c a d)
gs <$.> gt = ($.) gs <$> gt

infixr 0 <*.>

-- | Similar to '<*>', but for '$.'.
--
-- @since 0.2.1.0
(<*.>) :: Applicative f => f (Walk c b d) -> f (GTraversal c a b) -> f (GTraversal c a d)
gs <*.> gt = ($.) <$> gs <*> gt

-- | @.iterate@ method on @GraphTraversal@.
--
-- 'gIterate' is not a 'Walk' because it's usually used to terminate
-- the method chain of Gremlin steps. The returned 'GTraversal'
-- outputs nothing, thus its end type is '()'.
--
-- @since 1.1.0.0
gIterate :: WalkType c => GTraversal c s e -> GTraversal c s ()
gIterate gt = unsafeWalk "iterate" [] $. gt

-- -- $walk-steps
-- --

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
modulateWith w []       = w
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

-- | @.cyclicPath@ step.
--
-- @since 1.0.1.0
gCyclicPath :: (WalkType c) => Walk c a a
gCyclicPath = liftWalk gCyclicPath'

-- | Monomorphic version of 'gCyclicPath'.
--
-- @since 1.0.1.0
gCyclicPath' :: Walk Filter a a
gCyclicPath' = unsafeWalk "cyclicPath" []

-- | @.simplePath@ step.
--
-- @since 1.0.1.0
gSimplePath :: (WalkType c) => Walk c a a
gSimplePath = liftWalk gSimplePath'

-- | Monomorphic version of 'gSimplePath'.
--
-- @since 1.0.1.0
gSimplePath' :: Walk Filter a a
gSimplePath' = unsafeWalk "simplePath" []

gWherePGeneric :: Maybe (AsLabel a)
               -> Greskell (LabeledP a)
               -> Maybe (ByProjection a b)
               -> Walk Filter x x
gWherePGeneric mstart p mby = modulateWith wh mods
  where
    wh = unsafeWalk "where" $ start_args ++ [toGremlin p]
    start_args = maybe [] (return . toGremlin) mstart
    mods = maybe [] (return . byStep) mby

-- | @.where@ step with @P@ argument only.
--
-- If the 'ByProjection' argument is 'Nothing', comparison is
-- performed on the type @a@. You have to ensure that the comparator
-- included in the 'LabeledP' argument can handle the type
-- @a@. Usually this means the type @a@ should implement Java's
-- @Comparable@ interface (this is true for most Java classes).
--
-- If the 'ByProjection' argument is given, comparison is performed on
-- the projected values of type @b@. So, the type @b@ should implement
-- Java's @Comparable@ interface.
--
-- @since 1.2.0.0
gWhereP1 :: WalkType c
         => Greskell (LabeledP a) -- ^ the @P@ argument for @.where@ step.
         -> Maybe (ByProjection a b) -- ^ optional @.by@ modulation following the @.where@ step.
         -> Walk c a a
gWhereP1 p mby = liftWalk $ gWhereP1' p mby

-- | Monomorphic version of 'gWhereP1'.
--
-- @since 1.2.0.0
gWhereP1' :: Greskell (LabeledP a) -> Maybe (ByProjection a b) -> Walk Filter a a
gWhereP1' p mby = gWherePGeneric Nothing p mby

-- | @.where@ step with the starting label and @P@ arguments. See also
-- 'gWhereP1'.
--
-- @since 1.2.0.0
gWhereP2 :: WalkType c
         => AsLabel a -- ^ the starting label of @.where@.
         -> Greskell (LabeledP a) -- ^ the @P@ argument for @.where@ step.
         -> Maybe (ByProjection a b) -- ^ optional @.by@ modulation following the @.where@ step.
         -> Walk c x x
gWhereP2 s p b = liftWalk $ gWhereP2' s p b

-- | Monomorphic version of 'gWhereP2'.
--
-- @since 1.2.0.0
gWhereP2' :: AsLabel a -> Greskell (LabeledP a) -> Maybe (ByProjection a b) -> Walk Filter x x
gWhereP2' start p mby = gWherePGeneric (Just start) p mby

-- Developer note: the @.where@ step with a traversal argument is not
-- implemented yet, because @.match@ basically covers the same
-- capability. If we are to implement it, consider the following.
--
-- - The @.where@ step with a traversal argument doesn't take @.by@
--   modulation.
--
-- - The traversal argument is a logic tree (zero or more combination
--   of @__.and()@, @__.or()@ and @__.not()@ methods) of filtering
--   traversals.
--
-- - If a filtering traversal starts with @__.as()@ step,
--   it has a special meaning. The @__.as()@ step works just like
--   @__.select()@, fetching a value specified by the label from the
--   path history. In this case, the input value passed to the
--   @.where@ step is discarded.
--
-- - If a filtering traversal ends with @.as()@ step, it works like a
--   predicate step. If fetches a value specified by the label from
--   the path history, and checks if it's equal to the input
--   value. This behavior is like the one in @.match@ step, but
--   without variable binding.
--
-- - If a filtering traversal doesn't have @.as()@ step at the
--   beginning or end, it works just like it's in @.filter@ step.


-- | Result of @.match@ step.
--
-- @since 1.2.0.0
data MatchResult

-- | A pattern for @.match@ step.
--
-- @since 1.2.0.0
data MatchPattern where
  -- | A pattern with the starting @.as@ label followed by traversal steps.
  MatchPattern :: AsLabel a -> Walk Transform a b -> MatchPattern

-- | Make a 'GTraversal' from the 'MatchPattern'. This function is
-- unsafe because it discards the types of input and output
-- traversers.
unsafePatternT :: MatchPattern -> GTraversal Transform () ()
unsafePatternT (MatchPattern l w) = unsafeCastEnd $ unsafeCastStart $ toGTraversal (gAs l >>> w)

-- | A convenient function to make a 'MatchPattern' wrapped by
-- 'Logic.Leaf'.
--
-- @since 1.2.0.0
mPattern :: (WalkType c, Lift c Transform) => AsLabel a -> Walk c a b -> Logic MatchPattern
mPattern l w = Logic.Leaf $ MatchPattern l (liftWalk w)

-- | @.match@ step.
--
-- If the top-level 'Logic' of the argument is 'Logic.And', the
-- patterns are directly passed to the @.match@ step arguments.
--
-- The result of @.match@ step, 'MatchResult', is an opaque
-- type. Basically you should not use it. Instead, you should use
-- 'gSelectN' etc to access the path history labels inside the
-- 'MatchPattern'.
--
-- See also: https://groups.google.com/g/gremlin-users/c/HVtldzV0Xk8
--
-- @since 1.2.0.0
gMatch :: Logic MatchPattern -> Walk Transform a MatchResult
gMatch patterns = unsafeWalk "match" args
  where
    args =
      case patterns of
        Logic.And p rest -> map (toGremlin . toTraversal) (p : rest)
        _                -> [toGremlin $ toTraversal patterns]
    toTraversal l =
      case l of
        Logic.Leaf p     -> unsafePatternT p
        Logic.And p rest -> toGTraversal $ gAnd $ map toTraversal (p : rest)
        Logic.Or p rest  -> toGTraversal $ gOr $ map toTraversal (p : rest)
        Logic.Not p      -> toGTraversal $ gNot $ toTraversal p

-- | @.is@ step of simple equality.
--
-- @since 1.0.1.0
gIs :: (WalkType c) => Greskell v -> Walk c v v
gIs = liftWalk . gIs'

-- | Monomorphic version of 'gIs'.
--
-- @since 1.0.1.0
gIs' :: Greskell v -> Walk Filter v v
gIs' v = unsafeWalk "is" [toGremlin v]

-- | @.is@ step with predicate 'P'.
--
-- @since 1.0.1.0
gIsP :: (WalkType c) => Greskell (P v) -> Walk c v v
gIsP = liftWalk . gIsP'

-- | Monomorphic version of 'gIsP'.
--
-- @since 1.0.1.0
gIsP' :: Greskell (P v) -> Walk Filter v v
gIsP' p = unsafeWalk "is" [toGremlin p]

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

-- | @.range@ step. This step is not a 'Filter', because the filtering
-- decision by this step is based on position of each element, not the
-- element itself. This violates 'Filter' law.
gRange :: Greskell Int
       -- ^ min
       -> Greskell Int
       -- ^ max
       -> Walk Transform s s
gRange min_g max_g = unsafeWalk "range" $ map toGremlin [min_g, max_g]

-- | @.limit@ step.
--
-- @since 0.2.1.0
gLimit :: Greskell Int -> Walk Transform s s
gLimit num = unsafeWalk "limit" [toGremlin num]

-- | @.tail@ step.
--
-- @since 0.2.1.0
gTail :: Greskell Int -> Walk Transform s s
gTail num = unsafeWalk "tail" [toGremlin num]

-- | @.skip@ step.
--
-- @since 0.2.1.0
gSkip :: Greskell Int -> Walk Transform s s
gSkip num = unsafeWalk "skip" [toGremlin num]

-- | A label that points to a loop created by @.repeat@ step. It can
-- be used by @.loops@ step to specify the loop.
--
-- @since 1.0.1.0
newtype RepeatLabel
  = RepeatLabel { unRepeatLabel :: Text }
  deriving (Eq, IsString, Ord, Show)

-- | Return Gremlin String literal.
instance ToGreskell RepeatLabel where
  type GreskellReturn RepeatLabel = Text
  toGreskell (RepeatLabel t) = Greskell.string t

-- | Position of a step modulator relative to @.repeat@ step.
--
-- @since 1.0.1.0
data RepeatPos
  = RepeatHead -- ^ Modulator before the @.repeat@ step.
  | RepeatTail -- ^ Modulator after the @.repeat@ step.
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | @.until@ or @.times@ modulator step.
--
-- Type @c@ is the 'WalkType' of the parent @.repeat@ step. Type @s@
-- is the start (and end) type of the @.repeat@ step.
--
-- @since 1.0.1.0
data RepeatUntil c s where
  -- | @.times@ modulator.
  RepeatTimes :: Greskell Int -> RepeatUntil c s
  -- | @.until@ modulator with a sub-traversal as the predicate to
  -- decide if the repetition should stop.
  RepeatUntilT :: (WalkType cc, WalkType c, Split cc c) => GTraversal cc s e -> RepeatUntil c s

deriving instance Show (RepeatUntil c s)

makeUntilWalk :: WalkType c => RepeatUntil c s -> Walk c s s
makeUntilWalk (RepeatTimes count) = unsafeWalk "times" [toGremlin count]
makeUntilWalk (RepeatUntilT trav) = unsafeWalk "until" [toGremlin trav]

-- | @.emit@ modulator step.
--
-- Type @c@ is the 'WalkType' of the parent @.repeat@ step. Type @s@
-- is the start (and end) type of the @.repeat@ step.
--
-- @since 1.0.1.0
data RepeatEmit c s where
  -- | @.emit@ modulator without argument. It always emits the input
  -- traverser of type @s@.
  RepeatEmit :: RepeatEmit c s
  -- | @.emit@ modulator with a sub-traversal as the predicate to
  -- decide if it emits the traverser.
  RepeatEmitT :: (WalkType cc, WalkType c, Split cc c) => GTraversal cc s e -> RepeatEmit c s

deriving instance Show (RepeatEmit c s)

makeEmitWalk :: WalkType c => RepeatEmit c s -> Walk c s s
makeEmitWalk (RepeatEmit)       = unsafeWalk "emit" []
makeEmitWalk (RepeatEmitT trav) = unsafeWalk "emit" [toGremlin trav]



-- | Zero or more Gremlin steps.
--
-- @since 1.0.1.0
newtype MWalk c s e
  = MWalk (Maybe (Walk c s e))
  deriving (Show)

deriving instance WalkType c => Semigroup (MWalk c s s)
deriving instance WalkType c => Monoid (MWalk c s s)

toMWalk :: Walk c s e -> MWalk c s e
toMWalk = MWalk . Just

-- | @MWalk Nothing@ is coverted to identity step.
fromMWalk :: WalkType c => MWalk c s s -> Walk c s s
fromMWalk (MWalk Nothing)  = mempty
fromMWalk (MWalk (Just w)) = w



-- | @.repeat@ step.
--
-- @since 1.0.1.0
gRepeat :: (ToGTraversal g, WalkType c)
        => Maybe RepeatLabel -- ^ Label for the loop.
        -> Maybe (RepeatPos, RepeatUntil c s)
        -- ^ @.until@ or @.times@ modulator. You can use 'gTimes',
        -- 'gUntilHead', 'gUntilTail' to make this argument.
        -> Maybe (RepeatPos, RepeatEmit c s)
        -- ^ @.emit@ modulator. You can use 'gEmitHead', 'gEmitTail',
        -- 'gEmitHeadT', 'gEmitTailT' to make this argument.
        -> g c s s -- ^ Repeated traversal
        -> Walk c s s
gRepeat mlabel muntil memit repeated_trav = fromMWalk (head_walk <> toMWalk repeat_body <> tail_walk)
  where
    repeat_body = unsafeWalk "repeat" (label_args ++ [travToG repeated_trav])
    label_args = maybe [] (\l -> [toGremlin l]) mlabel
    head_walk = head_until <> head_emit
    tail_walk = tail_until <> tail_emit
    (head_until, tail_until) =
      case muntil of
        Nothing -> (mempty, mempty)
        Just (pos, u) ->
          case pos of
            RepeatHead -> (toMWalk $ makeUntilWalk u, mempty)
            RepeatTail -> (mempty, toMWalk $ makeUntilWalk u)
    (head_emit, tail_emit) =
      case memit of
        Nothing -> (mempty, mempty)
        Just (pos, e) ->
          case pos of
            RepeatHead -> (toMWalk $ makeEmitWalk e, mempty)
            RepeatTail -> (mempty, toMWalk $ makeEmitWalk e)

-- | @.times@ modulator before the @.repeat@ step. It always returns
-- 'Just'.
--
-- @since 1.0.1.0
gTimes :: Greskell Int
       -- ^ Repeat count. If it's less than or equal to 0, the
       -- repeated traversal is never executed.
       -> Maybe (RepeatPos, RepeatUntil c s)
gTimes c = Just (RepeatHead, RepeatTimes c)

-- | @.until@ modulator before the @.repeat@ step. It always returns
-- 'Just'.
--
-- @since 1.0.1.0
gUntilHead :: (ToGTraversal g, WalkType c, WalkType cc, Split cc c) => g cc s e -> Maybe (RepeatPos, RepeatUntil c s)
gUntilHead trav = Just (RepeatHead, RepeatUntilT $ toGTraversal trav)

-- | @.until@ modulator after the @.repeat@ step. It always returns
-- 'Just'.
--
-- @since 1.0.1.0
gUntilTail :: (ToGTraversal g, WalkType c, WalkType cc, Split cc c) => g cc s e -> Maybe (RepeatPos, RepeatUntil c s)
gUntilTail trav = Just (RepeatTail, RepeatUntilT $ toGTraversal trav)

-- | @.emit@ modulator without argument before the @.repeat@ step. It
-- always returns 'Just'.
--
-- @since 1.0.1.0
gEmitHead :: Maybe (RepeatPos, RepeatEmit c s)
gEmitHead = Just (RepeatHead, RepeatEmit)

-- | @.emit@ modulator without argument after the @.repeat@ step. It
-- always returns 'Just'.
--
-- @since 1.0.1.0
gEmitTail :: Maybe (RepeatPos, RepeatEmit c s)
gEmitTail = Just (RepeatTail, RepeatEmit)

-- | @.emit@ modulator with a sub-traversal argument before the
-- @.repeat@ step. It always returns 'Just'.
--
-- @since 1.0.1.0
gEmitHeadT :: (ToGTraversal g, WalkType c, WalkType cc, Split cc c) => g cc s e -> Maybe (RepeatPos, RepeatEmit c s)
gEmitHeadT trav = Just (RepeatHead, RepeatEmitT $ toGTraversal trav)

-- | @.emit@ modulator with a sub-traversal argument after the
-- @.repeat@ step. It always returns 'Just'.
--
-- @since 1.0.1.0
gEmitTailT :: (ToGTraversal g, WalkType c, WalkType cc, Split cc c) => g cc s e -> Maybe (RepeatPos, RepeatEmit c s)
gEmitTailT trav = Just (RepeatTail, RepeatEmitT $ toGTraversal trav)

-- | @.loops@ step.
--
-- @since 1.0.1.0
gLoops :: Maybe RepeatLabel -> Walk Transform s Int
gLoops mlabel = unsafeWalk "loops" $ maybe [] (\l -> [toGremlin l]) mlabel

-- | @.local@ step.
--
-- @since 1.0.1.0
gLocal :: (ToGTraversal g, WalkType c) => g c s e -> Walk c s e
gLocal t = unsafeWalk "local" [travToG t]

-- | @.union@ step.
--
-- @since 1.0.1.0
gUnion :: (ToGTraversal g, WalkType c) => [g c s e] -> Walk c s e
gUnion ts = unsafeWalk "union" $ map travToG ts

-- | @.coalesce@ step.
--
-- Like 'gFlatMap', 'gCoalesce' always modifies path history.
--
-- @since 1.1.0.0
gCoalesce :: (ToGTraversal g, Split cc c, Lift Transform c, WalkType c, WalkType cc)
          => [g cc s e] -> Walk c s e
gCoalesce ts = unsafeWalk "coalesce" $ map travToG ts

-- | @.choose@ step with if-then-else style.
--
-- @since 1.0.1.0
gChoose3 :: (ToGTraversal g, Split cc c, WalkType cc, WalkType c)
         => g cc s ep -- ^ the predicate traversal.
         -> g c s e -- ^ The traversal executed if the predicate traversal outputs something.
         -> g c s e -- ^ The traversal executed if the predicate traversal outputs nothing.
         -> Walk c s e
gChoose3 pt tt ft = unsafeWalk "choose"
                    [ travToG pt,
                      travToG tt,
                      travToG ft
                    ]

-- | @.barrier@ step.
--
-- @since 1.0.1.0
gBarrier :: WalkType c
         => Maybe (Greskell Int)
         -- ^ Max number of traversers kept at this barrier.
         -> Walk c s s
gBarrier mmax = unsafeWalk "barrier" $ maybe [] (\m -> [toGremlin m]) mmax

-- | @.dedup@ step without argument.
--
-- @.dedup@ step is 'Transform' because the filtering decision depends
-- on the sequence (order) of input elements.
--
-- @since 1.0.1.0
gDedup :: Maybe (ByProjection s e)
       -- ^ @.by@ modulator. If specified, the result of type @e@ is
       -- used as the criterion of deduplication.
       -> Walk Transform s s
gDedup mp = gDedupGeneric [] mp

-- | @.dedup@ step with at least one argument. The tuple specified by
-- the 'AsLabel's is used as the criterion of deduplication.
--
-- @since 1.0.1.0
gDedupN :: AsLabel a -> [AsLabel a] -> Maybe (ByProjection a e) -> Walk Transform s s
gDedupN l ls mp = gDedupGeneric (map toGremlin (l : ls)) mp

gDedupGeneric :: [Text] -> Maybe (ByProjection a b) -> Walk Transform s s
gDedupGeneric args mp =
  case mp of
    Nothing               -> main_walk
    Just (ByProjection g) -> modulateWith main_walk [unsafeWalk "by" [toGremlin g]]
  where
    main_walk = unsafeWalk "dedup" args


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
-- 'ByComparator' is an 'IsString', meaning projection by the given
-- key.
gOrder :: [ByComparator s] -- ^ following @.by@ steps.
       -> Walk Transform s s
gOrder bys = modulateWith order_step by_steps
  where
    order_step = unsafeWalk "order" []
    by_steps = map (unsafeWalk "by" . toByArgs) bys
    toByArgs :: ByComparator s -> [Text]
    toByArgs bc = case bc of
      ByComparatorProj (ByProjection p)          -> [toGremlin p]
      ByComparatorComp comp                      -> [toGremlin comp]
      ByComparatorProjComp (ByProjection p) comp -> [toGremlin p, toGremlin comp]

-- | A 'ByProjection' associated with an 'AsLabel'. You can construct
-- it by 'gByL'.
--
-- @since 1.0.0.0
data LabeledByProjection s where
  LabeledByProjection :: AsLabel a -> ByProjection s a -> LabeledByProjection s

-- | @.by@ step associated with an 'AsLabel'.
--
-- @since 1.0.0.0
gByL :: (ProjectionLike p, ToGreskell p) => AsLabel (ProjectionLikeEnd p) -> p -> LabeledByProjection (ProjectionLikeStart p)
gByL l p = LabeledByProjection l $ gBy p

-- | @.flatMap@ step.
--
-- @.flatMap@ step is at least as powerful as 'Transform', even if the
-- child walk is 'Filter' type. This is because @.flatMap@ step always
-- modifies the path of the Traverser.
--
-- @since 1.1.0.0
gFlatMap :: (Lift Transform c, Split cc c, ToGTraversal g, WalkType c, WalkType cc) => g cc s e -> Walk c s e
gFlatMap gt = unsafeWalk "flatMap" [travToG gt]

-- | Monomorphic version of 'gFlatMap'.
--
-- @since 1.1.0.0
gFlatMap' :: ToGTraversal g => g Transform s e -> Walk Transform s e
gFlatMap' gt = gFlatMap gt

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
gV' :: [Greskell (ElementID AVertex)] -> Walk Transform s AVertex
gV' = gV

-- | @.constant@ step.
--
-- @since 1.0.1.0
gConstant :: Greskell a -> Walk Transform s a
gConstant v = unsafeWalk "constant" [toGremlin v]

-- | @.unfold@ step.
--
-- Note that we use 'AsIterator' here because basically the @.unfold@
-- step does the same thing as @IteratorUtils.asIterator@ function in
-- Tinkerpop. However, Tinkerpop's implementation of @.unfold@ step
-- doesn't necessarily use @asIterator@, so there may be some corner
-- cases where @asIterator@ and @.unfold@ step behave differently.
--
-- @since 1.0.1.0
gUnfold :: AsIterator a => Walk Transform a (IteratorItem a)
gUnfold = unsafeWalk "unfold" []

-- | @.as@ step.
--
-- @.as@ step is 'Transform' because it adds the label to the
-- traverser.
--
-- @since 0.2.2.0
gAs :: AsLabel a -> Walk Transform a a
gAs l = unsafeWalk "as" [toGremlin l]

-- | @.values@ step.
--
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

-- | @.valueMap@ step.
--
-- @since 1.0.0.0
gValueMap :: Element s
          => Keys s
          -> Walk Transform s (PMap (ElementPropertyContainer s) GValue)
gValueMap keys = unsafeWalk "valueMap" $ toGremlinKeys keys

-- | @.elementMap@ step.
--
-- @since 2.0.1.0
gElementMap :: Element s
            => Keys s
            -> Walk Transform s (PMap Single GValue)
gElementMap keys = unsafeWalk "elementMap" $ toGremlinKeys keys

-- | @.select@ step with one argument.
--
-- @since 0.2.2.0
gSelect1 :: AsLabel a -> Walk Transform s a
gSelect1 l = unsafeWalk "select" [toGremlin l]

-- | @.select@ step with more than one arguments.
--
-- @since 0.2.2.0
gSelectN :: AsLabel a -> AsLabel b -> [AsLabel c] -> Walk Transform s (SelectedMap GValue)
gSelectN l1 l2 ls = unsafeWalk "select" ([toGremlin l1, toGremlin l2] ++ map toGremlin ls)

unsafeChangeEnd :: Walk c a b -> Walk c a b'
unsafeChangeEnd (Walk t) = Walk t

byStep :: WalkType t => ByProjection a b -> Walk t c c
byStep (ByProjection p) = unsafeWalk "by" [toGremlin p]

-- | @.select@ step with one argument followed by @.by@ step.
--
-- @since 0.2.2.0
gSelectBy1 :: AsLabel a -> ByProjection a b -> Walk Transform s b
gSelectBy1 l bp = modulateWith (unsafeChangeEnd $ gSelect1 l) [byStep bp]

-- | @.select@ step with more than one arguments followed by @.by@
-- step.
--
-- @since 0.2.2.0
gSelectByN :: AsLabel a -> AsLabel a -> [AsLabel a] -> ByProjection a b -> Walk Transform s (SelectedMap b)
gSelectByN l1 l2 ls bp = modulateWith (unsafeChangeEnd $ gSelectN l1 l2 ls) [byStep bp]

-- | @.project@ step.
--
-- @since 1.0.0.0
gProject :: LabeledByProjection s -> [LabeledByProjection s] -> Walk Transform s (PMap Single GValue)
gProject lp_head lps = foldl' f (unsafeWalk "project" labels) (lp_head : lps)
  where
    labels = map toLabelGremlin (lp_head : lps)
    toLabelGremlin (LabeledByProjection l _) = toGremlin l
    f acc lp = acc >>> toByStep lp
    toByStep :: LabeledByProjection s -> Walk Transform a a
    toByStep (LabeledByProjection _ (ByProjection p)) = unsafeWalk "by" [toGremlin p]

-- | @.path@ step without modulation.
--
-- @since 1.1.0.0
gPath :: Walk Transform s (Path GValue)
gPath = unsafeWalk "path" []

-- | @.path@ step with one or more @.by@ modulations.
--
-- @since 1.1.0.0
gPathBy :: ByProjection a b -> [ByProjection a b] -> Walk Transform s (Path b)
gPathBy b1 bn = modulateWith (unsafeWalk "path" []) $ map byStep $ b1 : bn

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
gOut' :: (Vertex v)
      => [Greskell Text] -- ^ edge labels
      -> Walk Transform v AVertex
gOut' = gOut

-- | @.outE@ step
gOutE :: (Vertex v, Edge e)
      => [Greskell Text] -- ^ edge labels
      -> Walk Transform v e
gOutE = genericTraversalWalk "outE"

-- | Monomorphic version of 'gOutE'.
gOutE' :: (Vertex v)
       => [Greskell Text]
       -> Walk Transform v AEdge
gOutE' = gOutE

-- | @.outV@ step.
--
-- @since 0.2.2.0
gOutV :: (Edge e, Vertex v) => Walk Transform e v
gOutV = unsafeWalk "outV" []

-- | Monomorphic version of 'gOutV'.
--
-- @since 0.2.2.0
gOutV' :: Edge e => Walk Transform e AVertex
gOutV' = gOutV

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

-- | @.inV@ step.
--
-- @since 0.2.2.0
gInV :: (Edge e, Vertex v) => Walk Transform e v
gInV = unsafeWalk "inV" []

-- | Monomorphic version of 'gInV'.
--
-- @since 0.2.2.0
gInV' :: Edge e => Walk Transform e AVertex
gInV' = gInV

-- | @.sideEffect@ step that takes a traversal.
gSideEffect :: (ToGTraversal g, WalkType c, WalkType p, Split c p) => g c s e -> Walk p s s
gSideEffect walk = unsafeWalk "sideEffect" [travToG walk]

-- | Monomorphic version of 'gSideEffect'. The result walk is always
-- 'SideEffect' type.
gSideEffect' :: (ToGTraversal g, WalkType c, Split c SideEffect) => g c s e -> Walk SideEffect s s
gSideEffect' w = gSideEffect w

-- | @.addV@ step with a label.
gAddV :: Vertex v => Greskell Text -> Walk SideEffect a v
gAddV label = unsafeWalk "addV" [toGremlin label]

-- | Monomorphic version of 'gAddV'.
gAddV' :: Greskell Text -> Walk SideEffect a AVertex
gAddV' = gAddV

-- | @.drop@ step on 'Element'.
gDrop :: Element e => Walk SideEffect e e
gDrop = unsafeWalk "drop" []

-- | @.drop@ step on 'Property'.
gDropP :: Property p => Walk SideEffect (p a) (p a)
gDropP = unsafeWalk "drop" []

-- | Simple @.property@ step. It adds a value to the property.
--
-- @since 0.2.0.0
gProperty :: Element e
          => Key e v -- ^ key of the property
          -> Greskell v -- ^ value of the property
          -> Walk SideEffect e e
gProperty key val = unsafeWalk "property" [toGremlin key, toGremlin val]

-- | @.property@ step for 'Vertex'.
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
        expand (KeyNoValue _)               = []

-- | Vertex anchor for 'gAddE'. It corresponds to @.from@ or @.to@
-- step following an @.addE@ step.
--
-- Type @s@ is the input Vertex for the @.addE@ step. Type @e@ is the
-- type of the anchor Vertex that the 'AddAnchor' yields. So, @.addE@
-- step creates an edge between @s@ and @e@.
--
-- @since 0.2.0.0
data AddAnchor s e
  = AddAnchor Text (GTraversal Transform s e)

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
-- @since 0.2.0.0
gAddE :: (Vertex vs, Vertex ve, Edge e)
      => Greskell Text
      -> AddAnchor vs ve
      -> Walk SideEffect vs e
gAddE label anch = (unsafeWalk "addE" [toGremlin label]) >>> anchorStep anch

-- | Monomorphic version of 'gAddE'.
--
-- @since 0.2.0.0
gAddE' :: Greskell Text -> AddAnchor AVertex AVertex -> Walk SideEffect AVertex AEdge
gAddE' = gAddE

-- | Examples of using this module. See the source. The 'fst' of the output is the testee, while the
-- 'snd' is the expectation.
examples :: [(Text, Text)]
examples =
  [ ( toGremlin $ source "g"
    , "g"
    )
  , ( toGremlin (source "g" & sV' (map (fmap ElementID . gvalueInt) ([1,2,3] :: [Int])))
    , "g.V(1,2,3)"
    )
  , ( toGremlin (source "g" & sE' (map (fmap ElementID . gvalueInt) ([1] :: [Int])))
    , "g.E(1)"
    )
  , ( toGremlin (source "g" & sAddV' "person")
    , "g.addV(\"person\")"
    )
  , ( toGremlin $ unsafeGTraversal "g.V().count()"
    , "g.V().count()"
    )
  , ( toGremlin (source "g" & sV' [] &. gValues ["age"])
    , "g.V().values(\"age\")"
    )
  , ( toGremlin (gValues ["age"] $. sV' [] $ source "g")
    , "g.V().values(\"age\")"
    )
  , ( toGremlin (source "g" & sAddV' "person" &. gProperty "name" "marko" & gIterate)
    , "g.addV(\"person\").property(\"name\",\"marko\").iterate()"
    )
  , ( toGremlin (source "g" & sV' [] &. unsafeWalk "valueMap" ["'foo'", "'bar'"])
    , "g.V().valueMap('foo','bar')"
    )
  , ( toGremlin (source "g" & sV' [] &. modulateWith (unsafeWalk "path" []) [unsafeWalk "by" ["'name'"], unsafeWalk "by" ["'age'"]])
    , "g.V().path().by('name').by('age')"
    )
  , ( toGremlin (source "g" & sV' [] &. gFilter (gOut' ["knows"]))
    , "g.V().filter(__.out(\"knows\"))"
    )
  , ( let la = "a" :: AsLabel AVertex
          age = "age" :: Key AVertex Int
       in toGremlin (source "g" & sV' [] &. gAs la &. gOut' [] &. gWhereP1 (pEq la) (Just $ gBy age))
    , "g.V().as(\"a\").out().where(P.eq(\"a\")).by(\"age\")"
    )
  , ( let la = "a" :: AsLabel AVertex
          lb = "b" :: AsLabel AVertex
          age = "age" :: Key AVertex Int
       in toGremlin (source "g" & sV' [] &. gAs la &. gOut' [] &. gAs lb &. gValues [age] &. gWhereP2 la (pEq lb) Nothing)
    , "g.V().as(\"a\").out().as(\"b\").values(\"age\").where(\"a\",P.eq(\"b\"))"
    )
  , ( let label_a = "a" :: AsLabel AVertex
          label_b = "b"
          key_age = "age" :: Key AVertex Int
          patterns =
            Logic.And
              ( mPattern label_a (gOut' [] >>> gAs label_b) )
              [ mPattern label_b (gHas2' key_age 25) ]
       in toGremlin (source "g" & sV' [] &. gMatch patterns &. gSelectN label_a label_b [])
    , "g.V().match(__.as(\"a\").out().as(\"b\"),__.as(\"b\").has(\"age\",25)).select(\"a\",\"b\")"
    )
  , ( toGremlin (source "g" & sV' [] &. gValues ["age" :: Key AVertex Int] &. gIs 30)
    , "g.V().values(\"age\").is(30)"
    )
  , ( toGremlin (source "g" & sV' [] &. gValues ["age" :: Key AVertex Int] &. gIsP (pLte 30))
    , "g.V().values(\"age\").is(P.lte(30))"
    )
  , ( toGremlin (source "g" & sV' [] &. gHas1 "age")
    , "g.V().has(\"age\")"
    )
  , ( toGremlin (source "g" & sV' [] &. gHas2 "age" (31 :: Greskell Int))
    , "g.V().has(\"age\",31)"
    )
  , ( toGremlin (source "g" & sV' [] &. gHas2P "age" (pBetween (30 :: Greskell Int) 40))
    , "g.V().has(\"age\",P.between(30,40))"
    )
  , ( toGremlin (source "g" & sV' [] &. gHasLabel "person")
    , "g.V().hasLabel(\"person\")"
    )
  , ( toGremlin (source "g" & sV' [] &. gHasLabelP (pEq "person"))
    , "g.V().hasLabel(P.eq(\"person\"))"
    )
  , ( toGremlin (source "g" & sV' [] &. gHasId (fmap ElementID $ gvalueInt $ (7 :: Int)))
    , "g.V().hasId(7)"
    )
  , ( toGremlin (source "g" & sV' [] &. gHasIdP (pLte $ fmap ElementID $ gvalueInt (100 :: Int)))
    , "g.V().hasId(P.lte(100))"
    )
  , ( toGremlin (source "g" & sV' [] &. gProperties [] &. gHasKey "age")
    , "g.V().properties().hasKey(\"age\")"
    )
  , ( toGremlin (source "g" & sV' [] &. gProperties ["age"] &. gHasValue (32 :: Greskell Int))
    , "g.V().properties(\"age\").hasValue(32)"
    )
  , ( toGremlin (source "g" & sV' [] &. gProperties ["age"] &. gHasValueP (pBetween (30 :: Greskell Int) 40))
    , "g.V().properties(\"age\").hasValue(P.between(30,40))"
    )
  , ( toGremlin (source "g" & sV' [] &. gAnd [gOut' ["knows"], gHas1 "age"])
    , "g.V().and(__.out(\"knows\"),__.has(\"age\"))"
    )
  , ( toGremlin (source "g" & sV' [] &. gOr [gOut' ["knows"], gHas1 "age"])
    , "g.V().or(__.out(\"knows\"),__.has(\"age\"))"
    )
  , ( toGremlin (source "g" & sV' [] &. gNot (gOut' ["knows"]))
    , "g.V().not(__.out(\"knows\"))"
    )
  , ( toGremlin (source "g" & sV' [] &. gRange 0 100)
    , "g.V().range(0,100)"
    )
  , ( toGremlin (source "g" & sV' [] &. gRepeat Nothing (gTimes 3) Nothing (gOut' []))
    , "g.V().times(3).repeat(__.out())"
    )
  , ( toGremlin (source "g" & sV' [] &. gRepeat Nothing (gUntilHead $ gHasLabel' "person") Nothing (gOut' []))
    , "g.V().until(__.hasLabel(\"person\")).repeat(__.out())"
    )
  , ( toGremlin (source "g" & sV' [] &. gRepeat Nothing (gUntilTail $ gHasLabel' "person") Nothing (gOut' []))
    , "g.V().repeat(__.out()).until(__.hasLabel(\"person\"))"
    )
  , ( toGremlin (source "g" & sV' [] &. gRepeat Nothing Nothing gEmitHead (gOut' []))
    , "g.V().emit().repeat(__.out())"
    )
  , ( toGremlin (source "g" & sV' [] &. gRepeat Nothing Nothing gEmitTail (gOut' []))
    , "g.V().repeat(__.out()).emit()"
    )
  , ( toGremlin (source "g" & sV' [] &. gRepeat Nothing Nothing (gEmitHeadT $ gHasLabel' "person") (gOut' []))
    , "g.V().emit(__.hasLabel(\"person\")).repeat(__.out())"
    )
  , ( toGremlin (source "g" & sV' [] &. gRepeat Nothing Nothing (gEmitTailT $ gHasLabel' "person") (gOut' []))
    , "g.V().repeat(__.out()).emit(__.hasLabel(\"person\"))"
    )
  , ( let loop_label = Just "the_loop"
       in toGremlin (source "g" & sV' [] &. gRepeat loop_label (gUntilTail $ gLoops loop_label >>> gIs 3) Nothing (gOut' []))
    , "g.V().repeat(\"the_loop\",__.out()).until(__.loops(\"the_loop\").is(3))"
    )
  , ( toGremlin (source "g" & sV' [] &. gLocal ( gOut' [] >>> gLimit 3 ))
    , "g.V().local(__.out().limit(3))"
    )
  , ( let key_age = "age" :: Key AVertex Int
          key_birth_year = ("birth_year" :: Key AVertex Int)
       in toGremlin (source "g" & sV' [] &. gUnion [gValues [key_age], gValues [key_birth_year]])
    , "g.V().union(__.values(\"age\"),__.values(\"birth_year\"))"
    )
  , ( toGremlin (source "g" & sV' [] &. gCoalesce [gOut' [], gIn' []])
    , "g.V().coalesce(__.out(),__.in())"
    )
  , ( let key_age = "age" :: Key AVertex Int
       in toGremlin (source "g" & sV' [] &. gChoose3 (gHas2' key_age 30) (gIn' []) (gOut' []))
    , "g.V().choose(__.has(\"age\",30),__.in(),__.out())"
    )
  , ( toGremlin (source "g" & sV' [] &. gDedup Nothing)
    , "g.V().dedup()"
    )
  , ( let key_age = "age" :: Key AVertex Int
       in toGremlin (source "g" & sV' [] &. gDedup (Just $ gBy key_age))
    , "g.V().dedup().by(\"age\")"
    )
  , ( let label_a = "a" :: AsLabel AVertex
          label_b = "b" :: AsLabel AVertex
       in toGremlin (source "g" & sV' [] &. gAs label_a &. gOut' [] &. gAs label_b &. gDedupN label_a [label_b] Nothing)
    , "g.V().as(\"a\").out().as(\"b\").dedup(\"a\",\"b\")"
    )
  , ( let key_age = "age" :: Key AVertex Int
       in toGremlin (source "g" & sV' [] &. gOrder [gBy1 key_age])
    , "g.V().order().by(\"age\")"
    )
  , ( let key_age = "age" :: Key AVertex Int
       in toGremlin (source "g" & sV' [] &. gOrder [gBy2 key_age oDecr, gBy1 tId])
    , "g.V().order().by(\"age\",Order.decr).by(T.id)"
    )
  , ( toGremlin (source "g" & sV' [] &. gOrder [gBy2 (gOut' ["knows"] >>> gCount) oIncr, gBy2 tId oIncr])
    , "g.V().order().by(__.out(\"knows\").count(),Order.incr).by(T.id,Order.incr)"
    )
  , ( toGremlin (source "g" & sV' [] &. gOrder ["age"])
    , "g.V().order().by(\"age\")"
    )
  , ( toGremlin (source "g" & sV' [] &. gFlatMap (gOut' ["knows"] >>> gOut' ["created"]))
    , "g.V().flatMap(__.out(\"knows\").out(\"created\"))"
    )
  , ( toGremlin (source "g" & sV' [] &. gConstant (10 :: Greskell Int))
    , "g.V().constant(10)"
    )
  , ( toGremlin (source "g" & sV' [] &. gFold &. gUnfold)
    , "g.V().fold().unfold()"
    )
  , ( toGremlin (source "g" & sV' [] &. gValues ["name", "age"])
    , "g.V().values(\"name\",\"age\")"
    )
  , ( toGremlin (source "g" & sV' [] &. gProperties ["age"])
    , "g.V().properties(\"age\")"
    )
  , ( toGremlin (source "g" & sV' [] &. gValueMap KeysNil)
    , "g.V().valueMap()"
    )
  , ( toGremlin (source "g" & sV' [] &. gValueMap ("name" -: "age" -: KeysNil))
    , "g.V().valueMap(\"name\",\"age\")"
    )
  , ( toGremlin (source "g" & sV' [] &. gElementMap KeysNil)
    , "g.V().elementMap()"
    )
  , ( toGremlin (source "g" & sV' [] &. gElementMap ("name" -: "age" -: KeysNil))
    , "g.V().elementMap(\"name\",\"age\")"
    )
  , ( let name_label = "a" :: AsLabel Text
          name_key = "name" :: Key AVertex Text
          count_label = "b" :: AsLabel Int
       in toGremlin (source "g" & sV' [] &. gProject (gByL name_label name_key) [gByL count_label (gOut' [] >>> gCount), gByL "c" tId])
    , "g.V().project(\"a\",\"b\",\"c\").by(\"name\").by(__.out().count()).by(T.id)"
    )
  , ( let inE = gInE' [] :: Walk Transform AVertex AEdge
       in toGremlin (source "g" & sV' [] &. gOut' [] &. gPathBy "name" [gBy $ inE >>> gValues ["relation"]])
    , "g.V().out().path().by(\"name\").by(__.inE().values(\"relation\"))"
    )
  , ( toGremlin (source "g" & sV' [fmap ElementID $ gvalueInt (8 :: Int)] &. gOut' ["knows"])
    , "g.V(8).out(\"knows\")"
    )
  , ( toGremlin (source "g" & sV' [] & liftWalk &. gHas2 "name" "marko" &. gSideEffect' (gAddV' "toshio"))
    , "g.V().has(\"name\",\"marko\").sideEffect(__.addV(\"toshio\"))"
    )
  , ( toGremlin (source "g" & sV' [] &. gHas2 "name" "marko" & liftWalk &. gDrop)
    , "g.V().has(\"name\",\"marko\").drop()"
    )
  , ( toGremlin (source "g" & sE' [] &. gProperties ["weight"] & liftWalk &. gDropP)
    , "g.E().properties(\"weight\").drop()"
    )
  , ( toGremlin (source "g" & sV' [] & liftWalk &. gProperty "age" (20 :: Greskell Int))
    , "g.V().property(\"age\",20)"
    )
  , ( let key_location = "location" :: Key AVertex Text
          key_since = "since" :: Key (AVertexProperty Text) Text
          key_score = "score" :: Key (AVertexProperty Text) Int
       in toGremlin (source "g" & sV' [] & liftWalk &. gPropertyV (Just cList) key_location "New York" [key_since =: "2012-09-23", key_score =: 8])
    , "g.V().property(list,\"location\",\"New York\",\"since\",\"2012-09-23\",\"score\",8)"
    )
  , ( let key_name = "name" :: Key AVertex Text
       in toGremlin (source "g" & sV' [] & liftWalk &. gAddE' "knows" (gFrom $ gV' [] >>> gHas2 key_name "marko"))
    , "g.V().addE(\"knows\").from(__.V().has(\"name\",\"marko\"))"
    )
  , ( let key_name = "name" :: Key AVertex Text
       in toGremlin (source "g" & sV' [] &. gHas2 key_name "marko" & liftWalk &. gAddE' "knows" (gTo $ gV' []))
    , "g.V().has(\"name\",\"marko\").addE(\"knows\").to(__.V())"
    )
  ]
