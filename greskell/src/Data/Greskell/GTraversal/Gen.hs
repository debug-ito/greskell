{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
-- | __This module is experimental. It may have breaking changes in future.__
--
-- This module has Gremlin traversals defined in 'Data.Greskell.GTraversal' generalized on the
-- walk type. It may save you from calling 'liftWalk' manually.
--
-- @since 2.0.3.0
module Data.Greskell.GTraversal.Gen
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
      -- * Walk/Steps
    , unsafeWalk
    , modulateWith
      -- ** Filter steps
    , gIdentity
    , gFilter
    , gCyclicPath
    , gSimplePath
      -- ** Is step
    , gIs
    , gIsP
      -- ** Has steps
    , gHas1
    , gHas2
    , gHas2P
    , gHasLabel
    , gHasLabelP
    , gHasId
    , gHasIdP
    , gHasKey
    , gHasKeyP
    , gHasValue
    , gHasValueP
      -- ** Logic steps
    , gAnd
    , gOr
    , gNot
      -- ** Where step
    , gWhereP1
    , gWhereP2
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
      -- ** @.by@ steps
    , ByProjection (..)
    , ProjectionLike (..)
    , ByComparator (..)
    , LabeledByProjection (..)
    , gBy
    , gBy1
    , gBy2
    , gByL
    ) where

import           Data.Text                (Text)

import           Data.Greskell.AsIterator (AsIterator (IteratorItem))
import           Data.Greskell.AsLabel    (AsLabel, LabeledP, SelectedMap)
import           Data.Greskell.Graph      (AEdge, AVertex, Cardinality, Edge, Element (..),
                                           ElementID, Key, KeyValue, Keys, Path, Property, Vertex)
import           Data.Greskell.GraphSON   (GValue)
import           Data.Greskell.Greskell   (Greskell)
import           Data.Greskell.GTraversal (AddAnchor, ByComparator (..), ByProjection (..), Filter,
                                           GTraversal (..), GraphTraversal, GraphTraversalSource,
                                           LabeledByProjection (..), Lift, MatchPattern (..),
                                           MatchResult, ProjectionLike (..), RepeatEmit (..),
                                           RepeatLabel (..), RepeatPos (..), RepeatUntil (..),
                                           SideEffect, Split, ToGTraversal (..), Transform, Walk,
                                           WalkType, gAnd, gBarrier, gBy, gBy1, gBy2, gByL,
                                           gChoose3, gCoalesce, gCyclicPath, gEmitHead, gEmitHeadT,
                                           gEmitTail, gEmitTailT, gFilter, gFlatMap, gHas1, gHas2,
                                           gHas2P, gHasId, gHasIdP, gHasKey, gHasKeyP, gHasLabel,
                                           gHasLabelP, gHasValue, gHasValueP, gIdentity, gIs, gIsP,
                                           gIterate, gLocal, gNot, gOr, gRepeat, gSideEffect,
                                           gSimplePath, gTimes, gUnion, gUntilHead, gUntilTail,
                                           gWhereP1, gWhereP2, mPattern, modulateWith, source,
                                           unsafeGTraversal, unsafeWalk, ($.), (&.), (<$.>), (<*.>))
import qualified Data.Greskell.GTraversal as G
import           Data.Greskell.Logic      (Logic)
import           Data.Greskell.PMap       (PMap, Single)

sV :: (Vertex v, WalkType c, Lift Transform c) => [Greskell (ElementID v)] -> Greskell GraphTraversalSource -> GTraversal c () v
sV a b = liftWalk $ G.sV a b

sV' :: (WalkType c, Lift Transform c) => [Greskell (ElementID AVertex)] -> Greskell GraphTraversalSource -> GTraversal c () AVertex
sV' = sV

sE :: (Edge e, WalkType c, Lift Transform c) => [Greskell (ElementID e)] -> Greskell GraphTraversalSource -> GTraversal c () e
sE a b = liftWalk $ G.sE a b

sE' :: (WalkType c, Lift Transform c) => [Greskell (ElementID AEdge)] -> Greskell GraphTraversalSource -> GTraversal c () AEdge
sE' = sE

sAddV :: (Vertex v, WalkType c, Lift SideEffect c) => Greskell Text -> Greskell GraphTraversalSource -> GTraversal c () v
sAddV a b = liftWalk $ G.sAddV a b

sAddV' :: (WalkType c, Lift SideEffect c) => Greskell Text -> Greskell GraphTraversalSource -> GTraversal c () AVertex
sAddV' = sAddV

gOrder :: (WalkType c, Lift Transform c) => [ByComparator s] -> Walk c s s
gOrder b = liftWalk $ G.gOrder b

gRange :: (WalkType c, Lift Transform c) => Greskell Int -> Greskell Int -> Walk c s s
gRange a b = liftWalk $ G.gRange a b

gLimit :: (WalkType c, Lift Transform c) => Greskell Int -> Walk c s s
gLimit a = liftWalk $ G.gLimit a

gTail :: (WalkType c, Lift Transform c) => Greskell Int -> Walk c s s
gTail a = liftWalk $ G.gTail a

gSkip :: (WalkType c, Lift Transform c) => Greskell Int -> Walk c s s
gSkip a = liftWalk $ G.gSkip a

gLoops :: (WalkType c, Lift Transform c) => Maybe RepeatLabel -> Walk c s Int
gLoops a = liftWalk $ G.gLoops a

gDedup :: (WalkType c, Lift Transform c) => Maybe (ByProjection s e) -> Walk c s s
gDedup a =  liftWalk $ G.gDedup a

gDedupN :: (WalkType c, Lift Transform c) => AsLabel a -> [AsLabel a] -> Maybe (ByProjection a e) -> Walk c s s
gDedupN a b c = liftWalk $ G.gDedupN a b c

gV :: (Vertex v, WalkType c, Lift Transform c) => [Greskell (ElementID v)] -> Walk c s v
gV a = liftWalk $ G.gV a

gV' :: (WalkType c, Lift Transform c) => [Greskell (ElementID AVertex)] -> Walk c s AVertex
gV' = gV

gConstant :: (WalkType c, Lift Transform c) => Greskell a -> Walk c s a
gConstant a = liftWalk $ G.gConstant a

gProject :: (WalkType c, Lift Transform c) => LabeledByProjection s -> [LabeledByProjection s] -> Walk c s (PMap Single GValue)
gProject a b = liftWalk $ G.gProject a b

gAs :: (WalkType c, Lift Transform c) => AsLabel a -> Walk c a a
gAs a = liftWalk $ G.gAs a

gValues :: (Element s, WalkType c, Lift Transform c) => [Key s e] -> Walk c s e
gValues a = liftWalk $ G.gValues a

gProperties :: (Element s, Property p, ElementProperty s ~ p, WalkType c, Lift Transform c) => [Key s v] -> Walk c s (p v)
gProperties a = liftWalk $ G.gProperties a

gId :: (Element s, WalkType c, Lift Transform c) => Walk c s (ElementID s)
gId = liftWalk G.gId

gLabel :: (Element s, WalkType c, Lift Transform c) => Walk c s Text
gLabel = liftWalk G.gLabel

gValueMap :: (Element s, WalkType c, Lift Transform c) => Keys s -> Walk c s (PMap (ElementPropertyContainer s) GValue)
gValueMap a = liftWalk $ G.gValueMap a

gElementMap :: (Element s, WalkType c, Lift Transform c) => Keys s -> Walk c s (PMap Single GValue)
gElementMap a = liftWalk $ G.gElementMap a

gSelect1 :: (WalkType c, Lift Transform c) => AsLabel a -> Walk c s a
gSelect1 a = liftWalk $ G.gSelect1 a

gSelectN :: (WalkType c, Lift Transform c) => AsLabel a -> AsLabel b -> [AsLabel c] -> Walk c s (SelectedMap GValue)
gSelectN a b c = liftWalk $ G.gSelectN a b c

gSelectBy1 :: (WalkType c, Lift Transform c) => AsLabel a -> ByProjection a b -> Walk c s b
gSelectBy1 a b = liftWalk $ G.gSelectBy1 a b

gSelectByN :: (WalkType c, Lift Transform c) => AsLabel a -> AsLabel a -> [AsLabel a] -> ByProjection a b -> Walk c s (SelectedMap b)
gSelectByN a b c d = liftWalk $ G.gSelectByN a b c d

gUnfold :: (AsIterator a, WalkType c, Lift Transform c) => Walk c a (IteratorItem a)
gUnfold = liftWalk $ G.gUnfold

gPath :: (WalkType c, Lift Transform c) => Walk c s (Path GValue)
gPath = liftWalk $ G.gPath

gPathBy :: (WalkType c, Lift Transform c) => ByProjection a b -> [ByProjection a b] -> Walk c s (Path b)
gPathBy a b = liftWalk $ G.gPathBy a b

gFold :: (WalkType c, Lift Transform c) => Walk c a [a]
gFold = liftWalk $ G.gFold

gCount :: (WalkType c, Lift Transform c) => Walk c a Int
gCount = liftWalk $ G.gCount

gOut :: (Vertex v1, Vertex v2, WalkType c, Lift Transform c) => [Greskell Text] -> Walk c v1 v2
gOut a = liftWalk $ G.gOut a

gOut' :: (Vertex v, WalkType c, Lift Transform c) => [Greskell Text] -> Walk c v AVertex
gOut' = gOut

gOutE :: (Vertex v, Edge e, WalkType c, Lift Transform c) => [Greskell Text] -> Walk c v e
gOutE a = liftWalk $ G.gOutE a

gOutE' :: (Vertex v, WalkType c, Lift Transform c) => [Greskell Text] -> Walk c v AEdge
gOutE' = gOutE

gOutV :: (Edge e, Vertex v, WalkType c, Lift Transform c) => Walk c e v
gOutV = liftWalk $ G.gOutV

gOutV' :: (Edge e, WalkType c, Lift Transform c) => Walk c e AVertex
gOutV' = gOutV

gIn :: (Vertex v1, Vertex v2, WalkType c, Lift Transform c) => [Greskell Text] -> Walk c v1 v2
gIn a = liftWalk $ G.gIn a

gIn' :: (Vertex v, WalkType c, Lift Transform c) => [Greskell Text] -> Walk c v AVertex
gIn' = gIn

gInE :: (Vertex v, Edge e, WalkType c, Lift Transform c) => [Greskell Text] -> Walk c v e
gInE a = liftWalk $ G.gInE a

gInE' :: (Vertex v, WalkType c, Lift Transform c) => [Greskell Text] -> Walk c v AEdge
gInE' = gInE

gInV :: (Edge e, Vertex v, WalkType c, Lift Transform c) => Walk c e v
gInV = liftWalk $ G.gInV

gInV' :: (Edge e, WalkType c, Lift Transform c) => Walk c e AVertex
gInV' = gInV

gMatch :: (WalkType c, Lift Transform c) => Logic MatchPattern -> Walk c a MatchResult
gMatch a = liftWalk $ G.gMatch a

gAddV :: (Vertex v, WalkType c, Lift SideEffect c) => Greskell Text -> Walk c a v
gAddV a = liftWalk $ G.gAddV a

gAddV' :: (WalkType c, Lift SideEffect c) => Greskell Text -> Walk c a AVertex
gAddV' = gAddV

gAddE :: (Vertex vs, Vertex ve, Edge e, WalkType c, Lift SideEffect c) => Greskell Text -> AddAnchor vs ve -> Walk c vs e
gAddE a b = liftWalk $ G.gAddE a b

gAddE' :: (WalkType c, Lift SideEffect c) => Greskell Text -> AddAnchor AVertex AVertex -> Walk c AVertex AEdge
gAddE' = gAddE

gFrom :: (ToGTraversal g, WalkType c, Lift c Transform) => g c s e -> AddAnchor s e
gFrom a = G.gFrom $ liftWalk a

gTo :: (ToGTraversal g, WalkType c, Lift c Transform) => g c s e -> AddAnchor s e
gTo a = G.gTo $ liftWalk a

gDrop :: (Element e, WalkType c, Lift SideEffect c) => Walk c e e
gDrop = liftWalk G.gDrop

gDropP :: (Property p, WalkType c, Lift SideEffect c) => Walk c (p a) (p a)
gDropP = liftWalk G.gDropP

gProperty :: (Element e, WalkType c, Lift SideEffect c) => Key e v -> Greskell v -> Walk c e e
gProperty a b = liftWalk $ G.gProperty a b

gPropertyV :: (Vertex e, vp ~ ElementProperty e, Property vp, Element (vp v), WalkType c, Lift SideEffect c)
           => Maybe (Greskell Cardinality) -> Key e v -> Greskell v -> [KeyValue (vp v)] -> Walk c e e
gPropertyV a b c d = liftWalk $ G.gPropertyV a b c d
