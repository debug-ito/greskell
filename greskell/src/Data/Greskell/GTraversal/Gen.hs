{-# LANGUAGE FlexibleContexts #-}
-- | This module has Gremlin traversals defined in 'Data.Greskell.GTraversal' generalized on the
-- walk type.
module Data.Greskell.GTraversal.Gen
    ( -- * GraphTraversalSource
      sV
    , sV'
    , sE
    , sE'
    , sAddV
    , sAddV'
    -- * GTraversal
    , gIterate
    -- ** Filter steps
    , gIdentity
--     , gFilter
--     , gCyclicPath
--     , gCyclicPath'
--     , gSimplePath
--     , gSimplePath'
--       -- ** Is step
--     , gIs
--     , gIs'
--     , gIsP
--     , gIsP'
--       -- ** Has steps
--     , gHas1
--     , gHas1'
--     , gHas2
--     , gHas2'
--     , gHas2P
--     , gHas2P'
--     , gHasLabel
--     , gHasLabel'
--     , gHasLabelP
--     , gHasLabelP'
--     , gHasId
--     , gHasId'
--     , gHasIdP
--     , gHasIdP'
--     , gHasKey
--     , gHasKey'
--     , gHasKeyP
--     , gHasKeyP'
--     , gHasValue
--     , gHasValue'
--     , gHasValueP
--     , gHasValueP'
--       -- ** Logic steps
--     , gAnd
--     , gOr
--     , gNot
--       -- ** Where step
--     , gWhereP1
--     , gWhereP1'
--     , gWhereP2
--     , gWhereP2'
--       -- ** Sorting steps
--     , gOrder
--       -- ** Paging steps
--     , gRange
--     , gLimit
--     , gTail
--     , gSkip
--       -- ** Repeat step
--     , gRepeat
--     , gTimes
--     , gUntilHead
--     , gUntilTail
--     , gEmitHead
--     , gEmitTail
--     , gEmitHeadT
--     , gEmitTailT
--     , gLoops
--     , RepeatUntil (..)
--     , RepeatEmit (..)
--     , RepeatPos (..)
--     , RepeatLabel (..)
--       -- ** Branching steps
--     , gLocal
--     , gUnion
--     , gCoalesce
--     , gChoose3
--       -- ** Barrier steps
--     , gBarrier
--     , gDedup
--     , gDedupN
--       -- ** Transformation steps
--     , gFlatMap
--     , gFlatMap'
--     , gV
--     , gV'
--     , gConstant
--     , gProject
--       -- ** As step
--     , gAs
--       -- ** Accessor steps
--     , gValues
--     , gProperties
--     , gId
--     , gLabel
--     , gValueMap
--     , gElementMap
--     , gSelect1
--     , gSelectN
--     , gSelectBy1
--     , gSelectByN
--     , gUnfold
--     , gPath
--     , gPathBy
--       -- ** Summarizing steps
--     , gFold
--     , gCount
--       -- ** Graph traversal steps
--     , gOut
--     , gOut'
--     , gOutE
--     , gOutE'
--     , gOutV
--     , gOutV'
--     , gIn
--     , gIn'
--     , gInE
--     , gInE'
--     , gInV
--     , gInV'
--       -- ** Match step
--     , gMatch
--     , MatchPattern (..)
--     , mPattern
--     , MatchResult
--       -- ** Side-effect steps
--     , gSideEffect
--     , gSideEffect'
--       -- ** Graph manipulation steps
--     , gAddV
--     , gAddV'
--     , gAddE
--     , gAddE'
--     , AddAnchor
--     , gFrom
--     , gTo
--     , gDrop
--     , gDropP
--     , gProperty
--     , gPropertyV
    ) where

import           Data.Greskell.Graph      (AEdge, AVertex, Edge, ElementID, Vertex)
import           Data.Greskell.Greskell   (Greskell)
import           Data.Greskell.GTraversal (GTraversal, GraphTraversalSource, Lift, SideEffect,
                                           ToGTraversal (..), Transform, WalkType, gIdentity,
                                           gIterate)
import qualified Data.Greskell.GTraversal as G

sV :: (Vertex v, WalkType c, Lift Transform c) => [Greskell (ElementID v)] -> Greskell GraphTraversalSource -> GTraversal c () v
sV ids src = liftWalk $ G.sV ids src

sV' :: (WalkType c, Lift Transform c) => [Greskell (ElementID AVertex)] -> Greskell GraphTraversalSource -> GTraversal c () AVertex
sV' = sV

sE :: (Edge e, WalkType c, Lift Transform c) => [Greskell (ElementID e)] -> Greskell GraphTraversalSource -> GTraversal c () e
sE ids src = liftWalk $ G.sE ids src

sE' :: (WalkType c, Lift Transform c) => [Greskell (ElementID AEdge)] -> Greskell GraphTraversalSource -> GTraversal c () AEdge
sE' = sE

sAddV :: (Vertex v, WalkType c, Lift SideEffect c) => Greskell Text -> Greskell GraphTraversalSource -> GTraversal c () v
sAddV label src = liftWalk $ G.sAddV label src

sAddV' :: (WalkType c, Lift SideEffect c) => Greskell Text -> Greskell GraphTraversalSource -> GTraversal c () AVertex
sAddV' = sAddV

