{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Data.Greskell.Extra
-- Description: Extra utility functions implemented by Greskell
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- Extra utility functions implemented by Greskell.
--
-- @since 0.2.3.0
module Data.Greskell.Extra
    ( -- * Property readers
      -- $readers
      lookupAs
    , lookupAs'
    , lookupListAs
    , lookupListAs'
    , pMapToFail
      -- * Property writers
    , writeKeyValues
    , (<=:>)
    , (<=?>)
    , writePropertyKeyValues
    , writePMapProperties
      -- * Control idioms
    , gWhenEmptyInput
      -- * Examples
    , examples
    ) where

import           Control.Category         ((<<<), (>>>))
import           Data.Aeson               (ToJSON)
import qualified Data.Aeson.KeyMap        as KeyMap
import           Data.Foldable            (Foldable)
import           Data.Function            ((&))
import           Data.Greskell.Binder     (Binder, newBind, runBinder)
import           Data.Greskell.Graph      (AVertex, Element, Key, KeyValue (..), Property (..),
                                           (=:))
import qualified Data.Greskell.Graph      as Graph
import           Data.Greskell.Greskell   (toGremlin)
import           Data.Greskell.GTraversal (GTraversal, Lift, SideEffect, Split, ToGTraversal (..),
                                           Transform, Walk, WalkType, gAddV, gCoalesce, gFold,
                                           gHas2, gProperty, gUnfold, liftWalk, sV', source, (&.))
import           Data.Greskell.PMap       (PMap, lookupAs, lookupAs', lookupListAs, lookupListAs',
                                           pMapToFail, pMapToList)
import           Data.List                (sortBy)
import           Data.Monoid              (mconcat)
import           Data.Ord                 (comparing)
import           Data.Text                (Text, unpack)


-- $readers
--
-- Re-export property readers.
--
-- @since 1.0.0.0

-- | Make a series of @.property@ steps to write the given key-value
-- pairs as properties.
--
-- @since 0.2.3.0
writePropertyKeyValues :: (ToJSON v, Element e) => [(Text, v)] -> Binder (Walk SideEffect e e)
writePropertyKeyValues pairs = fmap writeKeyValues $ mapM toKeyValue pairs
  where
    toKeyValue (key, value) = Graph.key key <=:> value

-- | Make a series of @.property@ steps to write the given key-value
-- pairs as properties. Use '<=:>' and '<=?>' to make a 'KeyValue'
-- within 'Binder'.
--
-- @since 1.0.0.0
writeKeyValues :: Element e => [KeyValue e] -> Walk SideEffect e e
writeKeyValues pairs = mconcat $ toPropStep =<< pairs
  where
    toPropStep (KeyValue k v) = [gProperty k v]
    toPropStep (KeyNoValue _) = []

-- | Make a series of @.property@ steps to write all properties in the
-- given 'PMap'.
--
-- @since 1.0.0.0
writePMapProperties :: (Foldable c, ToJSON v, Element e)
                    => PMap c v -> Binder (Walk SideEffect e e)
writePMapProperties = writePropertyKeyValues . pMapToList

-- | Like '=:', but this one takes a real value, binds it into a
-- 'Greskell' value and returns 'KeyValue'.
--
-- @since 1.0.0.0
(<=:>) :: ToJSON b => Key a b -> b -> Binder (KeyValue a)
(<=:>) k v = (=:) k <$> newBind v

-- | Like '<=:>', but this one is for an optional property. If the
-- value is 'Just', it's equivalent to '<=:>'. If the value is
-- 'Nothing', it returns 'KeyNoValue'.
--
-- @since 1.0.0.0
(<=?>) :: ToJSON b => Key a (Maybe b) -> Maybe b -> Binder (KeyValue a)
(<=?>) k v@(Just _) = k <=:> v
(<=?>) k Nothing    = return $ KeyNoValue k

-- | The result 'Walk' emits the input elements as-is when there is at
-- least one input element. If there is no input element, it runs the
-- body traversal once and outputs its result.
--
-- You can use this function to implement \"upsert\" a vertex
-- (i.e. add a vertex if not exist).
--
-- See also: https://stackoverflow.com/questions/46027444/
--
-- @since 1.1.0.0
gWhenEmptyInput :: (ToGTraversal g, Split cc c, Lift Transform cc, Lift Transform c, WalkType c, WalkType cc)
                => g cc [s] s -- ^ the body traversal
                -> Walk c s s -- ^ the result walk
gWhenEmptyInput body = gCoalesce
                       [ liftWalk $ toGTraversal gUnfold,
                         toGTraversal body
                       ] <<< liftWalk gFold

-- | Examples of using this module. See the source. The 'fst' of the output is the testee, while the
-- 'snd' is the expectation.
examples :: [(String, String)]
examples = for_writePropertyKeyValues ++ for_writeKeyValues ++ for_operators ++ for_gWhenEmptyInput
  where
    for_writePropertyKeyValues =
      let binder = (writePropertyKeyValues [("age", (21 :: Int))] :: Binder (Walk SideEffect AVertex AVertex))
          (walk, binding) = runBinder binder
      in [ (unpack $ toGremlin walk, "__.property(\"age\",__v0).identity()")
         , (show $ sortBy (comparing fst) $ KeyMap.toList binding, "[(\"__v0\",Number 21.0)]")
         ]
    for_writeKeyValues =
      let keyAge = ("age" :: Key AVertex Int)
          keyName = ("name" :: Key AVertex Text)
          (walk, binding) = runBinder $ writeKeyValues <$> sequence [keyAge <=:> 21, keyName <=:> "Josh"]
      in [ (unpack $ toGremlin walk, "__.property(\"age\",__v0).property(\"name\",__v1).identity()")
         , (show $ sortBy (comparing fst) $ KeyMap.toList binding, "[(\"__v0\",Number 21.0),(\"__v1\",String \"Josh\")]")
         ]
    for_operators =
      let keyNName = ("nickname" :: Key AVertex (Maybe Text))
          keyCompany = ("company" :: Key AVertex (Maybe Text))
          (walk, binding) = runBinder $ writeKeyValues <$> sequence [keyNName <=?> Nothing, keyCompany <=?> Just "foobar.com"]
      in [ (unpack $ toGremlin walk, "__.property(\"company\",__v0).identity()")
         , (show $ sortBy (comparing fst) $ KeyMap.toList binding, "[(\"__v0\",String \"foobar.com\")]")
         ]
    for_gWhenEmptyInput =
      let getMarko = (source "g" & sV' [] &. gHas2 "name" "marko" :: GTraversal Transform () AVertex)
          upsertMarko = (liftWalk getMarko &. gWhenEmptyInput (gAddV "person" >>> gProperty "name" "marko") :: GTraversal SideEffect () AVertex)
      in [ (unpack $ toGremlin upsertMarko, "g.V().has(\"name\",\"marko\").fold().coalesce(__.unfold(),__.addV(\"person\").property(\"name\",\"marko\"))")
         ]

