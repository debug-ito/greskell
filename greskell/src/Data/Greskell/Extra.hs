{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
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
    lookupAs,
    lookupAs',
    lookupListAs,
    lookupListAs',
    pMapToFail,
    -- * Property writers
    writeKeyValues,
    (<=:>),
    (<=?>),
    writePropertyKeyValues,
    writePMapProperties,
    -- * Control idioms
    gWhenEmptyInput
  ) where

import Data.Aeson (ToJSON)
import Control.Category ((<<<))
import Data.Foldable (Foldable)
import Data.Greskell.Binder (Binder, newBind)
import Data.Greskell.Graph
  ( Property(..), Element, KeyValue(..), (=:), Key
  )
import qualified Data.Greskell.Graph as Graph
import Data.Greskell.GTraversal
  ( Walk, WalkType, SideEffect, Transform,
    ToGTraversal(..), Split, Lift, liftWalk,
    gProperty, gCoalesce, gUnfold, gFold
  )
import Data.Greskell.PMap
  ( PMap, pMapToList,
    lookupAs,
    lookupAs',
    lookupListAs,
    lookupListAs',
    pMapToFail
  )
import Data.Monoid (mconcat)
import Data.Text (Text)

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Control.Category ((>>>))
-- >>> import Data.Function ((&))
-- >>> import Data.Greskell.Binder (runBinder)
-- >>> import Data.Greskell.Greskell (toGremlin)
-- >>> import Data.Greskell.Graph (AVertex)
-- >>> import Data.Greskell.GTraversal (GTraversal, source, sV', gHas2, (&.), gAddV)
-- >>> import Data.List (sortBy)
-- >>> import Data.Ord (comparing)
-- >>> import qualified Data.HashMap.Strict as HashMap

-- $readers
--
-- Re-export property readers.
--
-- @since 1.0.0.0

-- | Make a series of @.property@ steps to write the given key-value
-- pairs as properties.
--
-- @since 0.2.3.0
--
-- >>> let binder = (writePropertyKeyValues [("age", (21 :: Int))] :: Binder (Walk SideEffect AVertex AVertex))
-- >>> let (walk, binding) = runBinder binder
-- >>> toGremlin walk
-- "__.property(\"age\",__v0).identity()"
-- >>> sortBy (comparing fst) $ HashMap.toList binding
-- [("__v0",Number 21.0)]
writePropertyKeyValues :: (ToJSON v, Element e) => [(Text, v)] -> Binder (Walk SideEffect e e)
writePropertyKeyValues pairs = fmap writeKeyValues $ mapM toKeyValue pairs
  where
    toKeyValue (key, value) = Graph.key key <=:> value

-- | Make a series of @.property@ steps to write the given key-value
-- pairs as properties. Use '<=:>' and '<=?>' to make a 'KeyValue'
-- within 'Binder'.
--
-- >>> let keyAge = ("age" :: Key AVertex Int)
-- >>> let keyName = ("name" :: Key AVertex Text)
-- >>> let (walk, binding) = runBinder $ writeKeyValues <$> sequence [keyAge <=:> 21, keyName <=:> "Josh"]
-- >>> toGremlin walk
-- "__.property(\"age\",__v0).property(\"name\",__v1).identity()"
-- >>> sortBy (comparing fst) $ HashMap.toList binding
-- [("__v0",Number 21.0),("__v1",String "Josh")]
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
-- >>> let keyNName = ("nickname" :: Key AVertex (Maybe Text))
-- >>> let keyCompany = ("company" :: Key AVertex (Maybe Text))
-- >>> let (walk, binding) = runBinder $ writeKeyValues <$> sequence [keyNName <=?> Nothing, keyCompany <=?> Just "foobar.com"]
-- >>> toGremlin walk
-- "__.property(\"company\",__v0).identity()"
-- >>> sortBy (comparing fst) $ HashMap.toList binding
-- [("__v0",String "foobar.com")]
--
-- @since 1.0.0.0
(<=?>) :: ToJSON b => Key a (Maybe b) -> Maybe b -> Binder (KeyValue a)
(<=?>) k v@(Just _) = k <=:> v
(<=?>) k Nothing = return $ KeyNoValue k

-- | The result 'Walk' emits the input elements as-is when there is at
-- least one input element. If there is no input element, it runs the
-- body traversal and outputs its result.
--
-- You can use this function to implement \"upsert\" a vertex
-- (i.e. add a vertex if not exist).
--
-- >>> let getMarko = (source "g" & sV' [] &. gHas2 "name" "marko" :: GTraversal Transform () AVertex)
-- >>> let upsertMarko = (liftWalk getMarko &. gWhenEmptyInput (gAddV "person" >>> gProperty "name" "marko") :: GTraversal SideEffect () AVertex)
--
-- See also: https://stackoverflow.com/questions/46027444/
--
-- @since 1.1.0.0
gWhenEmptyInput :: (ToGTraversal g, Split cc c, Lift Transform cc, Lift Transform c, WalkType c, WalkType cc)
                => g cc [s] s -- ^ the body traversal
                -> Walk c s s
gWhenEmptyInput body = gCoalesce
                       [ liftWalk $ toGTraversal gUnfold,
                         toGTraversal body
                       ] <<< liftWalk gFold
