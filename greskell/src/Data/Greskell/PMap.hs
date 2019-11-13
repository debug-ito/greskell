{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveTraversable, TypeFamilies, OverloadedStrings #-}
-- |
-- Module: Data.Greskell.PMap
-- Description: Property map, a map with Text keys and cardinality options
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.PMap
  ( -- * PMap
    PMap,
    -- ** Single lookup
    lookup,
    lookupM,
    lookupAs,
    lookupAsM,
    -- ** List lookup
    lookupList,
    lookupListAs,
    lookupListAsM,
    -- ** Others
    pMapInsert,
    pMapDelete,
    pMapLookup,
    pMapToList,
    pMapFromList,
    -- * Cardinality
    Single,
    Multi,
    -- * PMapKey
    PMapKey(..),
    -- * Errors
    PMapLookupException(..)
  ) where

import Prelude hiding (lookup)

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow(..))
import qualified Data.Foldable as F
import Data.Functor.Identity (Identity)
import Data.Greskell.AsIterator (AsIterator(..))
import Data.Greskell.GMap (GMapEntry)
import Data.Greskell.GraphSON (GValue, GraphSONTyped(..), FromGraphSON(..), parseEither)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup((<>)))
import qualified Data.Semigroup as S
import Data.Traversable (Traversable(traverse))
import Data.Text (Text)

import Data.Greskell.NonEmptyLike (NonEmptyLike)
import qualified Data.Greskell.NonEmptyLike as NEL

-- | A property map, which has text keys and @v@ values. @c@ specifies
-- the cardinality of each item, and should be an instance of
-- 'NonEmptyLike'.
newtype PMap c v = PMap (HM.HashMap Text (c v))
                 deriving (Show,Eq,Functor,Foldable,Traversable)

instance GraphSONTyped (PMap c v) where
  gsonTypeFor _ = "g:Map"

instance FromGraphSON (c v) => FromGraphSON (PMap c v) where
  parseGraphSON gv = fmap PMap $ parseGraphSON gv

-- | Make a union of the two 'PMap's. If the two 'PMap's share some
-- keys, those values are merged by 'NEL.append' method from
-- 'NonEmptyLike'.
instance NonEmptyLike c => Semigroup (PMap c v) where
  (PMap a) <> (PMap b) = PMap (HM.unionWith NEL.append a b)

instance NonEmptyLike c => Monoid (PMap c v) where
  mempty = PMap $ HM.empty
  mappend = (<>)

instance AsIterator (PMap c v) where
  type IteratorItem (PMap c v) = GMapEntry Text (c v)

-- | Insert a key-value pair to 'PMap'. It depends on the 'NEL.append'
-- method of the 'NonEmptyLike' type @c@ how it behaves when it
-- already has items for that key.
pMapInsert :: NonEmptyLike c => Text -> v -> PMap c v -> PMap c v
pMapInsert k v (PMap hm) = PMap $ HM.insertWith NEL.append k (NEL.singleton v) hm

-- | Delete a key and all values associated with it.
pMapDelete :: Text -> PMap c v -> PMap c v
pMapDelete k (PMap hm) = PMap $ HM.delete k hm

-- | Lookup all items for the key (low-level function). If there is no
-- item for the key, it returns an empty list.
pMapLookup :: NonEmptyLike c => Text -> PMap c v -> [v]
pMapLookup k (PMap hm) = maybe [] (F.toList . NEL.toNonEmpty) $ HM.lookup k hm

-- | List up all entries.
pMapToList :: F.Foldable c => PMap c v -> [(Text, v)]
pMapToList (PMap hm) = expandValues =<< HM.toList hm
  where
    expandValues (k, cv) = map ((,) k) $ F.toList cv

-- | Make a 'PMap' from list of entries.
pMapFromList :: NonEmptyLike c => [(Text, v)] -> PMap c v
pMapFromList = F.foldr f mempty
  where
    f (k, v) pm = pMapInsert k v pm

-- | Lookup the first value for the key from 'PMap'.
lookup :: (PMapKey k, NonEmptyLike c) => k -> PMap c v -> Maybe v
lookup k pm = listToMaybe $ lookupList k pm

-- | 'MonadThrow' version of 'lookup'. If there is no value for the
-- key, it throws 'NoSuchAsLabel'.
lookupM :: (PMapKey k, NonEmptyLike c, MonadThrow m) => k -> PMap c v -> m v
lookupM k pm = maybe (throwM $ PMapNoSuchKey $ keyText k) return $ lookup k pm

-- | Lookup the value and parse it into @a@.
lookupAs :: (PMapKey k, NonEmptyLike c, PMapValue k ~ a, FromGraphSON a)
         => k -> PMap c GValue -> Either PMapLookupException a
lookupAs k pm =
  case lookup k pm of
    Nothing -> Left $ PMapNoSuchKey kt
    Just gv -> either (Left . PMapParseError kt) Right $ parseEither gv
  where
    kt = keyText k

-- | 'MonadThrow' version of 'lookupAs'.
lookupAsM :: (PMapKey k, NonEmptyLike c, PMapValue k ~ a, FromGraphSON a, MonadThrow m)
          => k -> PMap c GValue -> m a
lookupAsM k pm = either throwM return $ lookupAs k pm

-- | Lookup all items for the key. If there is no item for the key, it
-- returns an empty list.
lookupList :: (PMapKey k, NonEmptyLike c) => k -> PMap c v -> [v]
lookupList k pm = pMapLookup (keyText k) pm

-- | Look up the values and parse them into @a@.
lookupListAs :: (PMapKey k, NonEmptyLike c, PMapValue k ~ a, FromGraphSON a)
             => k -> PMap c GValue -> Either PMapLookupException (NonEmpty a)
lookupListAs k pm =
  case lookupList k pm of
    [] -> Left $ PMapNoSuchKey kt
    (x : rest) -> either (Left . PMapParseError kt) Right $ traverse parseEither (x :| rest)
  where
    kt = keyText k

-- | 'MonadThrow' version of 'lookupListAs'
lookupListAsM :: (PMapKey k, NonEmptyLike c, PMapValue k ~ a, FromGraphSON a, MonadThrow m)
              => k -> PMap c GValue -> m (NonEmpty a)
lookupListAsM k pm = either throwM return $ lookupListAs k pm

-- | The single cardinality for 'PMap'. 'pMapInsert' method replaces
-- the old value. '(<>)' on 'PMap' prefers the items from the left
-- 'PMap'. 'pMapFromList' prefers the first item for each key.
type Single = S.First

-- | The \"one or more\" cardinality for 'PMap'. 'pMapInsert' method
-- prepends the new value at the head. '(<>)' on 'PMap' appends the
-- right items to the tail of the left items. 'pMapFromList' preserves
-- the order of the items for each key.
newtype Multi a = Multi (NonEmpty a)
              deriving (Show,Eq,Ord,Functor,Semigroup,Foldable,Traversable,NonEmptyLike,FromGraphSON)

-- | A typed key for 'PMap'.
class PMapKey k where
  -- | Type of the value associate with the key.
  type PMapValue k :: *

  -- | 'Text' representation of the key.
  keyText :: k -> Text

-- | For simple 'Text', the 'GValue' is returned without being parsed.
instance PMapKey Text where
  type PMapValue Text = GValue
  keyText = id

-- | An 'Exception' raised when looking up values from 'PMap'.
data PMapLookupException =
    PMapNoSuchKey Text
  -- ^ The 'PMap' doesn't have the given key.
  | PMapParseError Text String
  -- ^ Failed to parse the value into the type that the 'PMapKey'
  -- indicates. The 'Text' is the key, and the 'String' is the error
  -- message.
  deriving (Show,Eq,Ord)

instance Exception PMapLookupException
