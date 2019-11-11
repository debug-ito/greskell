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
    emptyPMap,
    insertPMap,
    -- ** Single lookup
    lookup,
    lookupM,
    lookupAs,
    lookupAsM,
    -- ** List lookup
    lookupList,
    lookupListAs,
    lookupListAsM,
    -- ** Low-level lookup
    lookupRaw,
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
import Data.Greskell.GraphSON (GValue, GraphSONTyped(..), FromGraphSON(..), parseEither)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (listToMaybe)
import Data.Semigroup (Semigroup, (<>))
import qualified Data.Semigroup as S
import Data.Traversable (Traversable(traverse))
import Data.Text (Text)

import Data.Greskell.NonEmptyLike (NonEmptyLike)
import qualified Data.Greskell.NonEmptyLike as NEL

-- | A property map, which has text keys and @v@ values. @c@ specifies
-- the 'PMapCardinality' of each item.
newtype PMap c v = PMap (HM.HashMap Text (c v))
                 deriving (Show,Eq,Functor,Foldable,Traversable)

instance GraphSONTyped (PMap c v) where
  gsonTypeFor _ = "g:Map"

instance FromGraphSON (c v) => FromGraphSON (PMap c v) where
  parseGraphSON gv = fmap PMap $ parseGraphSON gv

-- | An empty 'PMap'.
emptyPMap :: PMap c v
emptyPMap = PMap HM.empty

-- | Insert a key-value pair to 'PMap'. It depends on the 'NEL.append'
-- method of the 'NonEmptyLike' type @c@ how it behaves when it
-- already has items for that key.
insertPMap :: NonEmptyLike c => Text -> v -> PMap c v -> PMap c v
insertPMap k v (PMap hm) = PMap $ HM.insertWith NEL.append k (NEL.singleton v) hm

-- | Lookup all items for the key. If there is no item for the key, it
-- returns an empty list.
lookupRaw :: NonEmptyLike c => Text -> PMap c v -> [v]
lookupRaw k (PMap hm) = maybe [] (F.toList . NEL.toNonEmpty) $ HM.lookup k hm

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
lookupList k pm = lookupRaw (keyText k) pm

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

-- | The single cardinality for 'PMap'. 'insert' method replaces the
-- old value.
type Single = S.First

-- | The "one or more" cardinality for 'PMap'. 'insert' method appends
-- the new value at the tail.
newtype Multi a = Multi (NonEmpty a)
              deriving (Show,Eq,Ord,Functor,Semigroup,Foldable,Traversable,NonEmptyLike)

-- | A typed key for 'PMap'.
class PMapKey k where
  -- | Type of the value associate with the key.
  type PMapValue k :: *

  -- | 'Text' representation of the key.
  keyText :: k -> Text

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
