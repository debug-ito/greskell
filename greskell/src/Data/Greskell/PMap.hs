{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
-- Module: Data.Greskell.PMap
-- Description: Property map, a map with Text keys and cardinality options
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This module defines 'PMap', a map with 'Text' keys and cardinality
-- options.
--
-- @since 1.0.0.0
module Data.Greskell.PMap
    ( -- * PMap
      PMap
      -- ** Single lookup
    , lookup
    , lookupM
    , lookupAs
    , lookupAs'
    , lookupAsM
      -- ** List lookup
    , lookupList
    , lookupListAs
    , lookupListAs'
      -- ** Others
    , pMapInsert
    , pMapDelete
    , pMapLookup
    , pMapToList
    , pMapFromList
      -- * Cardinality
    , Single
    , Multi
      -- * PMapKey
    , PMapKey (..)
      -- * Errors
    , PMapLookupException (..)
    , pMapDecribeError
    , pMapToThrow
    , pMapToFail
    ) where

import           Prelude                    hiding (lookup)

import           Control.Exception          (Exception)
import           Control.Monad.Catch        (MonadCatch (..), MonadThrow (..))
import           Control.Monad.Fail         (MonadFail)
import           Data.Aeson.Types           (Parser)
import qualified Data.Foldable              as F
import           Data.Functor.Identity      (Identity)
import           Data.Greskell.AsIterator   (AsIterator (..))
import           Data.Greskell.GMap         (GMapEntry)
import           Data.Greskell.GraphSON     (FromGraphSON (..), GValue, GraphSONTyped (..),
                                             parseEither)
import qualified Data.HashMap.Strict        as HM
import           Data.Kind                  (Type)
import           Data.List.NonEmpty         (NonEmpty ((:|)))
import           Data.Maybe                 (listToMaybe)
import           Data.Monoid                (Monoid (..))
import           Data.Semigroup             (Semigroup ((<>)))
import qualified Data.Semigroup             as S
import           Data.Text                  (Text, unpack)
import           Data.Traversable           (Traversable (traverse))

import           Data.Greskell.NonEmptyLike (NonEmptyLike)
import qualified Data.Greskell.NonEmptyLike as NEL

-- | A property map, which has text keys and @v@ values. @c@ specifies
-- the cardinality of each item, and should be an instance of
-- 'NonEmptyLike'.
--
-- You can look up values from 'PMap' using key types of 'PMapKey'
-- class.
--
-- @since 1.0.0.0
newtype PMap c v
  = PMap (HM.HashMap Text (c v))
  deriving (Eq, Foldable, Functor, Show, Traversable)

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

-- | Insert a key-value pair to 'PMap'. If it already has some items
-- for that key, 'NEL.append' method of the 'NonEmptyLike' type @c@
-- determines its behavior.
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
-- key, it throws 'PMapNoSuchKey'.
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

-- | Similar to 'lookupAs', but this function converts a @null@ result
-- into 'Nothing'.
--
-- A @null@ result is either (1) the key @k@ is not found in the map,
-- or (2) the key is found, but the value is @null@.
lookupAs' :: (PMapKey k, NonEmptyLike c, PMapValue k ~ (Maybe a), FromGraphSON a)
          => k -> PMap c GValue -> Either PMapLookupException (Maybe a)
lookupAs' k pm = either fromError Right $ lookupAs k pm
  where
    fromError (PMapNoSuchKey _) = Right Nothing
    fromError e                 = Left e

-- | 'MonadThrow' version of 'lookupAs'.
lookupAsM :: (PMapKey k, NonEmptyLike c, PMapValue k ~ a, FromGraphSON a, MonadThrow m)
          => k -> PMap c GValue -> m a
lookupAsM k pm = pMapToThrow $ lookupAs k pm

-- | Lookup all items for the key. If there is no item for the key, it
-- returns an empty list.
lookupList :: (PMapKey k, NonEmptyLike c) => k -> PMap c v -> [v]
lookupList k pm = pMapLookup (keyText k) pm

-- | Look up the values and parse them into @a@.
lookupListAs :: (PMapKey k, NonEmptyLike c, PMapValue k ~ a, FromGraphSON a)
             => k -> PMap c GValue -> Either PMapLookupException (NonEmpty a)
lookupListAs k pm =
  case lookupList k pm of
    []         -> Left $ PMapNoSuchKey kt
    (x : rest) -> either (Left . PMapParseError kt) Right $ traverse parseEither (x :| rest)
  where
    kt = keyText k

-- | Similar to 'lookupListAs', but this function accepts @null@
-- results.
--
-- If the key @k@ is not found in the map, it returns an empty
-- list. If the key @k@ is found and @null@s are included in the
-- values, they are obtained as 'Nothing'.
lookupListAs' :: (PMapKey k, NonEmptyLike c, PMapValue k ~ (Maybe a), FromGraphSON a)
              => k -> PMap c GValue -> Either PMapLookupException [Maybe a]
lookupListAs' k pm = either fromError (Right . F.toList) $ lookupListAs k pm
  where
    fromError (PMapNoSuchKey _) = Right []
    fromError e                 = Left e

-- | The single cardinality for 'PMap'. 'pMapInsert' method replaces
-- the old value. '<>' on 'PMap' prefers the items from the left
-- 'PMap'. 'pMapFromList' prefers the first item for each key.
--
-- @since 1.0.0.0
type Single = S.First

-- | The \"one or more\" cardinality for 'PMap'. 'pMapInsert' method
-- prepends the new value at the head. '<>' on 'PMap' appends the
-- right items to the tail of the left items. 'pMapFromList' preserves
-- the order of the items for each key.
--
-- @since 1.0.0.0
newtype Multi a
  = Multi (NonEmpty a)
  deriving (Eq, Foldable, FromGraphSON, Functor, NonEmptyLike, Ord, Semigroup, Show, Traversable)

-- | A typed key for 'PMap'.
--
-- @since 1.0.0.0
class PMapKey k where
  -- | Type of the value associated with the key.
  type PMapValue k :: Type

  -- | 'Text' representation of the key.
  keyText :: k -> Text

-- | For simple 'Text', the 'GValue' is returned without being parsed.
instance PMapKey Text where
  type PMapValue Text = GValue
  keyText = id

-- | An 'Exception' raised when looking up values from 'PMap'.
--
-- @since 1.0.0.0
data PMapLookupException
  = PMapNoSuchKey Text
  -- ^ The 'PMap' doesn't have the given key.
  | PMapParseError Text String
  -- ^ Failed to parse the value into the type that the 'PMapKey'
  -- indicates. The 'Text' is the key, and the 'String' is the error
  -- message.
  deriving (Eq, Ord, Show)

instance Exception PMapLookupException

-- | Make a human-readable description on 'PMapLookupException'.
pMapDecribeError :: PMapLookupException -> String
pMapDecribeError (PMapNoSuchKey k)     = "Property '" ++ unpack k ++ "' does not exist."
pMapDecribeError (PMapParseError k pe) = "Parse error of property '" ++ unpack k ++ "': " ++ pe

-- | Convert the lookup result into a 'MonadThrow'. It throws
-- 'PMapLookupException'.
pMapToThrow :: MonadThrow m => Either PMapLookupException a -> m a
pMapToThrow (Left e)  = throwM e
pMapToThrow (Right a) = return a

-- | Convert the lookup result into a 'MonadFail'. It fails with the
-- description returned by 'pMapDecribeError'.
pMapToFail :: MonadFail m => Either PMapLookupException a -> m a
pMapToFail (Left e)  = fail $ pMapDecribeError e
pMapToFail (Right a) = return a
