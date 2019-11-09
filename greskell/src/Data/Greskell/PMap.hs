{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveTraversable, TypeFamilies #-}
-- |
-- Module: Data.Greskell.PMap
-- Description: Property map, a map with Text keys and cardinality options
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.PMap
  ( -- * PMap
    PMap,
    empty,
    insert,
    -- ** Single lookup
    lookup,
    lookupM,
    lookupAs,
    lookupAsM,
    -- ** List lookup
    lookupList,
    -- ** Low-level lookup
    lookupRaw,
    -- * Cardinality
    PMapCardinality(..),
    Single,
    Multi,
    -- * PMapKey
    PMapKey(..),
    -- * Errors
    AsLookupException(..)
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
import Data.Text (Text)

-- | A property map, which has text keys and @v@ values. @c@ specifies
-- the 'PMapCardinality' of each item.
newtype PMap c v = PMap (HM.HashMap Text (c v))
                 deriving (Show,Eq,Functor,Foldable,Traversable)

-- | An empty 'PMap'.
empty :: PMap c v
empty = PMap HM.empty

-- | Insert a key-value pair to 'PMap'. It depends on the 'pmAppend'
-- method of the 'PMapCardinality' @c@ how it behaves when it already
-- has items for that key.
insert :: PMapCardinality c => Text -> v -> PMap c v -> PMap c v
insert k v (PMap hm) = PMap $ HM.insertWith pmAppend k (pmSingleton v) hm

-- | Lookup all items for the key. If there is no item for the key, it
-- returns an empty list.
lookupRaw :: PMapCardinality c => Text -> PMap c v -> [v]
lookupRaw k (PMap hm) = maybe [] pmToList $ HM.lookup k hm

-- | Lookup the first value for the key from 'PMap'.
lookup :: (PMapKey k, PMapCardinality c) => k -> PMap c v -> Maybe v
lookup k pm = listToMaybe $ lookupList k pm

-- | 'MonadThrow' version of 'lookup'. If there is no value for the
-- key, it throws 'NoSuchAsLabel'.
lookupM :: (PMapKey k, PMapCardinality c, MonadThrow m) => k -> PMap c v -> m v
lookupM k pm = maybe (throwM NoSuchAsLabel) return $ lookup k pm

-- | Lookup the value and parse it into @a@.
lookupAs :: (PMapKey k, PMapCardinality c, PMapValue k ~ a, FromGraphSON a)
         => k -> PMap c GValue -> Either AsLookupException a
lookupAs k pm =
  case lookup k pm of
    Nothing -> Left NoSuchAsLabel
    Just gv -> either (Left . ParseError) Right $ parseEither gv

-- | 'MonadThrow' version of 'lookupAs'.
lookupAsM :: (PMapKey k, PMapCardinality c, PMapValue k ~ a, FromGraphSON a, MonadThrow m)
          => k -> PMap c GValue -> m a
lookupAsM k pm = either throwM return $ lookupAs k pm

-- | Lookup all items for the key. If there is no item for the key, it
-- returns an empty list.
lookupList :: (PMapKey k, PMapCardinality c) => k -> PMap c v -> [v]
lookupList k pm = lookupRaw (keyText k) pm

-- | Cardinality used for 'PMap' type. It's basically a non-empty
-- container.
class F.Foldable c => PMapCardinality c where
  -- | Make a container with a single value.
  pmSingleton :: a -> c a
  
  -- | Append two containers.
  pmAppend :: c a -> c a -> c a

  -- | Convert the container to list. By default, it's 'F.toList' from
  -- 'F.Foldable'.
  pmToList :: c a -> [a]
  pmToList = F.toList

-- | 'pmAppend' is '(<>)' from 'Semigroup'.
instance PMapCardinality S.First where
  pmSingleton = S.First
  pmAppend = (<>)

-- | 'pmAppend' is '(<>)' from 'Semigroup'.
instance PMapCardinality S.Last where
  pmSingleton = S.Last
  pmAppend = (<>)

-- | 'pmAppend' is '(<>)' from 'Semigroup'.
instance PMapCardinality NonEmpty where
  pmSingleton a = a :| []
  pmAppend = (<>)

-- | The single cardinality for 'PMap'. 'insert' method replaces the
-- old value.
type Single = S.First

-- | The "one or more" cardinality for 'PMap'. 'insert' method appends
-- the new value at the tail.
newtype Multi a = Multi (NonEmpty a)
              deriving (Show,Eq,Ord,Functor,Semigroup,Foldable,Traversable,PMapCardinality)

-- | A typed key for 'PMap'.
class PMapKey k where
  -- | Type of the value associate with the key.
  type PMapValue k :: *

  -- | 'Text' representation of the key.
  keyText :: k -> Text

-- | An 'Exception' raised by 'lookupM' and 'lookupAsM'.
data AsLookupException = NoSuchAsLabel
                         -- ^ The 'SelectedMap' does not have the
                         -- given 'AsLabel' as the key.
                       | ParseError String
                         -- ^ Failed to parse the value into the type
                         -- that the 'AsLabel' indicates. The 'String'
                         -- is the error message.
                       deriving (Show,Eq,Ord)

instance Exception AsLookupException
