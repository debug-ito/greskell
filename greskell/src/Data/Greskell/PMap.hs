{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveTraversable #-}
-- |
-- Module: Data.Greskell.PMap
-- Description: Property map, a map with Text keys and cardinality options
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.PMap
  ( PMap,
    empty,
    insert,
    lookupText,
    Cardinality(..),
    Single,
    Multi
  ) where

import qualified Data.Foldable as F
import Data.Functor.Identity (Identity)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Semigroup (Semigroup, (<>))
import qualified Data.Semigroup as S
import Data.Text (Text)

-- | A property map, which has text keys and @v@ values. @c@ specifies
-- the 'Cardinality' of each item.
newtype PMap c v = PMap (HM.HashMap Text (c v))
                 deriving (Show,Eq,Functor,Foldable,Traversable)

-- | An empty 'PMap'.
empty :: PMap c v
empty = PMap HM.empty

-- | Insert a key-value pair to 'PMap'. It depends on the 'append'
-- method of the 'Cardinality' @c@ how it behaves when it already has
-- items for that key.
insert :: Cardinality c => Text -> v -> PMap c v -> PMap c v
insert k v (PMap hm) = PMap $ HM.insertWith append k (singleton v) hm

-- | Lookup all elements for the key. If there is no element for the
-- key, it returns an empty list.
lookupText :: Cardinality c => Text -> PMap c v -> [v]
lookupText k (PMap hm) = maybe [] toList $ HM.lookup k hm

  
-- | Cardinality used for 'PMap' type. It's basically a non-empty
-- container.
class F.Foldable c => Cardinality c where
  -- | Make a container with a single value.
  singleton :: a -> c a
  
  -- | Append two containers.
  append :: c a -> c a -> c a

  -- | Convert the container to list. By default, it's 'F.toList' from
  -- 'F.Foldable'.
  toList :: c a -> [a]
  toList = F.toList

-- | 'append' is '(<>)' from 'Semigroup'.
instance Cardinality S.First where
  singleton = S.First
  append = (<>)

-- | 'append' is '(<>)' from 'Semigroup'.
instance Cardinality S.Last where
  singleton = S.Last
  append = (<>)

-- | 'append' is '(<>)' from 'Semigroup'.
instance Cardinality NonEmpty where
  singleton a = a :| []
  append = (<>)

-- | The single cardinality for 'PMap'. 'insert' method replaces the
-- old value.
type Single = S.First

-- | The "one or more" cardinality for 'PMap'. 'insert' method appends
-- the new value at the tail.
newtype Multi a = Multi (NonEmpty a)
              deriving (Show,Eq,Ord,Functor,Semigroup,Foldable,Traversable,Cardinality)
