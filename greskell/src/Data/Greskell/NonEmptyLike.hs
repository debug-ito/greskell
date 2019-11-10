-- |
-- Module: Data.Greskell.NonEmptyLike
-- Description: Class of non-empty containers
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.NonEmptyLike
  ( NonEmptyLike(..)
  ) where

import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup, (<>))
import qualified Data.Semigroup as S

-- | Non-empty containers. Its cardinality is one or more.
class F.Foldable t => NonEmptyLike t where
  -- | Make a container with a single value.
  singleton :: a -> t a
  -- | Append two containers.
  append :: t a -> t a -> t a
  -- | Convert the container to 'NonEmpty' list.
  toNonEmpty :: t a -> NonEmpty a

-- | 'append' is '(<>)' from 'Semigroup'.
instance NonEmptyLike NonEmpty where
  singleton a = a :| []
  append = (<>)
  toNonEmpty = id
  
-- | 'append' is '(<>)' from 'Semigroup'.
instance NonEmptyLike S.First where
  singleton = S.First
  append = (<>)
  toNonEmpty (S.First a) = singleton a

-- | 'append' is '(<>)' from 'Semigroup'.
instance NonEmptyLike S.Last where
  singleton = S.Last
  append = (<>)
  toNonEmpty (S.Last a) = singleton a

