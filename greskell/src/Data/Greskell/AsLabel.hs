{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}
-- |
-- Module: Data.Greskell.AsLabel
-- Description: Label string used in .as step
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- @since 0.2.2.0
module Data.Greskell.AsLabel
       ( -- * AsLabel
         AsLabel(..),
         SelectedMap,
         unsafeCastAsLabel,
         -- * Re-exports
         lookup,
         lookupM,
         lookupAs,
         lookupAsM,
         PMapLookupException(..),
         -- * LabeledP
         LabeledP
       ) where

import Prelude hiding (lookup)

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow(..))
import Data.Foldable (Foldable)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Greskell.GraphSON (GValue, GraphSONTyped(..), FromGraphSON(..), parseEither)
import Data.Greskell.Greskell (ToGreskell(..))
import qualified Data.Greskell.Greskell as Greskell
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Traversable (Traversable)

import Data.Greskell.Gremlin (PLike(..), P)
import Data.Greskell.PMap
  ( PMap, PMapKey(..), Single,
    lookup, lookupM, lookupAs, lookupAsM, PMapLookupException(..)
  )

-- | 'AsLabel' @a@ represents a label string used in @.as@ step
-- pointing to the data of type @a@.
newtype AsLabel a = AsLabel { unAsLabel :: Text }
               deriving (Show,Eq,Ord,Hashable)

-- | @since 1.0.0.0
instance IsString (AsLabel a) where
  fromString = AsLabel . fromString

-- | Returns the 'Text' as a Gremlin string.
instance ToGreskell (AsLabel a) where
  type GreskellReturn (AsLabel a) = Text
  toGreskell (AsLabel t) = Greskell.string t

-- | Unsafely convert the phantom type.
instance Functor AsLabel where
  fmap _ (AsLabel t) = AsLabel t

-- | @since 1.0.0.0
instance PMapKey (AsLabel a) where
  type PMapValue (AsLabel a) = a
  keyText = unAsLabel

-- | A map keyed with 'AsLabel'. Obtained from @.select@ step, for
-- example.
type SelectedMap = PMap Single

-- | Unsafely cast the phantom type of the 'AsLabel'.
--
-- @since 1.1.0.0
unsafeCastAsLabel :: AsLabel a -> AsLabel b
unsafeCastAsLabel = AsLabel . unAsLabel


-- | 'LabeledP' is just like 'P', a Haskell representation of
-- TinkerPop's @P@ class. Unlike 'P', however, 'LabeledP' keeps a
-- label ('AsLabel') inside. It is used in @.where@ and @.match@
-- steps, for example.
--
-- @since 1.2.0.0
data LabeledP a

-- Design note: neo4j-gremlin has `LabelP` class, which has nothing to
-- do with the 'LabeledP' type above.


-- | You can construct a 'LabeledP' from 'AsLabel'.
instance PLike (LabeledP a) where
  type PParameter (LabeledP a) = AsLabel a
