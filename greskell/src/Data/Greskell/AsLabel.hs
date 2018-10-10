{-# LANGUAGE OverloadedStrings, TypeFamilies, GeneralizedNewtypeDeriving, DeriveTraversable #-}
-- |
-- Module: Data.Greskell.AsLabel
-- Description: Label string used in .as step
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.AsLabel
       ( AsLabel(..),
         SelectedMap,
         lookup,
         lookupM,
         lookupAs,
         lookupAsM,
         AsLookupException(..)
       ) where

import Prelude hiding (lookup)

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow(..))
import Data.Foldable (Foldable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Greskell.GraphSON (GValue, GraphSONTyped(..), FromGraphSON(..), parseEither)
import Data.Greskell.Greskell (ToGreskell(..))
import qualified Data.Greskell.Greskell as Greskell
import Data.Text (Text)
import Data.Traversable (Traversable)

-- | 'AsLabel' @a@ represents a label string used in @.as@ step
-- pointing to the data of type @a@.
newtype AsLabel a = AsLabel { unAsLabel :: Text }
               deriving (Show,Eq,Ord)

-- | Returns the 'Text' as a Gremlin string.
instance ToGreskell (AsLabel a) where
  type GreskellReturn (AsLabel a) = Text
  toGreskell (AsLabel t) = Greskell.string t

-- | Unsafely convert the phantom type.
instance Functor AsLabel where
  fmap _ (AsLabel t) = AsLabel t

-- | A map keyed with 'AsLabel'. Obtained from @.select@ step, for
-- example.
newtype SelectedMap a = SelectedMap (HashMap Text a)
                    deriving (Show,Eq,Functor,Foldable,Traversable)

instance GraphSONTyped (SelectedMap a) where
  gsonTypeFor _ = "g:Map"

instance FromGraphSON a => FromGraphSON (SelectedMap a) where
  parseGraphSON gv = fmap SelectedMap $ parseGraphSON gv

data AsLookupException = NoSuchAsLabel
                         -- ^ The 'SelectedMap' does not have the
                         -- given 'AsLabel' as the key.
                       | ParseError String
                         -- ^ Failed to parse the value into the type
                         -- that the 'AsLabel' indicates. The 'String'
                         -- is the error message.
                       deriving (Show,Eq,Ord)

instance Exception AsLookupException

-- | Get value from 'SelectedMap'.
lookup :: AsLabel a -> SelectedMap b -> Maybe b
lookup (AsLabel l) (SelectedMap m) = HM.lookup l m

-- | 'MonadThrow' version of 'lookup'. If there is no value for the
-- 'AsLabel', it throws 'NoSuchAsLabel'.
lookupM :: MonadThrow m => AsLabel a -> SelectedMap b -> m b
lookupM l m = maybe (throwM NoSuchAsLabel) return $ lookup l m

-- | Get value from 'SelectedMap' and parse the value into @a@.
lookupAs :: FromGraphSON a => AsLabel a -> SelectedMap GValue -> Either AsLookupException a
lookupAs l m =
  case lookup l m of
   Nothing -> Left NoSuchAsLabel
   Just gv -> either (Left . ParseError) Right $ parseEither gv

-- | 'MonadThrow' version of 'lookupAs'.
lookupAsM :: (MonadThrow m, FromGraphSON a) => AsLabel a -> SelectedMap GValue -> m a
lookupAsM l m = either throwM return $ lookupAs l m
