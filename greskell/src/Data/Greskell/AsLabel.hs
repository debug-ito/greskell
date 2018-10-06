{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
-- |
-- Module: Data.Greskell.AsLabel
-- Description: Label string used in .as step
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.AsLabel
       ( AsLabel(..),
         SelectedMap,
         lookupAs,
         lookupAsM,
         AsLookupException(..)
       ) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow(..))
import Data.HashMap.Strict (HashMap)
import Data.Greskell.GraphSON (GValue, GraphSONTyped(..), FromGraphSON(..))
import Data.Greskell.Greskell (ToGreskell(..))
import qualified Data.Greskell.Greskell as Greskell
import Data.Text (Text)

-- | 'AsLabel' @a@ represents a label string used in @.as@ step
-- pointing to the data of type @a@.
newtype AsLabel a = AsLabel Text
               deriving (Show,Eq,Ord)

-- | Returns the 'Text' as a Gremlin string.
instance ToGreskell (AsLabel a) where
  type GreskellReturn (AsLabel a) = Text
  toGreskell (AsLabel t) = Greskell.string t

-- | A value-heterogeneous map keyed with 'AsLabel'. Obtained from
-- @.select@ step, for example.
newtype SelectedMap = SelectedMap (HashMap Text GValue)
                    deriving (Show,Eq)

instance GraphSONTyped SelectedMap where
  gsonTypeFor _ = "g:Map"

instance FromGraphSON SelectedMap where
  parseGraphSON gv = fmap SelectedMap $ parseGraphSON gv

data AsLookupException = AsLookupException -- TODO: write proper options
                       deriving (Show,Eq,Ord)

instance Exception AsLookupException

lookupAs :: AsLabel a -> SelectedMap -> Either AsLookupException a
lookupAs = undefined -- TODO

lookupAsM :: MonadThrow m => AsLabel a -> SelectedMap -> m a
lookupAsM l m = either throwM return $ lookupAs l m
