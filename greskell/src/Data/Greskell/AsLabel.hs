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
import qualified Data.HashMap.Strict as HM
import Data.Greskell.GraphSON (GValue, GraphSONTyped(..), FromGraphSON(..), parseEither)
import Data.Greskell.Greskell (ToGreskell(..))
import qualified Data.Greskell.Greskell as Greskell
import Data.Text (Text)

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

-- | A value-heterogeneous map keyed with 'AsLabel'. Obtained from
-- @.select@ step, for example.
newtype SelectedMap = SelectedMap (HashMap Text GValue)
                    deriving (Show,Eq)

instance GraphSONTyped SelectedMap where
  gsonTypeFor _ = "g:Map"

instance FromGraphSON SelectedMap where
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

lookupAs :: FromGraphSON a => AsLabel a -> SelectedMap -> Either AsLookupException a
lookupAs (AsLabel l) (SelectedMap m) =
  case HM.lookup l m of
   Nothing -> Left NoSuchAsLabel
   Just gv -> either (Left . ParseError) Right $ parseEither gv

lookupAsM :: (MonadThrow m, FromGraphSON a) => AsLabel a -> SelectedMap -> m a
lookupAsM l m = either throwM return $ lookupAs l m
