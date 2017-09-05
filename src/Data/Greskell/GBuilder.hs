{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
-- |
-- Module: Data.Greskell.GBuilder
-- Description: GBuilder monad for building Gremlin
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.GBuilder
       ( -- * Types
         GBuilder,
         GScript,
         -- * Actions
         newBind,
         -- * Runners
         runGBuilder
       ) where

import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State
import Data.Aeson (Value, ToJSON(toJSON))
import Data.Monoid ((<>))
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, pack)

import Data.Greskell.GScript
  ( GScript, gPlaceHolder, toPlaceHolderVariable,
    PlaceHolderIndex, getGScript
  )

-- | A Monad that stores bound variables and values.
newtype GBuilder a = GBuilder { unGBuilder :: State (PlaceHolderIndex, [Value]) a }
                   deriving (Functor, Applicative, Monad)

-- | Create a new Gremlin variable bound to the given value.
newBind :: ToJSON v
        => v -- ^ bound value
        -> GBuilder GScript -- ^ variable
newBind val = GBuilder $ do
  (next_index, values) <- State.get
  State.put (succ next_index, values ++ [toJSON val])
  return $ gPlaceHolder next_index

runGBuilder :: GBuilder a -> (a, HM.HashMap Text Value)
runGBuilder gbuilder = (ret, binding)
  where
    (ret, (_, values)) = State.runState (unGBuilder gbuilder) (0, [])
    binding = HM.fromList $ zip (map toPlaceHolderVariable [0 ..]) $ values

-- seqGremlin :: [GBuilder Text] -> GBuilder Text
-- seqGremlin = fmap seqSentences . sequence
--   where
--     seqSentences = T.intercalate "; "
-- 
