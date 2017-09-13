{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
-- |
-- Module: Data.Greskell.Binder
-- Description: Binder monad to make binding between Gremlin variables and JSON values.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.Binder
       ( -- * Types
         Binder,
         Binding,
         -- * Actions
         newBind,
         -- * Runners
         runBinder
       ) where

import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State
import Data.Aeson (Value, ToJSON(toJSON), Object)
import Data.Monoid ((<>))
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, pack)

import Data.Greskell.Greskell
  ( Greskell, placeHolder, toPlaceHolderVariable,
    PlaceHolderIndex
  )

-- | A Monad that manages a 'Binding' in its context.
newtype Binder a = Binder { unBinder :: State (PlaceHolderIndex, [Value]) a }
                   deriving (Functor, Applicative, Monad)

-- | Binding between Gremlin variable names and JSON values.
type Binding = Object

-- | Create a new Gremlin variable bound to the given value.
newBind :: ToJSON v
        => v -- ^ bound value
        -> Binder Greskell -- ^ variable
newBind val = Binder $ do
  (next_index, values) <- State.get
  State.put (succ next_index, values ++ [toJSON val])
  return $ placeHolder next_index

-- | Execute the given 'Binder' monad to obtain 'Binding'.
runBinder :: Binder a -> (a, Binding)
runBinder binder = (ret, binding)
  where
    (ret, (_, values)) = State.runState (unBinder binder) (0, [])
    binding = HM.fromList $ zip (map toPlaceHolderVariable [0 ..]) $ values

-- seqGremlin :: [Binder Text] -> Binder Text
-- seqGremlin = fmap seqSentences . sequence
--   where
--     seqSentences = T.intercalate "; "
-- 
