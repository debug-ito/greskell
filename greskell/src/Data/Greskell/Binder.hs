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
import qualified Data.Text.Lazy as TL

import Data.Greskell.Greskell (unsafeGreskellLazy, Greskell)

-- | A Monad that manages binding variables to values.
newtype Binder a = Binder { unBinder :: State (PlaceHolderIndex, [Value]) a }
                   deriving (Functor, Applicative, Monad)

-- | Binding between Gremlin variable names and JSON values.
type Binding = Object

-- | Create a new Gremlin variable bound to the given value.
newBind :: ToJSON v
        => v -- ^ bound value
        -> Binder (Greskell v) -- ^ variable
newBind val = Binder $ do
  (next_index, values) <- State.get
  State.put (succ next_index, values ++ [toJSON val])
  return $ unsafePlaceHolder next_index

-- | Execute the given 'Binder' monad to obtain 'Binding'.
runBinder :: Binder a -> (a, Binding)
runBinder binder = (ret, binding)
  where
    (ret, (_, values)) = State.runState (unBinder binder) (0, [])
    binding = HM.fromList $ zip (map toPlaceHolderVariableStrict [0 ..]) $ values
    toPlaceHolderVariableStrict = TL.toStrict . toPlaceHolderVariable

-- | __This type is only for internal use.__
type PlaceHolderIndex = Int

-- | __This function is only for internal use.__
--
-- Unsafely create a placeholder variable of arbitrary type with the
-- given index.
unsafePlaceHolder :: PlaceHolderIndex -> Greskell a
unsafePlaceHolder = unsafeGreskellLazy . toPlaceHolderVariable

-- | __This function is only for internal use.__
--
-- Create placeholder variable string from the index.
toPlaceHolderVariable :: PlaceHolderIndex -> TL.Text
toPlaceHolderVariable i =  TL.pack ("__v" ++ show i)
