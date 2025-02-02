{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
-- |
-- Module: Data.Greskell.Binder
-- Description: Binder monad to make binding between Gremlin variables and JSON values
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
--
module Data.Greskell.Binder
    ( -- * Types
      Binder
    , Binding
      -- * Actions
    , newBind
    , newAsLabel
      -- * Runners
    , runBinder
    ) where

import           Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State
import           Data.Aeson                (Object, ToJSON (toJSON), Value)
import qualified Data.Aeson.Key            as Key
import qualified Data.Aeson.KeyMap         as KM
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL

import           Data.Greskell.AsLabel     (AsLabel (..))
import           Data.Greskell.Greskell    (Greskell, unsafeGreskellLazy)

-- | State in the 'Binder'.
data BinderS
  = BinderS
      { varIndex     :: PlaceHolderIndex
      , varBindings  :: [Value]
      , asLabelIndex :: PlaceHolderIndex
      }
  deriving (Eq, Show)

initBinderS :: BinderS
initBinderS =
  BinderS
  { varIndex = 0,
    varBindings = [],
    asLabelIndex = 0
  }

-- | A Monad that manages binding variables and labels to values.
newtype Binder a
  = Binder { unBinder :: State BinderS a }
  deriving (Applicative, Functor, Monad)

-- | Binding between Gremlin variable names and JSON values.
type Binding = Object

-- | Create a new Gremlin variable bound to the given value.
--
-- The value @v@ is kept in the monadic context. The returned
-- 'Greskell' is a Gremlin variable pointing to the @v@. The Gremlin
-- variable is guaranteed to be unique in the current monadic context.
newBind :: ToJSON v
        => v -- ^ bound value
        -> Binder (Greskell v) -- ^ variable
newBind val = Binder $ do
  state <- State.get
  let next_index = varIndex state
      values = varBindings state
  State.put $ state { varIndex = succ next_index,
                      varBindings = values ++ [toJSON val]
                    }
  return $ unsafePlaceHolder next_index

-- | Execute the given 'Binder' monad to obtain 'Binding'.
runBinder :: Binder a -> (a, Binding)
runBinder binder = (ret, binding)
  where
    (ret, state) = State.runState (unBinder binder) initBinderS
    values = varBindings state
    binding = KM.fromList $ zip (map toPlaceHolderVariableKey [0 ..]) $ values
    toPlaceHolderVariableKey = Key.fromText . TL.toStrict . toPlaceHolderVariable

-- | __This type is only for internal use.__
type PlaceHolderIndex = Int

-- | __This function is only for internal use.__
--
-- Unsafely create a placeholder variable of arbitrary type with the
-- given index.
unsafePlaceHolder :: PlaceHolderIndex -> Greskell a
unsafePlaceHolder = unsafeGreskellLazy . wrapWithParens  . toPlaceHolderVariable
  where
    wrapWithParens v = "((" <> v <> "))"
    -- This is necessary to ensure the v is always treated as a varible name (NOT a type name) in Groovy script.
    -- See https://github.com/debug-ito/greskell/issues/18

-- | __This function is only for internal use.__
--
-- Create placeholder variable string from the index.
toPlaceHolderVariable :: PlaceHolderIndex -> TL.Text
toPlaceHolderVariable i = TL.pack ("__v" ++ show i)

-- | Create a new 'AsLabel'.
--
-- The returned 'AsLabel' is guaranteed to be unique in the current
-- monadic context.
--
-- @since 0.2.2.0
newAsLabel :: Binder (AsLabel a)
newAsLabel = Binder $ do
  state <- State.get
  let label_index = asLabelIndex state
      label = "__a" ++ show label_index
  State.put $ state { asLabelIndex = succ label_index }
  return $ AsLabel $ T.pack label
