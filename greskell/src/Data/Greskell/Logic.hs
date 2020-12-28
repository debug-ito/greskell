{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module: Data.Greskell.Logic
-- Description: Logic tree data structure
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- Developer note: This module defines 'Logic', a data structure for
-- logic operation tree. There are already similar packages to this
-- module, but, none of them satisfy our requirements.
--
-- Boolean/logic tree data structures
--
-- - https://hackage.haskell.org/package/boolean-normal-forms
-- - https://hackage.haskell.org/package/logic-classes
-- - https://hackage.haskell.org/package/PropLogic
-- - https://hackage.haskell.org/package/Logic
-- - https://hackage.haskell.org/package/boolean-like
--
-- Typeclasses about boolean/logic operations
--
-- - https://hackage.haskell.org/package/Boolean
-- - https://hackage.haskell.org/package/cond
--
-- Trees that contain heterogeneous values
--
-- - http://hackage.haskell.org/package/dual-tree
-- - http://hackage.haskell.org/package/fingertree
--
-- @since 1.2.0.0 
module Data.Greskell.Logic
  ( Logic(..),
    runBool
  ) where

import Control.Applicative (Applicative(pure, (<*>)), (<$>))
import Control.Monad (Monad(return,(>>=)))
import Data.Foldable (Foldable(foldMap, toList))
import Data.Traversable (Traversable)
import Data.Monoid ((<>), All(..), Any(..))
import GHC.Generics (Generic)

-- | A general-purpose logic tree structure. Only the leaf nodes have
-- values of type @a@. The tree is lazy both in value and spine (structure).
data Logic a =
    Leaf a -- ^ Leaf node with value
  | And (Logic a) [Logic a] -- ^ \"and\" logic operator
  | Or  (Logic a) [Logic a] -- ^ \"or\" logic operator
  | Not (Logic a) -- ^ \"not\" logic operator
  deriving (Show,Eq,Ord,Generic)

instance Functor Logic where
  fmap f l =
    case l of
      Leaf a -> Leaf (f a)
      And ll rls -> And (fmap f ll) (map (fmap f) rls)
      Or ll rls -> Or (fmap f ll) (map (fmap f) rls)
      Not nl -> Not (fmap f nl)

-- | 'pure' is 'Leaf'. @fl@ '<*>' @rl@ appends the @rl@ to the leaves
-- of @fl@.
instance Applicative Logic where
  pure a = Leaf a
  fl <*> rl =
    case fl of
      Leaf f -> fmap f rl
      And lfl rfls -> And (lfl <*> rl) (map (<*> rl) rfls)
      Or lfl rfls -> Or (lfl <*> rl) (map (<*> rl) rfls)
      Not nfl -> Not (nfl <*> rl)

instance Monad Logic where
  return = pure
  l >>= f =
    case l of
      Leaf a -> f a
      And ll rls -> And (ll >>= f) (map (>>= f) rls)
      Or ll rls -> Or (ll >>= f) (map (>>= f) rls)
      Not nl -> Not (nl >>= f)

instance Foldable Logic where
  foldMap f l =
    case l of
      Leaf a -> f a
      And ll rls -> foldMap f ll <> foldMap (foldMap f) rls
      Or ll rls -> foldMap f ll <> foldMap (foldMap f) rls
      Not nl -> foldMap f nl

instance Traversable Logic where
  traverse f l =
    case l of
      Leaf a -> Leaf <$> f a
      And ll rls -> And <$> traverse f ll <*> traverse (traverse f) rls
      Or ll rls -> Or <$> traverse f ll <*> traverse (traverse f) rls
      Not nl -> Not <$> traverse f nl

-- | Run the logic tree of 'Bool' values to get the result.
runBool :: Logic Bool -> Bool
runBool l =
  case l of
    Leaf b -> b
    And ll rls -> getAll $ mconcat $ (All $ runBool ll) : map (All . runBool) rls
    Or ll rls  -> getAny $ mconcat $ (Any $ runBool ll) : map (Any . runBool) rls
    Not nl -> not $ runBool nl
