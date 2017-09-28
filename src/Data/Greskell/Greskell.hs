{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
-- |
-- Module: Data.Greskell.Greskell
-- Description: Low-level Gremlin script data type
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.Greskell
       ( -- * Type
         Greskell,
         -- * Constructors
         string,
         true,
         false,
         -- * Conversions
         runGreskell,
         runGreskellLazy,
         -- * Unsafe operations
         unsafeGreskell,
         unsafePlaceHolder,
         PlaceHolderIndex,
         toPlaceHolderVariable,
         unsafeFunCall,
         unsafeMethodCall
       ) where

import Data.Monoid (Monoid(..), (<>))
import Data.String (IsString(..))
import Data.List (intersperse)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL

-- | Gremlin expression of type @a@.
--
-- 'Greskell' is essentially just a piece of Gremlin script with a
-- phantom type. The type @a@ represents the type of data the script
-- evaluates to.
newtype Greskell a = Greskell { unGreskell :: TL.Text }
                   deriving (Show,Eq,Ord)

-- | Same as 'string' except for the input type.
instance IsString a => IsString (Greskell a) where
  fromString = Greskell . TL.pack . escapeDQuotes

-- | Unsafely convert the phantom type.
instance Functor Greskell where
  fmap _ = Greskell . unGreskell

escapeDQuotes :: String -> String
escapeDQuotes orig = ('"' : (esc =<< orig)) ++ "\""
  where
    esc c = case c of
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      '\\' -> "\\\\"
      '"'  -> "\\\""
      x    -> [x]
      -- do we have to espace other characters?


-- | Unsafely create a 'Greskell' of arbitrary type. The given Gremlin
-- script is printed as-is.
unsafeGreskell :: Text -- ^ Gremlin script
               -> Greskell a
unsafeGreskell = Greskell . TL.fromStrict

-- | Create a String literal in Gremlin script. The content is
-- automatically escaped.
string :: Text -> Greskell Text
string = fromString . unpack

-- | Boolean @true@ literal.
true :: Greskell Bool
true = unsafeGreskell "true"

-- | Boolean @false@ literal.
false :: Greskell Bool
false = unsafeGreskell "false"

type PlaceHolderIndex = Int

-- | Unsafely create a placeholder variable of arbitrary type with the
-- given index.
unsafePlaceHolder :: PlaceHolderIndex -> Greskell a
unsafePlaceHolder = Greskell . TL.fromStrict . toPlaceHolderVariable

-- | Create placeholder variable string from the index.
toPlaceHolderVariable :: PlaceHolderIndex -> Text
toPlaceHolderVariable i =  pack ("__v" ++ show i)

-- | Create a readable Gremlin script from 'Greskell'.
runGreskell :: Greskell -> Text
runGreskell = TL.toStrict . unGreskell

-- | Same as 'runGreskell' except that this returns lazy 'TL.Text'.
runGreskellLazy :: Greskell -> TL.Text
runGreskellLazy = unGreskell

unsafeFunCallText :: Text -> [Text] -> Text
unsafeFunCallText fun_name args = fun_name <> "(" <> args_g <> ")"
  where
    args_g = mconcat $ intersperse "," args

-- | Unsafely create a 'Greskell' that calls the given function with
-- the given arguments.
unsafeFunCall :: Text -- ^ function name
              -> [Text] -- ^ arguments
              -> Greskell a
unsafeFunCall fun_name args = unsafeGreskell $ unsafeFunCallText fun_name args

-- | Unsafely create a 'Greskell' that calls the given (object or
-- class) method with the given arguments.
unsafeMethodCall :: Text -- ^ method name
                 -> [Text] -- ^ arguments
                 -> Greskell a
unsafeMethodCall method_name args =
  unsafeGreskell $ ("." <>) $ unsafeFunCallText method_name args
