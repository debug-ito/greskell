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
         GreskellLike(..),
         -- * Constructors
         raw,
         string,
         funCall,
         methodCall,
         -- * Conversions
         runGreskell,
         runGreskell',
         -- * Placeholders
         PlaceHolderIndex,
         placeHolder,
         toPlaceHolderVariable
       ) where

import Data.Monoid (Monoid(..), (<>))
import Data.String (IsString(..))
import Data.List (intersperse)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL

-- | Gremlin script data.
newtype Greskell = Greskell { unGreskell :: TL.Text }
                deriving (Show,Eq,Ord,Monoid)

-- | Same as 'string' except for the input type.
instance IsString Greskell where
  fromString = Greskell . TL.pack . escapeDQuotes

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

-- | Something that is isomorphic to 'Greskell'.
class GreskellLike g where
  unsafeFromGreskell :: Greskell -> g
  toGreskell :: g -> Greskell

instance GreskellLike Greskell where
  unsafeFromGreskell = id
  toGreskell = id

-- | Create a raw Gremlin script. It is printed as-is.
raw :: Text -> Greskell
raw = Greskell . TL.fromStrict

-- | Create a string literal in Gremlin script. The content is
-- automatically escaped.
string :: Text -> Greskell
string = fromString . unpack

type PlaceHolderIndex = Int

-- | Create a placeholder variable with the given index.
placeHolder :: PlaceHolderIndex -> Greskell
placeHolder = Greskell . TL.fromStrict . toPlaceHolderVariable

-- | Create placeholder variable string from the index.
toPlaceHolderVariable :: PlaceHolderIndex -> Text
toPlaceHolderVariable i =  pack ("__v" ++ show i)

-- | Create a readable Gremlin script from 'Greskell'.
runGreskell :: Greskell -> Text
runGreskell = TL.toStrict . unGreskell

-- | Polymorphic version of 'runGreskell'.
runGreskell' :: GreskellLike g => g -> Text
runGreskell' = runGreskell . toGreskell

-- | Create a 'Greskell' that calls the given function with the given
-- arguments.
funCall :: Text -- ^ function name
        -> [Greskell] -- ^ arguments
        -> Greskell
funCall fun_name args = raw fun_name <> raw "(" <> args_g <> raw ")"
  where
    args_g = mconcat $ intersperse (raw ",") args

-- | Create a 'Greskell' that calls the given (object or class) method
-- call with the given arguments.
methodCall :: Text -- ^ method name
           -> [Greskell] -- ^ arguments
           -> Greskell
methodCall method_name args = raw "." <> funCall method_name args
