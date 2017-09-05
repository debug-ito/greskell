{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
-- |
-- Module: Data.Greskell.GScript
-- Description: Low-level Gremlin script data type
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.GScript
       ( -- * Type
         GScript,
         -- * Constructors
         gRaw,
         gLiteral,
         gFunCall,
         gMethodCall,
         -- * Conversions
         getGScript,
         -- * Placeholders
         PlaceHolderIndex,
         gPlaceHolder,
         toPlaceHolderVariable
       ) where

import Data.Monoid (Monoid(..), (<>))
import Data.String (IsString(..))
import Data.List (intersperse)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL

-- | Gremlin script data.
newtype GScript = GScript { unGScript :: TL.Text }
                deriving (Show,Eq,Ord,Monoid)

-- | Same as 'gLiteral' except for the input type.
instance IsString GScript where
  fromString = GScript . TL.pack . escapeDQuotes

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

-- | Create a raw Gremlin script. It is printed as-is.
gRaw :: Text -> GScript
gRaw = GScript . TL.fromStrict

-- | Create a string literal in Gremlin script. The content is
-- automatically escaped.
gLiteral :: Text -> GScript
gLiteral = fromString . unpack

type PlaceHolderIndex = Int

-- | Create a placeholder variable with the given index.
gPlaceHolder :: PlaceHolderIndex -> GScript
gPlaceHolder = GScript . TL.fromStrict . toPlaceHolderVariable

-- | Create placeholder variable string from the index.
toPlaceHolderVariable :: PlaceHolderIndex -> Text
toPlaceHolderVariable i =  pack ("__v" ++ show i)

-- | Create a readable Gremlin script from 'GScript'.
getGScript :: GScript -> Text
getGScript = TL.toStrict . unGScript

-- | Create a 'GScript' that calls the given function with the given
-- arguments.
gFunCall :: Text -- ^ function name
         -> [GScript] -- ^ arguments
         -> GScript
gFunCall fun_name args = gRaw fun_name <> gRaw "(" <> args_g <> gRaw ")"
  where
    args_g = mconcat $ intersperse (gRaw ", ") args

-- | Create a 'GScript' that calls the given (object or class) method
-- call with the given arguments.
gMethodCall :: Text -- ^ method name
            -> [GScript] -- ^ arguments
            -> GScript
gMethodCall method_name args = gRaw "." <> gFunCall method_name args


