{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- |
-- Module: Data.Greskell.Greskell
-- Description: Low-level Gremlin script data type
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.Greskell
       ( -- * Type
         Greskell,
         ToGreskell(..),
         -- * Conversions
         toGremlin,
         toGremlinLazy,
         -- * Literals
         --
         -- $literals
         string,
         true,
         false,
         list,
         -- * Unsafe constructors
         unsafeGreskell,
         unsafeGreskellLazy,
         unsafePlaceHolder,
         PlaceHolderIndex,
         toPlaceHolderVariable,
         unsafeFunCall,
         unsafeMethodCall
       ) where

import Data.Monoid (Monoid(..), (<>))
import Data.Ratio (numerator, denominator)
import Data.String (IsString(..))
import Data.List (intersperse)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL

-- | Gremlin expression of type @a@.
--
-- 'Greskell' is essentially just a piece of Gremlin script with a
-- phantom type. The type @a@ represents the type of data that the
-- script is supposed to evaluate to.
--
-- 'Eq' and 'Ord' instances compare Gremlin scripts, NOT the values
-- they evaluate to.
newtype Greskell a = Greskell { unGreskell :: TL.Text }
                   deriving (Show,Eq,Ord)

-- | Same as 'string' except for the input and output type.
instance IsString a => IsString (Greskell a) where
  fromString = Greskell . TL.pack . escapeDQuotes

-- | Unsafely convert the phantom type.
instance Functor Greskell where
  fmap _ = Greskell . unGreskell

-- | Integer literals and numeric operation in Gremlin
instance Num a => Num (Greskell a) where
  (+) = biOp "+"
  (-) = biOp "-"
  (*) = biOp "*"
  negate (Greskell a) = Greskell ("-" <> paren a)
  abs (Greskell a) = Greskell ("java.lang.Math.abs" <> paren a)
  signum (Greskell a) = Greskell ("java.lang.Long.signum" <> paren a)
  fromInteger val = Greskell (TL.pack $ show val)
  
-- | Floating-point number literals and numeric operation in Gremlin
instance Fractional a => Fractional (Greskell a) where
  (/) = biOp "/"
  recip (Greskell a) = Greskell ("1.0/" <> paren a)
  fromRational rat = Greskell $ paren (scriptOf numerator <> ".0/" <> scriptOf denominator)
    where
      scriptOf accessor = TL.pack $ show $ accessor rat

-- | Monoidal operations on 'Greskell' assumes @String@ operations in
-- Gremlin. 'mempty' is the empty String, and 'mappend' is String
-- concatenation.
instance IsString a => Monoid (Greskell a) where
  mempty = fromString ""
  mappend = biOp "+"

-- | Something that can convert to 'Greskell'.
class ToGreskell a where
  type GreskellReturn a
  -- ^ type of return value by Greskell.
  toGreskell :: a -> Greskell (GreskellReturn a)

-- | It's just 'id'.
instance ToGreskell (Greskell a) where
  type GreskellReturn (Greskell a) = a
  toGreskell = id


biOp :: TL.Text -> Greskell a -> Greskell a -> Greskell a
biOp operator (Greskell a) (Greskell b) = Greskell (paren a <> operator <> paren b)

paren :: TL.Text -> TL.Text
paren t = "(" <> t <> ")"

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

-- | Same as 'unsafeGreskell', but it takes lazy 'TL.Text'.
unsafeGreskellLazy :: TL.Text -- ^ Gremlin script
                   -> Greskell a
unsafeGreskellLazy = Greskell


-- $literals
--
-- Functions to create literals in Gremlin script. Use 'fromInteger'
-- to create integer literals. Use 'fromRational' to create
-- floating-point data literals.

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

-- | List literal.
list :: [Greskell a] -> Greskell [a]
list gs = unsafeGreskellLazy $ ("[" <> TL.intercalate "," gs_txt <> "]")
  where
    gs_txt = map toGremlinLazy gs


type PlaceHolderIndex = Int

-- | Unsafely create a placeholder variable of arbitrary type with the
-- given index.
unsafePlaceHolder :: PlaceHolderIndex -> Greskell a
unsafePlaceHolder = Greskell . TL.fromStrict . toPlaceHolderVariable

-- | Create placeholder variable string from the index.
toPlaceHolderVariable :: PlaceHolderIndex -> Text
toPlaceHolderVariable i =  pack ("__v" ++ show i)

-- | Create a readable Gremlin script from 'Greskell'.
toGremlin :: ToGreskell a => a -> Text
toGremlin = TL.toStrict . unGreskell . toGreskell

-- | Same as 'toGremlin' except that this returns lazy 'TL.Text'.
toGremlinLazy :: ToGreskell a => a -> TL.Text
toGremlinLazy = unGreskell . toGreskell

unsafeFunCallText :: Text -> [Text] -> Text
unsafeFunCallText fun_name args = fun_name <> "(" <> args_g <> ")"
  where
    args_g = mconcat $ intersperse "," args

-- | Unsafely create a 'Greskell' that calls the given function with
-- the given arguments.
unsafeFunCall :: Text -- ^ function name
              -> [Text] -- ^ arguments
              -> Greskell a -- ^ return value of the function call
unsafeFunCall fun_name args = unsafeGreskell $ unsafeFunCallText fun_name args

-- | Unsafely create a 'Greskell' that calls the given object method
-- call with the given target and arguments.
unsafeMethodCall :: Greskell a -- ^ target object
                 -> Text -- ^ method name
                 -> [Text] -- ^ arguments
                 -> Greskell b -- ^ return value of the method call
unsafeMethodCall target name args = unsafeGreskell ("(" <> toGremlin target <> ")." <> unsafeFunCallText name args)
