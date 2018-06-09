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
         single,
         number,
         value,
         valueInt,
         gvalue,
         gvalueInt,
         -- * Unsafe constructors
         unsafeGreskell,
         unsafeGreskellLazy,
         unsafeFunCall,
         unsafeMethodCall
       ) where

import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import qualified Data.HashMap.Lazy as HM
import Data.Monoid (Monoid(..))
import Data.Ratio (numerator, denominator, Rational)
import Data.Scientific (Scientific, coefficient, base10Exponent)
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import Data.List (intersperse)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL

import Data.Greskell.GraphSON (GValue, GValueBody(..), unwrapAll, nonTypedGValue)

-- $
-- >>> :set -XOverloadedStrings

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
  fromRational rat = Greskell $ scriptOf numerator <> ".0/" <> scriptOf denominator
    where
      scriptOf accessor = TL.pack $ show $ accessor rat

-- | Semigroup operator '(<>)' on 'Greskell' assumes @String@
-- concatenation on Gremlin.
instance IsString a => Semigroup (Greskell a) where
  (<>) = biOp "+"

-- | Monoidal operations on 'Greskell' assumes @String@ operations in
-- Gremlin. 'mempty' is the empty String, and 'mappend' is String
-- concatenation.
instance IsString a => Monoid (Greskell a) where
  mempty = fromString ""
  mappend = (<>)

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
      '$'  -> "\\$"
      x    -> [x]
      -- do we have to espace other characters?


-- | Unsafely create a 'Greskell' of arbitrary type. The given Gremlin
-- script is printed as-is.
--
-- >>> toGremlin $ unsafeGreskell "x + 100"
-- "x + 100"
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
-- to create integer literals. Use 'fromRational' or 'number' to
-- create floating-point data literals.

-- | Create a String literal in Gremlin script. The content is
-- automatically escaped.
--
-- >>> toGremlin $ string "foo bar"
-- "\"foo bar\""
-- >>> toGremlin $ string "escape newline\n escape dollar $"
-- "\"escape newline\\n escape dollar \\$\""
string :: Text -> Greskell Text
string = fromString . unpack

-- | Boolean @true@ literal.
--
-- >>> toGremlin true
-- "true"
true :: Greskell Bool
true = unsafeGreskell "true"

-- | Boolean @false@ literal.
--
-- >>> toGremlin false
-- "false"
false :: Greskell Bool
false = unsafeGreskell "false"

-- | List literal.
--
-- >>> toGremlin $ list ([100, 200, 300] :: [Greskell Int])
-- "[100,200,300]"
list :: [Greskell a] -> Greskell [a]
list gs = unsafeGreskellLazy $ ("[" <> TL.intercalate "," gs_txt <> "]")
  where
    gs_txt = map toGremlinLazy gs

-- | Make a list with a single object. Useful to prevent the Gremlin
-- Server from automatically iterating the result object.
--
-- >>> toGremlin $ single ("hoge" :: Greskell String)
-- "[\"hoge\"]"
single :: Greskell a -> Greskell [a]
single g = list [g]

-- | Arbitrary precision number literal, like \"123e8\".
--
-- >>> toGremlin $ number 123e8
-- "1.23e10"
number :: Scientific -> Greskell Scientific
number = unsafeGreskell . pack . show

-- | Aeson 'Value' literal.
--
-- >>> toGremlin $ value Aeson.Null
-- "null"
-- >>> toGremlin $ value $ Aeson.toJSON $ ([10, 20, 30] :: [Int])
-- "[10.0,20.0,30.0]"
-- >>> toGremlin $ value $ Aeson.Object mempty
-- "[:]"
--
-- Note that 'Aeson.Number' does not distinguish integers from
-- floating-point numbers, so 'value' function may format an integer
-- as a floating-point number. To ensure formatting as integers, use
-- 'valueInt'.
value :: Value -> Greskell Value
value Aeson.Null = unsafeGreskellLazy "null"
value (Aeson.Bool b) = unsafeToValue (if b then true else false)
value (Aeson.Number sci) = unsafeToValue $ number sci
value (Aeson.String s) = unsafeToValue $ string s
value (Aeson.Array v) = unsafeToValue $ list $ map value $ toList v
value (Aeson.Object obj)
  | HM.null obj = unsafeGreskellLazy "[:]"
  | otherwise = unsafeGreskellLazy $ toGroovyMap $ HM.toList obj
  where
    toGroovyMap pairs = "[" <> TL.intercalate "," (map toPairText pairs) <> "]"
    toPairText (key, val) = (toGremlinLazy $ string key) <> ":" <> (toGremlinLazy $ value val)

-- | Integer literal as 'Value' type.
--
-- >>> toGremlin $ valueInt (100 :: Int)
-- "100"
valueInt :: Integral a => a -> Greskell Value
valueInt n = fmap toValue $ fromIntegral n
  where
    toValue :: Integer -> Value
    toValue = const Aeson.Null

-- | 'Value' literal as 'GValue' type.
gvalue :: Value -> Greskell GValue
gvalue = fmap phantomToGValue . value
  where
    phantomToGValue _ = nonTypedGValue $ GNull

-- | Integer literal as 'GValue' type.
--
-- >>> toGremlin $ gvalueInt (256 :: Int)
-- "256"
gvalueInt :: Integral a => a -> Greskell GValue
gvalueInt n = fmap toGValue $ fromIntegral n
  where
    toGValue :: Integer -> GValue
    toGValue = const $ nonTypedGValue $ GNull

unsafeToValue :: Greskell a -> Greskell Value
unsafeToValue = fmap (const Aeson.Null)

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
--
-- >>> toGremlin $ unsafeFunCall "add" ["10", "20"]
-- "add(10,20)"
unsafeFunCall :: Text -- ^ function name
              -> [Text] -- ^ arguments
              -> Greskell a -- ^ return value of the function call
unsafeFunCall fun_name args = unsafeGreskell $ unsafeFunCallText fun_name args

-- | Unsafely create a 'Greskell' that calls the given object method
-- call with the given target and arguments.
--
-- >>> toGremlin $ unsafeMethodCall ("foobar" :: Greskell String) "length" []
-- "(\"foobar\").length()"
unsafeMethodCall :: Greskell a -- ^ target object
                 -> Text -- ^ method name
                 -> [Text] -- ^ arguments
                 -> Greskell b -- ^ return value of the method call
unsafeMethodCall target name args = unsafeGreskell ("(" <> toGremlin target <> ")." <> unsafeFunCallText name args)
