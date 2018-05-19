{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-- |
-- Module: Data.Greskell.GraphSON
-- Description: Encoding and decoding GraphSON
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.GraphSON
       ( -- * GraphSON
         GraphSON(..),
         GraphSONTyped(..),
         -- ** constructors
         nonTypedGraphSON,
         typedGraphSON,
         typedGraphSON',
         -- ** parser support
         parseTypedGraphSON,
         parseTypedGraphSON',
         -- * GValue
         GValue(..),
         GValueBody(..),
         unwrapAll,
         unwrapOne
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), object, (.=), Value(..), (.:!))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import Data.Foldable (Foldable(foldr), foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Lazy as HML
import Data.Hashable (Hashable(..))
import Data.HashSet (HashSet)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Traversable (Traversable(traverse))
import Data.Vector (Vector)
import GHC.Generics (Generic)

-- $
-- >>> :set -XOverloadedStrings

-- | Wrapper for \"typed JSON object\" introduced in GraphSON version
-- 2. See http://tinkerpop.apache.org/docs/current/dev/io/#graphson
--
-- This data type is useful for encoding/decoding GraphSON text.
-- 
-- >>> Aeson.decode "1000" :: Maybe (GraphSON Int32)
-- Just (GraphSON {gsonType = Nothing, gsonValue = 1000})
-- >>> Aeson.decode "{\"@type\": \"g:Int32\", \"@value\": 1000}" :: Maybe (GraphSON Int32)
-- Just (GraphSON {gsonType = Just "g:Int32", gsonValue = 1000})
--
-- Note that encoding of the \"g:Map\" type is inconsistent between
-- GraphSON v1, v2 and v3. To handle the encoding, use
-- "Data.Greskell.GMap".
data GraphSON v =
  GraphSON
  { gsonType :: Maybe Text,
    -- ^ Type ID, corresponding to @\@type@ field.
    gsonValue :: v
    -- ^ Value, correspoding to @\@value@ field.
  }
  deriving (Show,Eq,Ord,Generic)

instance Functor GraphSON where
  fmap f gs = gs { gsonValue = f $ gsonValue gs }

instance Foldable GraphSON where
  foldr f start gs = f (gsonValue gs) start

instance Traversable GraphSON where
  traverse f gs = fmap (\v -> gs { gsonValue = v }) $ f $ gsonValue gs

instance Hashable v => Hashable (GraphSON v)

-- | Create a 'GraphSON' without 'gsonType'.
--
-- >>> nonTypedGraphSON (10 :: Int)
-- GraphSON {gsonType = Nothing, gsonValue = 10}
nonTypedGraphSON :: v -> GraphSON v
nonTypedGraphSON = GraphSON Nothing

-- | Create a 'GraphSON' with its type ID.
--
-- >>> typedGraphSON (10 :: Int32)
-- GraphSON {gsonType = Just "g:Int32", gsonValue = 10}
typedGraphSON :: GraphSONTyped v => v -> GraphSON v
typedGraphSON v = GraphSON (Just $ gsonTypeFor v) v

-- | Create a 'GraphSON' with the given type ID.
--
-- >>> typedGraphSON' "g:Int32" (10 :: Int)
-- GraphSON {gsonType = Just "g:Int32", gsonValue = 10}
typedGraphSON' :: Text -> v -> GraphSON v
typedGraphSON' t = GraphSON (Just t)

-- | If 'gsonType' is 'Just', the 'GraphSON' is encoded as a typed
-- JSON object. If 'gsonType' is 'Nothing', the 'gsonValue' is
-- directly encoded.
instance ToJSON v => ToJSON (GraphSON v) where
  toJSON gson = case gsonType gson of
    Nothing -> toJSON $ gsonValue gson
    Just t -> object [ "@type" .= t,
                       "@value" .= gsonValue gson
                     ]

-- | If the given 'Value' is a typed JSON object, 'gsonType' field of
-- the result is 'Just'. Otherwise, the given 'Value' is directly
-- parsed into 'gsonValue', and 'gsonType' is 'Nothing'.
instance FromJSON v => FromJSON (GraphSON v) where
  parseJSON v@(Object o) = do
    if length o /= 2
      then parseDirect v
      else do
      mtype <- o .:! "@type"
      mvalue <- o .:! "@value"
      maybe (parseDirect v) return $ typedGraphSON' <$> mtype <*> mvalue
  parseJSON v = parseDirect v
    
parseDirect :: FromJSON v => Value -> Parser (GraphSON v)
parseDirect v = GraphSON Nothing <$> parseJSON v


-- | Types that have an intrinsic type ID for 'gsonType' field.
class GraphSONTyped a where
  gsonTypeFor :: a -> Text
  -- ^ Type ID for 'gsonType'.

instance GraphSONTyped Char where
  gsonTypeFor _ = "gx:Char"

-- | Map to \"gx:Byte\". Note that Java's Byte is signed.
instance GraphSONTyped Int8 where
  gsonTypeFor _ = "gx:Byte"

instance GraphSONTyped Int16 where
  gsonTypeFor _ = "gx:Int16"

instance GraphSONTyped Int32 where
  gsonTypeFor _ = "g:Int32"

instance GraphSONTyped Int64 where
  gsonTypeFor _ = "g:Int64"

instance GraphSONTyped Float where
  gsonTypeFor _ = "g:Float"

instance GraphSONTyped Double where
  gsonTypeFor _ = "g:Double"

instance GraphSONTyped [a] where
  gsonTypeFor _ = "g:List"

instance GraphSONTyped (Vector a) where
  gsonTypeFor _ = "g:List"

-- | Map to \"g:Double\".
instance GraphSONTyped Scientific where
  gsonTypeFor _ = "g:Double"

-- | Note that Lazy HashMap and Strict HashMap are the same data type.
instance GraphSONTyped (HML.HashMap k v) where
  gsonTypeFor _ = "g:Map"

instance GraphSONTyped (HashSet a) where
  gsonTypeFor _ = "g:Set"

instance (GraphSONTyped a, GraphSONTyped b) => GraphSONTyped (Either a b) where
  gsonTypeFor e = either gsonTypeFor gsonTypeFor e

-- | Parse @GraphSON v@, but it checks 'gsonType'. If 'gsonType' is
-- 'Nothing' or it's not equal to 'gsonTypeFor', the 'Parser' fails.
parseTypedGraphSON :: (GraphSONTyped v, FromJSON v) => Value -> Parser (GraphSON v)
parseTypedGraphSON v = either fail return =<< parseTypedGraphSON' v

-- | Like 'parseTypedGraphSON', but this handles parse errors in a
-- finer granularity.
--
-- - If the given 'Value' is not a typed JSON object, it returns
--   'Left'.
-- - If the given 'Value' is a typed JSON object but it fails to parse
--   the \"\@value\" field, the 'Parser' fails.
-- - If the given 'Value' is a typed JSON object but the \"\@type\"
--   field is not equal to the 'gsonTypeFor' of type @v@, the 'Parser'
--   fails.
-- - Otherwise (if the given 'Value' is a typed JSON object with valid
--   \"\@type\" and \"\@value\" fields,) it returns 'Right'.
parseTypedGraphSON' :: (GraphSONTyped v, FromJSON v) => Value -> Parser (Either String (GraphSON v))
parseTypedGraphSON' v = do
  graphsonv <- parseGraphSONPlain v
  case gsonType graphsonv of
   Nothing -> return $ Left ("Not a valid typed JSON object.")
   Just got_type -> do
     goal <- parseJSON $ gsonValue graphsonv
     let exp_type = gsonTypeFor goal 
     when (got_type /= exp_type) $ do
       fail ("Expected @type of " ++ show exp_type ++ ", but got " ++ show got_type)
     return $ Right $ graphsonv { gsonValue = goal }
  where
    parseGraphSONPlain :: Value -> Parser (GraphSON Value)
    parseGraphSONPlain = parseJSON



-- | An Aeson 'Value' wrapped in 'GraphSON' wrapper type. Useful to
-- parse JSON text in GraphSON format.
newtype GValue = GValue { unGValue :: GraphSON GValueBody }
                 deriving (Show,Eq,Generic)

instance Hashable GValue

data GValueBody =
    GObject !(HashMap Text GValue)
  | GArray !(Vector GValue)
  | GString !Text
  | GNumber !Scientific
  | GBool !Bool
  | GNull
  deriving (Show,Eq,Generic)

instance Hashable GValueBody where
-- See Data.Aeson.Types.Internal
  hashWithSalt s (GObject o) = s `hashWithSalt` (0::Int) `hashWithSalt` o
  hashWithSalt s (GArray a) = foldl' hashWithSalt (s `hashWithSalt` (1::Int)) a
  hashWithSalt s (GString str) = s `hashWithSalt` (2::Int) `hashWithSalt` str
  hashWithSalt s (GNumber n) = s `hashWithSalt` (3::Int) `hashWithSalt` n
  hashWithSalt s (GBool b) = s `hashWithSalt` (4::Int) `hashWithSalt` b
  hashWithSalt s GNull = s `hashWithSalt` (5::Int)

-- | Parse 'GraphSON' wrappers recursively in 'Value', making it into
-- 'GValue'.
instance FromJSON GValue where
  parseJSON input = do
    gv <- parseJSON input
    recursed_value <- recurse $ gsonValue gv
    return $ GValue $ gv { gsonValue = recursed_value }
    where
      recurse :: Value -> Parser GValueBody
      recurse (Object o) = GObject <$> traverse parseJSON o
      recurse (Array a) = GArray <$> traverse parseJSON a
      recurse (String s) = return $ GString s
      recurse (Number n) = return $ GNumber n
      recurse (Bool b) = return $ GBool b
      recurse Null = return GNull

-- TODO: implement tests for unwrapGraphSON.

-- TODO: make FromGraphSON class
    
-- | Reconstruct 'Value' from 'GValue'.
instance ToJSON GValue where
  toJSON (GValue gson_body) = toJSON $ fmap toJSON gson_body

instance ToJSON GValueBody where
  toJSON (GObject o) = toJSON o
  toJSON (GArray a) = toJSON a
  toJSON (GString s) = String s
  toJSON (GNumber n) = Number n
  toJSON (GBool b) = Bool b
  toJSON GNull = Null

-- | Remove all 'GraphSON' wrappers recursively from 'GValue'.
unwrapAll :: GValue -> Value
unwrapAll = unwrapBase unwrapAll

-- | Remove the top-level 'GraphSON' wrapper, but leave other wrappers
-- as-is. The remaining wrappers are reconstructed by 'toJSON' to make
-- them into 'Value'.
unwrapOne :: GValue -> Value
unwrapOne = unwrapBase toJSON

unwrapBase :: (GValue -> Value) -> GValue -> Value
unwrapBase mapChild (GValue gson_body) = unwrapBody $ gsonValue gson_body
  where
    unwrapBody GNull = Null
    unwrapBody (GBool b) = Bool b
    unwrapBody (GNumber n) = Number n
    unwrapBody (GString s) = String s
    unwrapBody (GArray a) = Array $ fmap mapChild a
    unwrapBody (GObject o) = Object $ fmap mapChild o
