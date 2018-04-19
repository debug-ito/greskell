{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Data.Greskell.GraphSON
-- Description: Encoding and decoding GraphSON
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.GraphSON
       ( -- * Type
         GraphSON(..),
         GraphSONTyped(..),
         -- * Constructors
         nonTypedGraphSON,
         typedGraphSON,
         typedGraphSON',
         -- * Parser support
         parseTypedGraphSON,
         parseTypedGraphSON'
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), object, (.=), Value(Object), (.:?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import Data.Foldable (Foldable(foldr))
import qualified Data.HashMap.Lazy as HML
import Data.HashSet (HashSet)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Traversable (Traversable(traverse))

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
data GraphSON v =
  GraphSON
  { gsonType :: Maybe Text,
    -- ^ Type ID, corresponding to @\@type@ field.
    gsonValue :: v
    -- ^ Value, correspoding to @\@value@ field.
  }
  deriving (Show,Eq,Ord)

instance Functor GraphSON where
  fmap f gs = gs { gsonValue = f $ gsonValue gs }

instance Foldable GraphSON where
  foldr f start gs = f (gsonValue gs) start

instance Traversable GraphSON where
  traverse f gs = fmap (\v -> gs { gsonValue = v }) $ f $ gsonValue gs

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
      mtype <- o .:? "@type"
      mvalue <- o .:? "@value"
      maybe (parseDirect v) return $ typedGraphSON'  <$> mtype <*> mvalue
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
parseTypedGraphSON v = checkType =<< parseJSON v
  where
    checkType gson = do
      let exp_type = gsonTypeFor $ gsonValue gson
          mgot_type = gsonType gson
      when (mgot_type /= Just exp_type) $ do
        fail ("Expected @type of " ++ show exp_type ++ ", but got " ++ show mgot_type)
      return gson

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
