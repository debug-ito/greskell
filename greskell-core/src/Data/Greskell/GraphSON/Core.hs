{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Data.Greskell.GraphSON.Core
-- Description:
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __Internal module.__ Definition of 'GraphSON' type.
module Data.Greskell.GraphSON.Core
    ( GraphSON (..)
    , nonTypedGraphSON
    , typedGraphSON
    , typedGraphSON'
    , parseTypedGraphSON
    , parseTypedGraphSON'
    ) where

import           Control.Applicative                  ((<$>), (<*>))
import           Control.Monad                        (when)
import           Data.Aeson                           (FromJSON (parseJSON), ToJSON (toJSON),
                                                       Value (..), object, (.=))
import qualified Data.Aeson                           as Aeson
import           Data.Aeson.Types                     (Parser)
import           Data.Foldable                        (Foldable (foldr))
import           Data.Hashable                        (Hashable (..))
import           Data.Text                            (Text)
import           Data.Traversable                     (Traversable (traverse))
import           GHC.Generics                         (Generic)

import           Data.Greskell.GraphSON.GraphSONTyped (GraphSONTyped (..))

-- | Wrapper for \"typed JSON object\" introduced in GraphSON version
-- 2. See http://tinkerpop.apache.org/docs/current/dev/io/#graphson
--
-- This data type is useful for encoding/decoding GraphSON text.
--
-- Note that encoding of the \"g:Map\" type is inconsistent between
-- GraphSON v1 and v2, v3. To handle the encoding, use
-- "Data.Greskell.GMap".
data GraphSON v
  = GraphSON
      { gsonType  :: Maybe Text
        -- ^ Type ID, corresponding to @\@type@ field.
      , gsonValue :: v
        -- ^ Value, correspoding to @\@value@ field.
      }
  deriving (Eq, Generic, Ord, Show)

instance Functor GraphSON where
  fmap f gs = gs { gsonValue = f $ gsonValue gs }

instance Foldable GraphSON where
  foldr f start gs = f (gsonValue gs) start

instance Traversable GraphSON where
  traverse f gs = fmap (\v -> gs { gsonValue = v }) $ f $ gsonValue gs

-- | @since 0.1.2.0
instance Hashable v => Hashable (GraphSON v)

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
      mtype <- o Aeson..:! "@type"
      mvalue <- o Aeson..:! "@value"
      maybe (parseDirect v) return $ typedGraphSON' <$> mtype <*> mvalue
  parseJSON v = parseDirect v

parseDirect :: FromJSON v => Value -> Parser (GraphSON v)
parseDirect v = GraphSON Nothing <$> parseJSON v


-- | Create a 'GraphSON' without 'gsonType'.
nonTypedGraphSON :: v -> GraphSON v
nonTypedGraphSON = GraphSON Nothing

-- | Create a 'GraphSON' with its type ID.
typedGraphSON :: GraphSONTyped v => v -> GraphSON v
typedGraphSON v = GraphSON (Just $ gsonTypeFor v) v

-- | Create a 'GraphSON' with the given type ID.
typedGraphSON' :: Text -> v -> GraphSON v
typedGraphSON' t = GraphSON (Just t)


-- | Parse @GraphSON v@, but it checks 'gsonType'. If 'gsonType' is
-- 'Nothing' or it's not equal to 'gsonTypeFor', the 'Parser' fails.
parseTypedGraphSON :: (GraphSONTyped v, FromJSON v) => Value -> Parser (GraphSON v)
parseTypedGraphSON v = either fail return =<< parseTypedGraphSON' v

-- | Note: this function is not exported because I don't need it for
-- now. If you need this function, just open an issue.
--
-- Like 'parseTypedGraphSON', but this handles parse errors in a finer
-- granularity.
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
