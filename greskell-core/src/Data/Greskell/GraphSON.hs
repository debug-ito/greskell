{-# LANGUAGE OverloadedStrings, DeriveGeneric, TypeFamilies #-}
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
         unwrapOne,
         -- * FromGraphSON
         FromGraphSON(..),
         -- ** parser support
         parseUnwrapAll,
         parseUnwrapTraversable
       ) where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad (when)
import Data.Aeson
  ( ToJSON(toJSON), FromJSON(parseJSON), FromJSONKey,
    object, (.=), Value(..), (.:!)
  )
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import Data.Foldable (Foldable(foldr), foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Lazy as L (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable(..))
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.IntMap.Lazy as L (IntMap)
import Data.IntSet (IntSet)
import qualified Data.Map.Lazy as L (Map)
import Data.Ratio (Ratio)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Traversable (Traversable(traverse))
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)
import GHC.Exts (IsList(Item))
import GHC.Generics (Generic)

import Data.Greskell.GraphSON.GraphSONTyped (GraphSONTyped(..))
import Data.Greskell.GMap (GMap, GMapEntry, unGMap)

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



-- | An Aeson 'Value' wrapped in 'GraphSON' wrapper type. Basically
-- this type is the Haskell representaiton of a GraphSON-encoded
-- document.
--
-- This type is used to parse GraphSON documents. See also
-- 'FromGraphSON' class.
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

-- | Types that can be constructed from 'GValue'. This is analogous to
-- 'FromJSON' class.
class FromGraphSON a where
  parseGraphSON :: GValue -> Parser a

-- | Unwrap the given 'GValue' with 'unwrapAll', and just parse the
-- result with 'parseJSON'.
--
-- Useful to implement 'FromGraphSON' instances for scalar types.
parseUnwrapAll :: FromJSON a => GValue -> Parser a
parseUnwrapAll gv = parseJSON $ unwrapAll gv

-- | Unwrap the given 'GValue' with 'unwrapOne', parse the result to
-- @(t GValue)@, and recursively parse the children with
-- 'parseGraphSON'.
--
-- Useful to implement 'FromGraphSON' instances for 'Traversable'
-- types.
parseUnwrapTraversable :: (Traversable t, FromJSON (t GValue), FromGraphSON a)
                       => GValue -> Parser (t a)
parseUnwrapTraversable gv = traverse parseGraphSON =<< (parseJSON $ unwrapOne gv)

---- Trivial instances

instance FromGraphSON Int where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Text where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON TL.Text where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Bool where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Char where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Double where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Float where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Int8 where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Int16 where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Int32 where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Int64 where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Integer where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Natural where
  parseGraphSON = parseUnwrapAll
instance (FromJSON a, Integral a) => FromGraphSON (Ratio a) where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Word where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Word8 where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Word16 where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Word32 where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Word64 where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON Scientific where
  parseGraphSON = parseUnwrapAll
instance FromGraphSON IntSet where
  parseGraphSON = parseUnwrapAll

---- List instances

instance FromGraphSON a => FromGraphSON [a] where
  parseGraphSON = parseUnwrapTraversable
instance FromGraphSON a => FromGraphSON (Vector a) where
  parseGraphSON = parseUnwrapTraversable
instance FromGraphSON a => FromGraphSON (Seq a) where
  parseGraphSON = parseUnwrapTraversable

-- TODO: Set and HashSet is not Traversable. Should we use IsList?


---- Map instances

instance (FromGraphSON v, Eq k, Hashable k, FromJSONKey k, FromJSON k) => FromGraphSON (L.HashMap k v) where
  parseGraphSON = fmap unGMap . parseUnwrapTraversable
instance (FromGraphSON v, Ord k, FromJSONKey k, FromJSON k) => FromGraphSON (L.Map k v) where
  parseGraphSON = fmap unGMap . parseUnwrapTraversable

-- TODO: IntMap cannot be used with GMap.

-- instance FromGraphSON v => FromGraphSON (L.IntMap v) where
--   parseGraphSON = fmap unGMap . parseUnwrapTraversable


---- GMap and GMapEntry

instance (Traversable (c k), FromJSON k, IsList (c k GValue), Item (c k GValue) ~ (k,GValue), FromJSON (c k GValue), FromGraphSON v)
         => FromGraphSON (GMap c k v) where
  parseGraphSON = parseUnwrapTraversable
instance (FromJSON k, FromJSONKey k, Ord k, FromGraphSON v) => FromGraphSON (GMapEntry k v) where
  parseGraphSON = parseUnwrapTraversable


---- Maybe and Either

-- | Parse 'GNull' into 'Nothing'.
instance FromGraphSON a => FromGraphSON (Maybe a) where
  parseGraphSON (GValue (GraphSON _ GNull)) = return Nothing
  parseGraphSON gv = fmap Just $ parseGraphSON gv

-- | Try 'Left', then 'Right'.
instance (FromGraphSON a, FromGraphSON b) => FromGraphSON (Either a b) where
  parseGraphSON gv = (fmap Left $ parseGraphSON gv) <|> (fmap Right $ parseGraphSON gv)


---- Value

-- | Call 'unwrapAll' to remove all GraphSON wrappers.
instance FromGraphSON Value where
  parseGraphSON = return . unwrapAll
