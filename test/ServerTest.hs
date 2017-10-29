{-# LANGUAGE OverloadedStrings #-}
module Main (main,spec) where

import qualified Data.Aeson as Aeson
import Data.Monoid (mempty, (<>))
import Data.Scientific (Scientific)
import Data.Text (unpack, Text)
import qualified Database.TinkerPop as TP
import qualified Database.TinkerPop.Types as TP (Connection)
import System.Environment (lookupEnv)
import Test.Hspec

import Data.Greskell.Gremlin
  ( oIncr, cCompare, Order
  )
import Data.Greskell.Greskell
  ( toGremlin, Greskell,
    true, false, list, value, singleton
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = withEnv $ do
  spec_basics
  spec_comparator


spec_basics :: SpecWith (String,Int)
spec_basics = do
  ---- Note: Currently these tests do not support GraphSON 2.0 or
  ---- later. Try them with GraphSON 1.0 serializer.
  describe "Num" $ do
    let checkInt :: Greskell Int -> Int -> SpecWith (String,Int)
        checkInt = checkOne
    checkInt 100 100
    checkInt (20 + 30) (20 + 30)
    checkInt (10 - 3 * 6) (10 - 3 * 6)
    checkInt (-99) (-99)
    checkInt (abs (-53)) (abs (-53))
    checkInt (signum 0) (signum 0)
    checkInt (signum 99) (signum 99)
    checkInt (signum (-12)) (signum (-12))
  describe "Fractional" $ do
    let checkFrac :: Greskell Scientific -> Scientific -> SpecWith (String,Int)
        checkFrac = checkOne
    checkFrac (20.5) (20.5)
    checkFrac (20.123) (20.123)
    checkFrac (32.25 / 2.5) (32.25 / 2.5)
    checkFrac (19.2 * recip 12.5) (19.2 * recip 12.5)
  describe "Monoid" $ do
    let checkT :: Greskell Text -> Text -> SpecWith (String,Int)
        checkT = checkOne
    checkT mempty mempty
    checkT ("hello, " <> "world!") ("hello, " <> "world!")
    checkT ("!\"#$%&'()=~\\|><+*;:@{}[]/?_\r\n\t  ") ("!\"#$%&'()=~\\|><+*;:@{}[]/?_\r\n\t  ")
  describe "Bool" $ do
    let checkB :: Greskell Bool -> Bool -> SpecWith (String,Int)
        checkB = checkOne
    checkB true True
    checkB false False
  describe "list" $ do
    let checkL :: Greskell [Int] -> [Int] -> SpecWith (String,Int)
        checkL = checkRaw
    checkL (list []) []
    checkL (list [20,30,20,10]) [20,30,20,10]
  describe "value (object)" $ do
    let checkV :: Greskell Aeson.Value -> Aeson.Value -> SpecWith (String,Int)
        checkV i e = checkRaw (singleton i) [e]
    checkV (value $ Aeson.object []) (Aeson.object [])
    
    let simple_nonempty = Aeson.object [("foo", Aeson.String "hoge"), ("bar", Aeson.Number 20)]
    checkV (value simple_nonempty) simple_nonempty
    
    let array_in_obj = Aeson.object [("foo", Aeson.toJSON [(3 :: Int), 2, 1]), ("hoge", Aeson.toJSON [("a" :: Text), "b", "c"])]
    checkV (value array_in_obj) array_in_obj


checkRaw :: Aeson.ToJSON b => Greskell a -> [b] -> SpecWith (String, Int)
checkRaw  input expected = specify label $ withConn $ \conn -> do
  TP.submit conn (toGremlin input) Nothing `shouldReturn` Right (map Aeson.toJSON expected)
  where
    label = unpack $ toGremlin input

checkOne :: Aeson.ToJSON a => Greskell a -> a -> SpecWith (String, Int)
checkOne input expected = checkRaw input [expected]

requireEnv :: String -> IO String
requireEnv env_key = maybe bail return =<< lookupEnv env_key
  where
    bail = pendingWith msg >> return ""
      where
        msg = "Set environment variable "++ env_key ++ " for Server test. "

withEnv :: SpecWith (String, Int) -> Spec
withEnv = before $ do
  hostname <- requireEnv "GRESKELL_TEST_HOST"
  port <- fmap read $ requireEnv "GRESKELL_TEST_PORT"
  return (hostname, port)

withConn :: (TP.Connection -> IO ()) -> (String, Int) -> IO ()
withConn act (host, port) = TP.run host port act

spec_comparator :: SpecWith (String,Int)
spec_comparator = do
  let oIncr' :: Greskell (Order Int)
      oIncr' = oIncr
  checkOne (cCompare oIncr' 20 20) 0
  checkOne (cCompare oIncr' 10 20) (-1)
  checkOne (cCompare oIncr' 20 10) 1
