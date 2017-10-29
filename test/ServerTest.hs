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

import Data.Greskell.Greskell
  ( toGremlin, Greskell,
    true, false
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = withEnv $ do
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

checkOne :: Aeson.ToJSON a => Greskell a -> a -> SpecWith (String, Int)
checkOne input expected = specify label $ withConn $ \conn -> do
  TP.submit conn (toGremlin input) Nothing `shouldReturn` Right [Aeson.toJSON expected]
  where
    label = unpack $ toGremlin input

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

