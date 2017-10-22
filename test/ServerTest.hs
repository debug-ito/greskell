module Main (main,spec) where

import qualified Data.Aeson as Aeson
import qualified Database.TinkerPop as TP
import qualified Database.TinkerPop.Types as TP (Connection)
import System.Environment (lookupEnv)
import Test.Hspec

import Data.Greskell.Greskell (toGremlin, Greskell)

main :: IO ()
main = hspec spec

spec :: Spec
spec = withEnv $ do
  specify "sample" $ withConn $ \conn -> do
    let g = toGremlin (100 :: Greskell Int)
    TP.submit conn g Nothing `shouldReturn` Right [Aeson.Number 100]

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

