module Main (main,spec) where

import Control.Exception.Safe (bracket)
import Data.Aeson (Value)
import System.Environment (lookupEnv)
import Test.Hspec

import Data.Greskell.WebSocket.Codec.JSON (jsonCodec)
import Data.Greskell.WebSocket.Connection
  ( Host, Port, Connection,
    close, connect, sendRequest, getResponse
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = withEnv $ do
  describe "Connection" $ do
    conn_basic_spec

requireEnv :: String -> IO String
requireEnv env_key = maybe bail return =<< lookupEnv env_key
  where
    bail = pendingWith msg >> return ""
      where
        msg = "Set environment variable "++ env_key ++ " for Server test. "

withEnv :: SpecWith (Host, Port) -> Spec
withEnv = before $ do
  hostname <- requireEnv "GRESKELL_TEST_HOST"
  port <- fmap read $ requireEnv "GRESKELL_TEST_PORT"
  return (hostname, port)

withConn :: (Connection Value -> IO a) -> (Host, Port) -> IO a
withConn act (host, port) = bracket makeConn close act
  where
    makeConn = connect jsonCodec host port "/gremlin"

conn_basic_spec :: SpecWith (Host, Port)
conn_basic_spec = do
  specify "basic transaction" $ withConn $ \conn -> do
    True `shouldBe` False
