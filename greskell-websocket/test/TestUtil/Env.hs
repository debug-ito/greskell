module TestUtil.Env
       ( requireEnv,
         withEnvForExtServer,
         withEnvForIntServer
       ) where

import System.Environment (lookupEnv)
import Test.Hspec

import Network.Greskell.WebSocket.Connection (Host, Port)

requireEnv :: String -> IO String
requireEnv env_key = maybe bail return =<< lookupEnv env_key
  where
    bail = expectationFailure msg >> return ""
      where
        msg = "Set environment variable "++ env_key ++ " for Server test. "

withEnvForExtServer :: SpecWith (Host, Port) -> Spec
withEnvForExtServer = before $ do
  hostname <- requireEnv "GRESKELL_TEST_HOST"
  port <- fmap read $ requireEnv "GRESKELL_TEST_PORT"
  return (hostname, port)

withEnvForIntServer :: SpecWith Port -> Spec
withEnvForIntServer = before $ fmap read $ requireEnv "GRESKELL_TEST_INTERNAL_PORT"

