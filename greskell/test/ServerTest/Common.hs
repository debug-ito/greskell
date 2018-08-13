-- |
-- Module: ServerTest.Common
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module ServerTest.Common
       ( withEnv,
         withClient
       ) where

import Control.Exception.Safe (bracket)
import qualified Network.Greskell.WebSocket.Client as WS
import System.Environment (lookupEnv)
import Test.Hspec

requireEnv :: String -> IO String
requireEnv env_key = maybe bail return =<< lookupEnv env_key
  where
    bail = expectationFailure msg >> return ""
      where
        msg = "Set environment variable "++ env_key ++ " for Server test. "

withEnv :: SpecWith (String, Int) -> Spec
withEnv = before $ do
  hostname <- requireEnv "GRESKELL_TEST_HOST"
  port <- fmap read $ requireEnv "GRESKELL_TEST_PORT"
  return (hostname, port)

withClient :: (WS.Client -> IO ()) -> (String, Int) -> IO ()
withClient act (host, port) = bracket (WS.connect host port) WS.close act


