module ServerTest.Client (main,spec) where

import Control.Exception.Safe (bracket)
import Test.Hspec

import Data.Greskell.Greskell (Greskell)

import Data.Greskell.WebSocket.Client
  ( Host, Port, Client, Options,
    connectWith, close, submit,
    defOptions,
    nextResult
  )

import TestUtil.Env (withEnvForExtServer)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Client" $ do
  withEnvForExtServer $ do
    client_basic_spec

withClient :: (Client -> IO a) -> (Host, Port) -> IO a
withClient act (host, port) = forClient' defOptions host port act

forClient' :: Options -> Host -> Port -> (Client -> IO a) -> IO a
forClient' opt host port act = bracket (connectWith opt host port) close act

client_basic_spec :: SpecWith (Host, Port)
client_basic_spec = do
  specify "eval Int" $ withClient $ \client -> do
    let g = 108 :: Greskell Int
    rh <- submit client g Nothing
    nextResult rh `shouldReturn` Just 108
    nextResult rh `shouldReturn` Nothing
    nextResult rh `shouldReturn` Nothing
