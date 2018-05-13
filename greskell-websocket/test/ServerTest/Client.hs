{-# LANGUAGE OverloadedStrings #-}
module ServerTest.Client (main,spec) where

import Control.Exception.Safe (bracket)
import Control.Monad (forM_)
import Data.Text (Text)
import Test.Hspec

import Data.Greskell.Greskell (Greskell)
import qualified Data.Greskell.Greskell as G
import Data.Greskell.GMap (GMap, GMapEntry, unGMapEntry)

import Data.Greskell.WebSocket.Client
  ( Host, Port, Client, Options,
    connectWith, close, submit,
    defOptions,
    nextResult, slurpResults
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
  specify "eval Text" $ withClient $ \client -> do
    let g = "hoge" :: Greskell Text
    rh <- submit client g Nothing
    nextResult rh `shouldReturn` Just "hoge"
    nextResult rh `shouldReturn` Nothing
    nextResult rh `shouldReturn` Nothing
  specify "eval [Int]" $ withClient $ \client -> do
    let g = G.list $ map fromInteger [1..20] :: Greskell [Int]
    rh <- submit client g Nothing
    forM_  [1..20] $ \n -> 
      nextResult rh `shouldReturn` Just n
    nextResult rh `shouldReturn` Nothing
    nextResult rh `shouldReturn` Nothing
  specify "eval (GMap Int String)" $ withClient $ \client -> do
    let g :: Greskell (GMap Int String)
        g = G.unsafeGreskell "[100: 'hoge', 200: 'foo', 300: 'bar']"
    rh <- submit client g Nothing
    got <- fmap (map unGMapEntry) $ slurpResults rh
    got `shouldMatchList` [(100, "hoge"), (200, "foo"), (300, "bar")]
