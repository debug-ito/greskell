{-# LANGUAGE OverloadedStrings #-}
module Main (main,spec) where

import qualified Data.Vector as V
import qualified Network.Greskell.WebSocket.Client as WS
import Test.Hspec

import Data.Greskell.Binder (newBind, runBinder)
import Data.Greskell.Graph
  ( AVertex, Key,
  )
import Data.Greskell.GTraversal
  ( Walk, GTraversal, SideEffect,
    source, sV', sAddV', gProperty, gId, gValues, gHasId, gHasLabel, gHas2,
    ($.), liftWalk
  )

import ServerTest.Common (withEnv, withClient)

main :: IO ()
main = hspec spec

spec :: Spec
spec = withEnv $ do
  spec_values_type

clearGraph :: WS.Client -> IO ()
clearGraph client = WS.drainResults =<< WS.submitRaw client "g.V().drop()" Nothing

spec_values_type :: SpecWith (String,Int)
spec_values_type = describe "return type of .values step" $ do
  specify "input Int, get Int" $ withClient $ \client -> do
    let prop_key :: Key AVertex Int
        prop_key = "foobar"
        searchProp = WS.drainResults =<< WS.submit client script (Just binding)
          where
            (script, binding) = runBinder $ do
              input <- newBind (100 :: Int)
              return $ gHas2 prop_key input $. sV' [] $ source "g"
        putProp = WS.slurpResults =<< WS.submit client script (Just binding)
          where
            (script, binding) = runBinder $ do
              input <- newBind (100 :: Int)
              return $ liftWalk gId $. gProperty prop_key input $. sAddV' "hoge" $ source "g"
        getProp vid = WS.slurpResults =<< WS.submit client script (Just binding)
          where
            (script, binding) = runBinder $ do
              vid_var <- newBind vid
              return $ gValues [prop_key] $. gHasId vid_var $. gHasLabel "hoge" $. sV' [] $ source "g"
    clearGraph client
    searchProp
    got_ids <- putProp
    got <- getProp (got_ids V.! 0)
    V.toList got `shouldBe` [100]
