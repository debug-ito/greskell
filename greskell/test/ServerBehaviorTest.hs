{-# LANGUAGE OverloadedStrings #-}
module Main (main,spec) where

import qualified Data.Vector as V
import qualified Network.Greskell.WebSocket.Client as WS
import System.IO (hPutStrLn, stderr)
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
  spec_generic_element_ID

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

spec_generic_element_ID :: SpecWith (String, Int)
spec_generic_element_ID = do
  specify "get Vertex ID as GValue, query Vertex by GValue" $ withClient $ \client -> do
    let prop_key :: Key AVertex Int
        prop_key = "sample"
        prop_val = 125
        make_v = liftWalk gId $. (liftWalk $ gProperty prop_key prop_val) $. (sAddV' "test" $ source "g")
    clearGraph client
    got_ids <- fmap V.toList $ WS.slurpResults =<< WS.submit client make_v Nothing
    hPutStrLn stderr ("Got IDs: " <> show got_ids)
    length got_ids `shouldBe` 1
    let (q, qbind) = runBinder $ do
          vid <- newBind (got_ids !! 0)
          return $ gValues [prop_key] $. (sV' [vid] $ source "g")
    got_vals <- fmap V.toList $ WS.slurpResults =<< WS.submit client q (Just qbind)
    got_vals `shouldBe` [125]
    
