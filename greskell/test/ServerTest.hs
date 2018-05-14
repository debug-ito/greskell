{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Main (main,spec) where

import Control.Category ((<<<))
import Control.Exception.Safe (bracket)
import qualified Data.Aeson as Aeson
import Data.Either (isRight)
import qualified Data.Greskell.WebSocket.Client as WS
import Data.Monoid (mempty, (<>))
import Data.Scientific (Scientific)
import Data.Text (unpack, Text)
import System.Environment (lookupEnv)
import Test.Hspec

import Data.Greskell.AsIterator
  ( AsIterator(IteratorItem)
  )
import Data.Greskell.Gremlin
  ( oIncr, oDecr, cCompare, Order,
    Predicate(..), pLt, pAnd, pGte, pNot, pEq, pTest
  )
import Data.Greskell.Greskell
  ( toGremlin, Greskell, toGreskell,
    true, false, list, value, single, number,
    unsafeMethodCall
  )
import Data.Greskell.Graph
  ( AVertex, T, tId, tLabel, tKey, tValue
  )
import Data.Greskell.GTraversal
  ( Walk, GTraversal,
    source, sV', ($.), gOrder, gBy1,
    Transform, unsafeWalk, unsafeGTraversal,
    gProperties
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = withEnv $ do
  spec_basics
  spec_comparator
  spec_predicate
  spec_T
  spec_P


spec_basics :: SpecWith (String,Int)
spec_basics = do
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
  describe "number" $ do
    let checkN :: Greskell Scientific -> Scientific -> SpecWith (String,Int)
        checkN = checkOne
    checkN (number 3.1415) (3.1415)
    checkN (number 2.31e12) (2.31e12)
    checkN (number (-434.23e-19)) (-434.23e-19)
  -- Now failing.. We need a way to unwrap GraphSON wrapper recursively.
  describe "value (object)" $ do
    let checkV :: Greskell Aeson.Value -> Aeson.Value -> SpecWith (String,Int)
        checkV i e = checkRaw (single i) [e]
    checkV (value $ Aeson.object []) (Aeson.object [])
    
    let simple_nonempty = Aeson.object [("foo", Aeson.String "hoge"), ("bar", Aeson.Number 20)]
    checkV (value simple_nonempty) simple_nonempty
    
    let array_in_obj = Aeson.object [("foo", Aeson.toJSON [(3 :: Int), 2, 1]), ("hoge", Aeson.toJSON [("a" :: Text), "b", "c"])]
    checkV (value array_in_obj) array_in_obj

checkRaw :: (AsIterator a, b ~ IteratorItem a, Aeson.FromJSON b, Eq b, Show b)
         => Greskell a
         -> [b]
         -> SpecWith (String, Int)
checkRaw  input expected = specify label $ withClient $ \client -> do
  got <- WS.slurpResults =<< WS.submit client input Nothing
  got `shouldBe` expected
  where
    label = unpack $ toGremlin input

checkOne :: (AsIterator a, b ~ IteratorItem a, Aeson.FromJSON b, Eq b, Show b)
         => Greskell a -> b -> SpecWith (String, Int)
checkOne input expected = checkRaw input [expected]

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

spec_comparator :: SpecWith (String,Int)
spec_comparator = do
  let oIncr' :: Greskell (Order Int)
      oIncr' = oIncr
      oDecr' :: Greskell (Order Int)
      oDecr' = oDecr
  checkOne (cCompare oIncr' 20 20) 0
  checkOne (cCompare oIncr' 10 20) (-1)
  checkOne (cCompare oIncr' 20 10) 1
  checkOne (cCompare oDecr' 20 20) 0
  checkOne (cCompare oDecr' 10 20) 1
  checkOne (cCompare oDecr' 20 10) (-1)

spec_predicate :: SpecWith (String,Int)
spec_predicate = do
  checkOne (pTest (pLt 20 `pAnd` pGte 10) (5 :: Greskell Int)) False
  checkOne (pTest (pLt 20 `pAnd` pGte 10) (10 :: Greskell Int)) True
  checkOne (pTest (pLt 20 `pAnd` pGte 10) (15 :: Greskell Int)) True
  checkOne (pTest (pLt 20 `pAnd` pGte 10) (20 :: Greskell Int)) False

spec_T :: SpecWith (String,Int)
spec_T = describe "T enum" $ do
  specFor "tId" (gMapT tId) [Aeson.Number 10]
  specFor "tLabel" (gMapT tLabel) ["VLABEL"]
  specFor "tKey" (gMapT tKey <<< gProperties ["vprop"]) ["vprop"]
  specFor "tValue" (gMapT tValue <<< gProperties ["vprop"]) [Aeson.Number 400]
  where
    gMapT :: Greskell (T a b) -> Walk Transform a b
    gMapT t = unsafeWalk "map" ["{ " <> toGremlin (unsafeMethodCall t "apply" ["it.get()"]) <> " }"]
    prefixedTraversal :: Walk Transform AVertex a -> GTraversal Transform () a
    prefixedTraversal mapper = unsafeGTraversal (prelude <> body)
      where
        prelude = 
          ( "graph = org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph.open(); "
            <> "v = graph.addVertex(id, 10, label, \"VLABEL\"); "
            <> "v.property(\"vprop\", 400, \"a\", \"A\", \"b\", \"B\"); "
            <> "g = graph.traversal(); "
          )
        body = toGremlin $ mapper $. sV' [] $ source "g"
    specFor :: (Aeson.FromJSON a, Eq a, Show a) => String -> Walk Transform AVertex a -> [a] -> SpecWith (String,Int)
    specFor desc mapper expected = specify desc $ withClient $ \client -> do
      got <- WS.slurpResults =<< WS.submit client (prefixedTraversal mapper) Nothing
      got `shouldBe` expected

spec_P :: SpecWith (String,Int)
spec_P = describe "P class" $ specify "pNot, pEq, pTest" $ withClient $ \client -> do
  let p = pNot $ pEq $ number 10
      test v = WS.slurpResults =<< WS.submit client (pTest p $ v) Nothing
  test (number 10) `shouldReturn` [False]
  test (number 15) `shouldReturn` [True]
