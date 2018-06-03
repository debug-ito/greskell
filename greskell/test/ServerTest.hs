{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Main (main,spec) where

import Control.Category ((<<<))
import Control.Exception.Safe (bracket)
import qualified Data.Aeson as Aeson
import Data.Either (isRight)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Network.Greskell.WebSocket.Client as WS
import Data.Monoid (mempty, (<>))
import Data.Scientific (Scientific)
import Data.Text (unpack, Text)
import System.Environment (lookupEnv)
import Test.Hspec

import Data.Greskell.AsIterator
  ( AsIterator(IteratorItem)
  )
import Data.Greskell.GMap (GMapEntry, unGMapEntry)
import Data.Greskell.Gremlin
  ( oIncr, oDecr, cCompare, Order,
    Predicate(..), pLt, pAnd, pGte, pNot, pEq, pTest
  )
import Data.Greskell.Greskell
  ( toGremlin, Greskell, toGreskell, ToGreskell(..),
    true, false, list, value, single, number, gvalue',
    unsafeMethodCall, unsafeGreskell
  )
import Data.Greskell.Graph
  ( AVertex, AEdge, AProperty(..),
    T, tId, tLabel, tKey, tValue
  )
import Data.Greskell.GraphSON
  ( FromGraphSON, gValueBody, GValueBody(..), nonTypedGValue, GValue
  )
import Data.Greskell.GTraversal
  ( Walk, GTraversal, SideEffect,
    source, sV', sE', gV', sAddV', gAddE', gTo, gHasValue,
    ($.), gOrder, gBy1,
    Transform, unsafeWalk, unsafeGTraversal,
    gProperties, gProperty, liftWalk
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
  spec_graph


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
  describe "nested map" $ do
    let check :: Greskell (HashMap Int (HashMap Text Int)) -> [(Int, (HashMap Text Int))] -> SpecWith (String,Int)
        check = checkRawMapped unGMapEntry
    check (unsafeGreskell "[:]") []
    check (unsafeGreskell "[100: [\"foo\": 55], 200: [:], 300: [\"bar\": 60, \"buzz\": 65]]")
      [ (100, HM.fromList [("foo", 55)]),
        (200, mempty),
        (300, HM.fromList [("bar", 60), ("buzz", 65)])
      ]
  describe "array in map" $ do
    let check :: Greskell (HashMap Text [Int]) -> [(Text, [Int])] -> SpecWith (String,Int)
        check = checkRawMapped unGMapEntry
    check (unsafeGreskell "[:]") []
    check (unsafeGreskell "[\"foo\": [], \"bar\": [1,2,3]]")
      [ ("foo", []),
        ("bar", [1,2,3])
      ]

checkRawMapped :: (AsIterator a, b ~ IteratorItem a, FromGraphSON b, Eq c, Show c)
               => (b -> c)
               -> Greskell a
               -> [c]
               -> SpecWith (String, Int)
checkRawMapped mapResult input expected = specify label $ withClient $ \client -> do
  got <- WS.slurpResults =<< WS.submit client input Nothing
  map mapResult got `shouldBe` expected
  where
    label = unpack $ toGremlin input

checkRaw :: (AsIterator a, b ~ IteratorItem a, FromGraphSON b, Eq b, Show b)
         => Greskell a
         -> [b]
         -> SpecWith (String, Int)
checkRaw = checkRawMapped id

checkOne :: (AsIterator a, b ~ IteratorItem a, FromGraphSON b, Eq b, Show b)
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
  specFor' "tId" (gMapT tId) gValueBody [GNumber 10]
  specFor "tLabel" (gMapT tLabel) ["VLABEL"]
  specFor "tKey" (gMapT tKey <<< gProperties ["vprop"]) ["vprop"]
  specFor' "tValue" (gMapT tValue <<< gProperties ["vprop"]) gValueBody [GNumber 400]
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
    specFor' :: (FromGraphSON a, Eq b, Show b) => String -> Walk Transform AVertex a -> (a -> b) -> [b] -> SpecWith (String,Int)
    specFor' desc mapper convResult expected = specify desc $ withClient $ \client -> do
      got <- WS.slurpResults =<< WS.submit client (prefixedTraversal mapper) Nothing
      (map convResult got) `shouldBe` expected
    specFor :: (FromGraphSON a, Eq a, Show a) => String -> Walk Transform AVertex a -> [a] -> SpecWith (String,Int)
    specFor desc mapper expected = specFor' desc mapper id expected

spec_P :: SpecWith (String,Int)
spec_P = describe "P class" $ specify "pNot, pEq, pTest" $ withClient $ \client -> do
  let p = pNot $ pEq $ number 10
      test v = WS.slurpResults =<< WS.submit client (pTest p $ v) Nothing
  test (number 10) `shouldReturn` [False]
  test (number 15) `shouldReturn` [True]

spec_graph :: SpecWith (String,Int)
spec_graph = do
  specify "AProperty (edge properties)" $ withClient $ \client -> do
    let trav = gProperties [] $. sE' [] $ source "g"
        prop t = AProperty "condition" $ nonTypedGValue $ GString t
        expected = map prop [ ">=0.11.2.1",
                              ">=1.2.2.1",
                              ">=1.2.3"
                            ]
    got <- WS.slurpResults =<< WS.submit client (withPrelude trav) Nothing
    got `shouldBe` expected
  where
    withPrelude :: (ToGreskell a) => a -> Greskell (GreskellReturn a)
    withPrelude orig = unsafeGreskell (toGremlin prelude <> toGremlin orig)
    prelude :: Greskell ()
    prelude = unsafeGreskell $ mconcat $ map (<> "; ")
              ( [ "graph = org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph.open()",
                  "g = graph.traversal()",
                  "graph.addVertex(id, 1, label, 'package')",
                  "graph.addVertex(id, 2, label, 'package')",
                  "graph.addVertex(id, 3, label, 'package')",
                  finalize $ setName 1 "greskell",
                  finalize $ setName 2 "aeson",
                  finalize $ setName 3 "text",
                  finalize $ dependsOn 1 2 ">=0.11.2.1",
                  finalize $ dependsOn 1 3 ">=1.2.2.1",
                  finalize $ dependsOn 2 3 ">=1.2.3"
                ]
                ++ addVersion 1 "0.1.1.0" "2018-04-08"
                ++ addVersion 2 "1.3.1.1" "2018-05-10"
                ++ addVersion 2 "1.2.2.0" "2017-09-20"
                ++ addVersion 3 "1.2.3.0" "2017-12-27"
                ++ addVersion 3 "1.2.2.0" "2017-12-23"
              )
    finalize :: ToGreskell a => a -> Text
    finalize gt = (toGremlin gt) <> ".iterate()"
    num :: Integer -> Greskell GValue
    num = gvalue' . GNumber . fromInteger
    setName :: Integer -> Greskell Text -> GTraversal SideEffect () AVertex
    setName vid name = gProperty "name" name $. liftWalk $ sV' [num vid] $ source "g"
    dependsOn :: Integer -> Integer -> Greskell Text -> GTraversal SideEffect () AEdge
    dependsOn from_id to_id version_cond =
      gProperty "condition" version_cond
      $. (gAddE' "depends_on" $ gTo (gV' [num to_id]))
      $. liftWalk $ sV' [num from_id] $ source "g"
    addVersion :: Integer -> Greskell Text -> Greskell Text -> [Text]
    addVersion vid ver date =
      [ finalize $ gProperty "version" ver $. liftWalk $ sV' [num vid] $ source "g",
        finalize $ gProperty "date" date $. liftWalk $ gHasValue ver $. gProperties ["version"] $. sV' [num vid] $ source "g"
      ]



  
