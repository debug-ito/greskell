{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Main (main,spec) where

import Control.Category ((<<<), (>>>))
import qualified Data.Aeson as Aeson
import Data.Either (isRight)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)
import Data.Monoid (mempty, (<>))
import Data.Scientific (Scientific)
import Data.Text (unpack, Text)
import qualified Data.Vector as V
import qualified Network.Greskell.WebSocket.Client as WS
import Test.Hspec

import Data.Greskell.AsLabel (AsLabel(..), lookupAsM)
import qualified Data.Greskell.AsLabel as As
import Data.Greskell.AsIterator
  ( AsIterator(IteratorItem)
  )
import Data.Greskell.Binder (newBind, runBinder)
import Data.Greskell.GMap (GMapEntry, unGMapEntry)
import Data.Greskell.Gremlin
  ( oIncr, oDecr, cCompare, Order,
    Predicate(..), pLt, pAnd, pGte, pNot, pEq, pTest
  )
import Data.Greskell.Greskell
  ( toGremlin, Greskell, toGreskell, ToGreskell(..),
    true, false, list, value, single, number, gvalueInt,
    unsafeMethodCall, unsafeGreskell
  )
import Data.Greskell.Graph
  ( AVertex(..), AEdge(..), AProperty(..), AVertexProperty(..),
    Key, Keys(..),
    T, tId, tLabel, tKey, tValue, cList, (=:),
    ElementID(..)
  )
import Data.Greskell.GraphSON
  ( FromGraphSON, nonTypedGValue, GValue,
    parseEither
  )
import Data.Greskell.GTraversal
  ( Walk, GTraversal, SideEffect,
    source, sV', sE', gV', sAddV', gAddE', gTo,
    ($.), gOrder, gBy1, gBy, gByL,
    Transform, unsafeWalk, unsafeGTraversal,
    gProperties, gProperty, gPropertyV, liftWalk, gValues,
    gAs, gSelect1, gSelectN, gSelectBy1, gSelectByN,
    gFilter, gOut', gOutV, gInV, gId, gLabel, gProject,
    gValueMap
  )
import Data.Greskell.PMap (lookupAsM)

import ServerTest.Common (withEnv, withClient)

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
  spec_as
  spec_selectBy

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
  fmap mapResult got `shouldBe` V.fromList expected
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

iterateTraversal :: GTraversal c s e -> Greskell ()
iterateTraversal gt = unsafeMethodCall (toGreskell gt) "iterate" []

spec_T :: SpecWith (String,Int)
spec_T = describe "T enum" $ do
  specFor' "tId" (gMapT tId) (parseEither . unElementID) [(Right 10 :: Either String Int)]
  specFor "tLabel" (gMapT tLabel) ["VLABEL"]
  specFor "tKey" (gMapT tKey <<< gProperties ["vprop"]) ["vprop"]
  specFor' "tValue" (gMapT tValue <<< gProperties ["vprop"]) parseEither [(Right 400 :: Either String Int)]
  where
    gMapT :: Greskell (T a b) -> Walk Transform a b
    gMapT t = unsafeWalk "map" ["{ " <> toGremlin (unsafeMethodCall t "apply" ["it.get()"]) <> " }"]
    prefixedTraversal :: Walk Transform AVertex a -> GTraversal Transform () a
    prefixedTraversal mapper = unsafeGTraversal (prelude <> body)
      where
        prelude = 
          ( "graph = org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph.open(); "
            <> "g = graph.traversal(); "
            <> "graph.addVertex(id, 10, label, \"VLABEL\"); "
            <> ( toGremlin $ iterateTraversal
                 $ gPropertyV Nothing "vprop" (gvalueInt $ (400 :: Int))
                   ["a" =: ("A" :: Greskell Text), "b" =: ("B" :: Greskell Text)]
                 $. liftWalk $ sV' [] $ source "g"
               ) <> "; "
          )
        body = toGremlin $ mapper $. sV' [] $ source "g"
    specFor' :: (FromGraphSON a, Eq b, Show b) => String -> Walk Transform AVertex a -> (a -> b) -> [b] -> SpecWith (String,Int)
    specFor' desc mapper convResult expected = specify desc $ withClient $ \client -> do
      got <- WS.slurpResults =<< WS.submit client (prefixedTraversal mapper) Nothing
      (fmap convResult got) `shouldBe` V.fromList expected
    specFor :: (FromGraphSON a, Eq a, Show a) => String -> Walk Transform AVertex a -> [a] -> SpecWith (String,Int)
    specFor desc mapper expected = specFor' desc mapper id expected

spec_P :: SpecWith (String,Int)
spec_P = describe "P class" $ specify "pNot, pEq, pTest" $ withClient $ \client -> do
  let p = pNot $ pEq $ number 10
      test v = WS.slurpResults =<< WS.submit client (pTest p $ v) Nothing
  test (number 10) `shouldReturn` V.fromList [False]
  test (number 15) `shouldReturn` V.fromList [True]

withPrelude :: (ToGreskell a) => Greskell () -> a -> Greskell (GreskellReturn a)
withPrelude prelude orig = unsafeGreskell (toGremlin prelude <> toGremlin orig)


-- | This test is supported TinkerPop 3.1.0 and above, because it uses
-- 'gAddE'' function.
spec_graph :: SpecWith (String,Int)
spec_graph = do
  specify "AProperty (edge properties)" $ withClient $ \client -> do
    let trav = gProperties [] $. sE' [] $ source "g"
        prop t = AProperty "condition" $ Right (t :: Text)
        expected = map prop [ ">=0.11.2.1",
                              ">=1.2.2.1",
                              ">=1.2.3"
                            ]
    got <- WS.slurpResults =<< WS.submit client (withPrelude' trav) Nothing
    (map (fmap parseEither) $ V.toList got) `shouldMatchList` expected
  specify "AProperty (vertex property meta-properties)" $ withClient $ \client -> do
    let trav = gProperties [] $. gProperties [] $. sV' [] $ source "g"
        prop t = AProperty "date" $ Right (t :: Text)
        expected = map prop [ "2018-04-08",
                              "2018-05-10",
                              "2017-09-20",
                              "2017-12-27",
                              "2017-12-23"
                            ]
    got <- WS.slurpResults =<< WS.submit client (withPrelude' trav) Nothing
    (map (fmap parseEither) $ V.toList got) `shouldMatchList` expected
  specify "AEdge" $ withClient $ \client -> do
    let lOutV :: AsLabel Text
        lOutV = "outV_name"
        lInV :: AsLabel Text
        lInV = "inV_name"
        lProps = "props"
        lEdge = "edge"
        lProj = "projections"
        kCond :: Key AEdge Text
        kCond = "condition"
        trav = gSelectN lEdge lProj [] $. gAs lProj
               $.
               ( gProject
                 (gByL lOutV (gOutV >>> gValues ["name"]))
                 [ gByL lInV  (gInV  >>> gValues ["name"]),
                   gByL lProps (gValueMap KeysNil)
                 ]
               )
               $. gAs lEdge $. sE' [] $ source "g"
        parseResult pm = do
          edge <- lookupAsM lEdge pm
          pj <- lookupAsM lProj pm
          (,,,) (aeLabel edge)
            <$> (lookupAsM lOutV pj)
            <*> (lookupAsM lInV pj)
            <*> (lookupAsM kCond =<< lookupAsM lProps pj)
        expected = [ ("depends_on", "greskell", "aeson", ">=0.11.2.1"),
                     ("depends_on", "greskell", "text", ">=1.2.2.1"),
                     ("depends_on", "aeson", "text", ">=1.2.3")
                   ]
    got <- traverse parseResult =<< WS.slurpResults =<< WS.submit client (withPrelude' trav) Nothing
    V.toList got `shouldMatchList` expected
  -- let getVP vp = (avpLabel vp, parseEither $ avpValue vp, fmap parseEither $ avpProperties vp)
  let getVP vp = (avpLabel vp, parseEither $ avpValue vp)
  specify "AVertexProperty" $ withClient $ \client -> do
    let trav = gProperties [] $. sV' [] $ source "g"
        expName :: Text -> (Text,Either String Text)
        expName val = ("name", Right val)
        expVer :: Text -> (Text,Either String Text)
        expVer val = ("version", Right val)
        expected = [ expName "greskell",
                     expName "aeson",
                     expName "text",
                     expVer "0.1.1.0", -- "2018-04-08",
                     expVer "1.3.1.1", -- "2018-05-10",
                     expVer "1.2.2.0", -- "2017-09-20",
                     expVer "1.2.3.0", -- "2017-12-27",
                     expVer "1.2.2.0"  -- "2017-12-23"
                   ]
    got <- WS.slurpResults =<< WS.submit client (withPrelude' trav) Nothing
    (map getVP $ V.toList got) `shouldMatchList` expected
  specify "AVertex" $ withClient $ \client -> do
    let trav = sV' [] $ source "g"
        getV v = ( parseEither $ unElementID $ avId v,
                   avLabel v
                 )
        -- sort' = sortBy $ \(k1, v1, _) (k2, v2, _) -> compare (show k1,show v1) (show k2,show v2)
        expV :: Int -> (Either String Int, Text)
        expV vid = (Right vid, "package")
        expected = [ expV 1,
                     expV 2,
                     expV 3
                   ]
    got <- WS.slurpResults =<< WS.submit client (withPrelude' trav) Nothing
    (map getV $ V.toList got) `shouldMatchList` expected
  where
    withPrelude' :: (ToGreskell a) => a -> Greskell (GreskellReturn a)
    withPrelude' = withPrelude prelude
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
    finalize :: GTraversal c s e -> Text
    finalize gt = toGremlin $ iterateTraversal gt
    num :: Integer -> Greskell (ElementID AVertex)
    num = fmap ElementID . gvalueInt
    setName :: Integer -> Greskell Text -> GTraversal SideEffect () AVertex
    setName vid name = gProperty "name" name $. liftWalk $ sV' [num vid] $ source "g"
    dependsOn :: Integer -> Integer -> Greskell Text -> GTraversal SideEffect () AEdge
    dependsOn from_id to_id version_cond =
      gProperty "condition" version_cond
      $. (gAddE' "depends_on" $ gTo (gV' [num to_id]))
      $. liftWalk $ sV' [num from_id] $ source "g"
    addVersion :: Integer -> Greskell Text -> Greskell Text -> [Text]
    addVersion vid ver date =
      [ finalize $ gPropertyV (Just cList) "version" ver ["date" =: date] $. liftWalk $ sV' [num vid] $ source "g"
      ]


spec_as :: SpecWith (String,Int)
spec_as = do
  let start :: GTraversal Transform () Int
      start = unsafeGTraversal "__(1,2,3)"
      mult :: Greskell Int -> Walk Transform Int Int
      mult factor = unsafeWalk "map" ["{ it.get() * " <> toGremlin factor <> " }"]
  specify "gAs and gSelect1" $ withClient $ \client -> do
    let label :: AsLabel Int
        label = AsLabel "a"
    got <- WS.slurpResults =<< WS.submit client (gSelect1 label $. mult 100 $. gAs label  $. start) Nothing
    V.toList got `shouldBe` [1,2,3]
  specify "gAs and gSelectN" $ withClient $ \client -> do
    let lorig, lmul :: AsLabel Int
        lorig = AsLabel "a"
        lmul = AsLabel "b"
        gt = gSelectN lorig lmul [] $. mult 5 $. gAs lmul $. mult 100 $. gAs lorig $. start
    got <- fmap V.toList $ WS.slurpResults =<< WS.submit client gt Nothing
    mapM (lookupAsM lorig) got `shouldReturn` [1,2,3]
    mapM (lookupAsM lmul) got `shouldReturn` [100,200,300]

spec_selectBy :: SpecWith (String,Int)
spec_selectBy = do
  let prelude :: Greskell ()
      prelude = unsafeGreskell $ mconcat $ map (<> "; ")
                [ "graph = org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph.open()",
                  "g = graph.traversal()",
                  "graph.addVertex(id, 1, label, 'person')",
                  "graph.addVertex(id, 2, label, 'person')",
                  "g.V(1).property('name', 'ito').property('age', 23).iterate()",
                  "g.V(2).property('name', 'tanaka').property('age', 18).iterate()",
                  "g.V(1).addE('knows').to(V(2)).iterate()"
                ]
  specify "gAs and gSelectBy1" $ withClient $ \client -> do
    let src :: AsLabel AVertex
        src = AsLabel "s"
        gt = gSelectBy1 src (gBy ("name" :: Key AVertex Text)) $. gAs src $. gFilter (gOut' ["knows"]) $. sV' [] $ source "g"
    got <- fmap V.toList $ WS.slurpResults =<< WS.submit client (withPrelude prelude gt) Nothing
    got `shouldBe` ["ito"]
  specify "gAs and gSelectByN" $ withClient $ \client -> do
    let src, dest :: AsLabel AVertex
        src = AsLabel "s"
        dest = AsLabel "d"
        gt = gSelectByN src dest [] (gBy ("age" :: Key AVertex Int))
             $. gAs dest $. gOut' ["knows"]
             $. gAs src $. gFilter (gOut' ["knows"]) $. sV' [] $ source "g"
    got <- fmap V.toList $ WS.slurpResults =<< WS.submit client (withPrelude prelude gt) Nothing
    map (As.lookup src) got `shouldBe` [Just 23]
    map (As.lookup dest) got `shouldBe` [Just 18]
