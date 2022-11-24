{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Main
    ( main
    , spec
    ) where

import           Control.Category                  ((<<<), (>>>))
import qualified Data.Aeson                        as Aeson
import           Data.Either                       (isRight)
import           Data.Foldable                     (toList)
import           Data.HashMap.Strict               (HashMap)
import qualified Data.HashMap.Strict               as HM
import           Data.List                         (sort)
import           Data.Monoid                       (mempty, (<>))
import           Data.Scientific                   (Scientific)
import           Data.Text                         (Text, unpack)
import           Data.Traversable                  (traverse)
import qualified Data.Vector                       as V
import qualified Network.Greskell.WebSocket.Client as WS
import           Test.Hspec

import           Data.Greskell.AsIterator          (AsIterator (IteratorItem))
import           Data.Greskell.AsLabel             (AsLabel (..), lookupAsM)
import qualified Data.Greskell.AsLabel             as As
import           Data.Greskell.Binder              (newBind, runBinder)
import           Data.Greskell.Extra               (gWhenEmptyInput)
import           Data.Greskell.GMap                (GMapEntry, unGMapEntry)
import           Data.Greskell.Graph               (AEdge (..), AProperty (..), AVertex (..),
                                                    AVertexProperty (..), ElementID (..), Key,
                                                    Keys (..), Path (..), T, cList, makePathEntry,
                                                    singletonKeys, tId, tKey, tLabel, tValue, (-:),
                                                    (=:))
import           Data.Greskell.GraphSON            (FromGraphSON, GValue, nonTypedGValue,
                                                    parseEither)
import           Data.Greskell.Gremlin             (Order, P, Predicate (..), cCompare, oDecr,
                                                    oIncr, pAnd, pEq, pGte, pLt, pNot, pTest)
import           Data.Greskell.Greskell            (Greskell, ToGreskell (..), false, gvalueInt,
                                                    list, number, single, toGremlin, toGreskell,
                                                    true, unsafeGreskell, unsafeMethodCall, value)
import           Data.Greskell.GTraversal          (GTraversal, SideEffect, Transform, Walk, gAddE',
                                                    gAddV, gAs, gBy, gBy1, gByL, gChoose3,
                                                    gEmitHead, gFilter, gHas2, gHasLabel, gId,
                                                    gIdentity, gInV, gInV', gIsP, gIsP', gIterate,
                                                    gLabel, gLoops, gMatch, gOrder, gOut', gOutV,
                                                    gOutV', gPath, gPathBy, gProject, gProperties,
                                                    gProperty, gPropertyV, gRepeat, gSelect1,
                                                    gSelectBy1, gSelectByN, gSelectN, gTimes, gTo,
                                                    gUntilTail, gV', gValueMap, gValues, gWhereP1,
                                                    gWhereP2, liftWalk, mPattern, sAddV', sE', sV',
                                                    source, unsafeGTraversal, unsafeWalk, ($.))
import           Data.Greskell.Logic               (Logic (..))
import           Data.Greskell.PMap                (lookupAsM, lookupListAs, pMapToThrow)

import           ServerTest.Common                 (withClient, withEnv)

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
  spec_project
  spec_repeat
  spec_upsert
  spec_path
  spec_where
  spec_match

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
  checkOne (pTest the_p 5)  False
  checkOne (pTest the_p 10) True
  checkOne (pTest the_p 15) True
  checkOne (pTest the_p 20) False
  where
    the_p :: Greskell (P Int)
    the_p = pLt 20 `pAnd` pGte 10

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
  let p :: Greskell (P Scientific)
      p = pNot $ pEq $ number 10
      test v = WS.slurpResults =<< WS.submit client (pTest p $ v) Nothing
  test (number 10) `shouldReturn` V.fromList [False]
  test (number 15) `shouldReturn` V.fromList [True]

withPrelude :: (ToGreskell a) => Greskell () -> a -> Greskell (GreskellReturn a)
withPrelude prelude orig = unsafeGreskell (toGremlin prelude <> toGremlin orig)

statements :: [Text] -> Greskell ()
statements = unsafeGreskell . mconcat . map (<> "; ")

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
        trav = gSelectN lEdge lProj [] $. gAs lProj $.
               ( gProject
                 ( gByL lOutV (gOutV' >>> gValues ["name"]) )
                 [ gByL lInV  (gInV'  >>> gValues ["name"]),
                   gByL lProps (gValueMap KeysNil)
                 ]
               ) $.
               gAs lEdge $. sE' [] $ source "g"
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
  specify "AVertexProperty" $ withClient $ \client -> do
    let lAV = "vertex_property"
        lProps = "props"
        kDate :: Key (AVertexProperty GValue) Text
        kDate = "date"
        trav = gSelectN lAV lProps [] $. gAs lProps $. gValueMap (singletonKeys kDate) $.
               gAs lAV $. gProperties [] $. sV' [] $ source "g"
        parseResult pm = do
          av <- lookupAsM lAV pm
          let label = avpLabel av
              e_val = parseEither $ avpValue av
          m_date <- if label == "version"
                    then fmap Just (lookupAsM kDate =<< lookupAsM lProps pm)
                    else return Nothing
          return (label, e_val, m_date)
        expected :: [(Text, Either String Text, Maybe Text)]
        expected = [ ("name", Right "greskell", Nothing),
                     ("name", Right "aeson", Nothing),
                     ("name", Right "text", Nothing),
                     ("version", Right "0.1.1.0", Just "2018-04-08"),
                     ("version", Right "1.3.1.1", Just "2018-05-10"),
                     ("version", Right "1.2.2.0", Just "2017-09-20"),
                     ("version", Right "1.2.3.0", Just "2017-12-27"),
                     ("version", Right "1.2.2.0", Just "2017-12-23")
                   ]
    got <- traverse parseResult =<< WS.slurpResults =<< WS.submit client (withPrelude' trav) Nothing
    V.toList got `shouldMatchList` expected
  specify "AVertex" $ withClient $ \client -> do
    let lVertex = "vertex"
        kName :: Key AVertex Text
        kName = "name"
        kVer :: Key AVertex Text
        kVer = "version"
        lProps = "props"
        trav = gSelectN lVertex lProps [] $. gAs lProps $. gValueMap (kName -: kVer -: KeysNil) $.
               gAs lVertex $. sV' [] $ source "g"
        parseResult pm = do
          v <- lookupAsM lVertex pm
          let label = avLabel v
              evid = parseEither $ unElementID $ avId v
          props <- lookupAsM lProps pm
          names <- fmap toList $ pMapToThrow $ lookupListAs kName props
          vers <- fmap (sort . toList) $ pMapToThrow $ lookupListAs kVer props
          return (evid, label, names, vers)
        expected :: [(Either String Int, Text, [Text], [Text])]
        expected = [ (Right 1, "package", ["greskell"], ["0.1.1.0"]),
                     (Right 2, "package", ["aeson"], ["1.2.2.0", "1.3.1.1"]),
                     (Right 3, "package", ["text"], ["1.2.2.0", "1.2.3.0"])
                   ]
    got <- traverse parseResult =<< WS.slurpResults =<< WS.submit client (withPrelude' trav) Nothing
    V.toList got `shouldMatchList` expected
  where
    withPrelude' :: (ToGreskell a) => a -> Greskell (GreskellReturn a)
    withPrelude' = withPrelude prelude
    prelude :: Greskell ()
    prelude = statements
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

multiplyWalk :: Greskell Int -> Walk Transform Int Int
multiplyWalk factor = unsafeWalk "map" ["{ it.get() * " <> toGremlin factor <> " }"]

plusWalk :: Greskell Int -> Walk Transform Int Int
plusWalk n = unsafeWalk "map" ["{ it.get() + " <> toGremlin n <> " }"]

squareWalk :: Walk Transform Int Int
squareWalk = unsafeWalk "map" ["{ it.get() * it.get() }"]

appendWalk :: Greskell Text -> Walk Transform Text Text
appendWalk t = unsafeWalk "map" ["{ it.get() + " <> toGremlin t <> " }"]

-- toLowerWalk :: Walk Transform Text Text
-- toLowerWalk = unsafeWalk "map" ["{ it.get().toLowerCase() }"]

lengthWalk :: Walk Transform Text Int
lengthWalk = unsafeWalk "map" ["{ it.get().length() }"]

substrWalk :: Greskell Int -> Greskell Int -> Walk Transform Text Text
substrWalk s e = unsafeWalk "map" ["{ it.get().substring(" <> toGremlin s <> ", " <> toGremlin e <> ") }"]

spec_as :: SpecWith (String,Int)
spec_as = do
  let start :: GTraversal Transform () Int
      start = unsafeGTraversal "__(1,2,3)"
      mult = multiplyWalk
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
      prelude = statements
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

spec_project :: SpecWith (String,Int)
spec_project = do
  let start :: GTraversal Transform () Int
      start = unsafeGTraversal "__(1,2,3)"
  specify "gProject with single item" $ withClient $ \client -> do
    let l2 = "l2"
        trav = gProject (gByL l2 $ multiplyWalk 2) [] $. start
    got <- fmap V.toList $ WS.slurpResults =<< WS.submit client trav Nothing
    got_l2 <- traverse (lookupAsM l2) got
    got_l2 `shouldBe` [2,4,6]
  specify "gProject with two items" $ withClient $ \client -> do
    let l2 = "l2"
        l3 = "l3"
        trav = gProject (gByL l2 $ multiplyWalk 2) [gByL l3 $ multiplyWalk 3] $. start
    got <- fmap V.toList $ WS.slurpResults =<< WS.submit client trav Nothing
    got_l2 <- traverse (lookupAsM l2) got
    got_l2 `shouldBe` [2,4,6]
    got_l3 <- traverse (lookupAsM l3) got
    got_l3 `shouldBe` [3,6,9]
  specify "gProject with more than one items in 'by' traversal" $ withClient $ \client -> do
    let rep_mul3 :: Walk Transform Int Int
        rep_mul3 = unsafeWalk "flatMap" ["{ def a = it.get(); return [a * 3, a * 4, a * 5].iterator() }"]
        lab = "rep_mul3"
        trav = gProject (gByL lab $ rep_mul3) [] $. start
    got_simple <- fmap V.toList $ WS.slurpResults =<< WS.submit client (rep_mul3 $. start) Nothing
    got_simple `shouldBe` [3, 4, 5, 6, 8, 10, 9, 12, 15]
    got <- fmap V.toList $ WS.slurpResults =<< WS.submit client trav Nothing
    got_l <- traverse (lookupAsM lab) got
    got_l `shouldBe` [3, 6, 9]
    -- only the first item from the by-projection traversal.
  specify "gProject with gSelect1 in by-projection" $ withClient $ \client -> do
    let as_orig = "as_orig"
        l_orig = "orig"
        l_mapped = "mapped"
        trav = gProject
               (gByL l_mapped $ multiplyWalk 4)
               [gByL l_orig $ gSelect1 as_orig ] $.
               gAs as_orig $. start
    got <- fmap V.toList $ WS.slurpResults =<< WS.submit client trav Nothing
    traverse (lookupAsM l_mapped) got `shouldReturn` [4, 8, 12]
    traverse (lookupAsM l_orig) got `shouldReturn` [1, 2, 3]

spec_repeat :: SpecWith (String,Int)
spec_repeat = do
  specify "gRepeat and gTimes" $ withClient $ \client -> do
    let start :: GTraversal Transform () Int
        start = unsafeGTraversal "__(1,2,3)"
        trav = gRepeat Nothing (gTimes 3) Nothing (multiplyWalk 2) $. start
    got <- fmap V.toList $ WS.slurpResults =<< WS.submit client trav Nothing
    got `shouldBe` [8, 16, 24]
  specify "gRepeat, gTimes and gEmitHead" $ withClient $ \client -> do
    let start :: GTraversal Transform () Int
        start = unsafeGTraversal "__(1, 10, 100)"
        trav = gRepeat Nothing (gTimes 3) gEmitHead (multiplyWalk 2) $. start
    got <- fmap V.toList $ WS.slurpResults =<< WS.submit client trav Nothing
    sort got `shouldBe` [1, 2, 4, 8, 10, 20, 40, 80, 100, 200, 400, 800]
  specify "gRepeat, gUntilTail and gLoops" $ withClient $ \client -> do
    let start :: GTraversal Transform () Int
        start = unsafeGTraversal "__(1, 10, 100)"
        trav = gRepeat Nothing (gUntilTail $ gIsP (pGte 4) <<< gLoops Nothing) Nothing (multiplyWalk 2) $. start
    got <- fmap V.toList $ WS.slurpResults =<< WS.submit client trav Nothing
    sort got `shouldBe` [16, 160, 1600]

spec_upsert :: SpecWith (String,Int)
spec_upsert = do
  describe "upsert vertex" $ do
    specify "upsert outputs the vertex" $ withClient $ \client -> do
      let body = liftWalk getName $. upsert "foo"
          pre = statements prelude
      got <- WS.slurpResults =<< WS.submit client (withPrelude pre body) Nothing
      V.toList got `shouldBe` ["foo"]
    specify "upsert adds a vertex" $ withClient $ \client -> do
      let pre = statements $ prelude ++
                [ toGremlin (gIterate $ upsert "foo")
                ]
          body = getName $. getAllPersons
      got <- WS.slurpResults =<< WS.submit client (withPrelude pre body) Nothing
      V.toList got `shouldBe` ["foo"]
    specify "upsert adds different vertices" $ withClient $ \client -> do
      let pre = statements $ prelude ++
                [ toGremlin (gIterate $ upsert "foo"),
                  toGremlin (gIterate $ upsert "bar"),
                  toGremlin (gIterate $ upsert "foo"),
                  toGremlin (gIterate $ upsert "buzz"),
                  toGremlin (gIterate $ upsert "bar")
                ]
          body = getName $. getAllPersons
      got <- WS.slurpResults =<< WS.submit client (withPrelude pre body) Nothing
      V.toList got `shouldMatchList` ["foo", "bar", "buzz"]
    specify "upsert returns existing vertex" $ withClient $ \client -> do
      let pre = statements $ prelude ++
                [ toGremlin (gIterate $ upsert "foo"),
                  toGremlin (gIterate $ upsert "foo")
                ]
          body = liftWalk getName $. upsert "foo"
      got <- WS.slurpResults =<< WS.submit client (withPrelude pre body) Nothing
      V.toList got `shouldBe` ["foo"]
  where
    prelude :: [Text]
    prelude =
      [ "graph = org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph.open()",
        "g = graph.traversal()"
      ]
    getAllPersons :: GTraversal Transform () AVertex
    getAllPersons = gHasLabel "person" $. sV' [] $ source "g"
    getPerson :: Greskell Text -> GTraversal Transform () AVertex
    getPerson name = gHas2 "name" name $. getAllPersons
    insert :: Greskell Text -> Walk SideEffect a AVertex
    insert name = gProperty "name" name <<< gAddV "person"
    upsert :: Greskell Text -> GTraversal SideEffect () AVertex
    upsert name = gWhenEmptyInput (insert name) $. liftWalk $ getPerson name
    getName :: Walk Transform AVertex Text
    getName = gValues ["name"]

spec_path :: SpecWith (String,Int)
spec_path = do
  let start :: GTraversal Transform () Int
      start = unsafeGTraversal "__(1,2,3)"
      mult = multiplyWalk
  specify "gPath" $ withClient $ \client -> do
    let g = gPath $. gAs "c" $. mult 10 $. mult 10 $. gAs "b" $. gAs "a" $. start
    got <- fmap V.toList $ WS.slurpResults =<< WS.submit client g Nothing
    let parsed = traverse (traverse parseEither) got
        expected :: [Path Int]
        expected = [ Path
                     [ makePathEntry ["a", "b"] 1,
                       makePathEntry [] 10,
                       makePathEntry ["c"] 100
                     ],
                     Path
                     [ makePathEntry ["a", "b"] 2,
                       makePathEntry [] 20,
                       makePathEntry ["c"] 200
                     ],
                     Path
                     [ makePathEntry ["a", "b"] 3,
                       makePathEntry [] 30,
                       makePathEntry ["c"] 300
                     ]
                   ]
    parsed `shouldBe` Right expected
  specify "gPathBy" $ withClient $ \client -> do
    let g = gPathBy (gBy $ mult 50) [gBy $ mult 800] $. mult 10 $. gAs "b" $. mult 10 $. gAs "a" $. start
    got <- fmap V.toList $ WS.slurpResults =<< WS.submit client g Nothing
    let expected :: [Path Int]
        expected = [ Path
                     [ makePathEntry ["a"] 50,
                       makePathEntry ["b"] 8000,
                       makePathEntry []    5000
                     ],
                     Path
                     [ makePathEntry ["a"] 100,
                       makePathEntry ["b"] 16000,
                       makePathEntry []    10000
                     ],
                     Path
                     [ makePathEntry ["a"] 150,
                       makePathEntry ["b"] 24000,
                       makePathEntry []    15000
                     ]
                   ]
    got `shouldBe` expected

spec_where :: SpecWith (String,Int)
spec_where = do
  let start :: GTraversal Transform () Int
      start = unsafeGTraversal "__(1,2,3)"
  specify "gWhereP1 (without modulation)" $ withClient $ \client -> do
    let g = gWhereP1 (pEq label_s) Nothing $. squareWalk $. gAs label_s $. start
        label_s = "s"
    got <- fmap V.toList $ WS.slurpResults =<< WS.submit client g Nothing
    got `shouldBe` [1]
  specify "gWhereP1 (with modulation)" $ withClient $ \client -> do
    let g = gWhereP1 (pEq label_s) (Just $ gBy $ mapper) $. multiplyWalk 5 $. gAs label_s $. start
        label_s = "s"
        mapper = gChoose3 (gIsP' $ pGte 12)
                 (plusWalk (-12))
                 gIdentity
    got <- fmap V.toList $ WS.slurpResults =<< WS.submit client g Nothing
    got `shouldBe` [15]
  specify "gWhereP2 (without modulation)" $ withClient $ \client -> do
    let g = gWhereP2 label_s (pEq label_m) Nothing $. plusWalk 10 $. gAs label_m $. squareWalk $. gAs label_s $. start
        label_s = "s"
        label_m = "m"
    got <- fmap V.toList $ WS.slurpResults =<< WS.submit client g Nothing
    got `shouldBe` [11]
  specify "gWhereP2 (with modulation)" $ withClient $ \client -> do
    let start_str :: GTraversal Transform () Text
        start_str = unsafeGTraversal "__(\"foo\", \"bar\", \"quux\", \"hoge\")"
        g = gWhereP2 label_s (pEq label_a) (Just $ gBy $ mapper) $. gAs label_a $. appendWalk "FOO" $. gAs label_s $. start_str
        label_s = "s"
        label_a = "a"
        mapper = gChoose3 (gIsP (pGte 6) <<< lengthWalk)
                 (substrWalk 0 3)
                 gIdentity
    got <- fmap V.toList $ WS.slurpResults =<< WS.submit client g Nothing
    got `shouldBe` ["fooFOO", "barFOO"]

spec_match :: SpecWith (String,Int)
spec_match = do
  specify "gMatch" $ withClient $ \client -> do
    let start_str :: GTraversal Transform () Text
        start_str = unsafeGTraversal "__(\"foo\", \"hoge\", \"bar\", \"quux\")"
        g = gSelectN label_s label_ss [label_len, label_dlen] $. gMatch pat $. start_str
        pat = And
              ( mPattern label_s (gAs label_len <<< lengthWalk) )
              [ mPattern label_s (gAs label_ss <<< appendWalk "HOGE"),
                mPattern label_len (gAs label_dlen <<< squareWalk),
                mPattern label_ss (gWhereP1 (pLt label_dlen) Nothing <<< plusWalk 5 <<< lengthWalk)
              ]
        label_s = "s"
        label_ss = "ss"
        label_len = "len"
        label_dlen = "dlen"
        extract sm = (,,,)
                     <$> lookupAsM label_s sm
                     <*> lookupAsM label_ss sm
                     <*> lookupAsM label_len sm
                     <*> lookupAsM label_dlen sm
    got <- traverse extract =<< (fmap V.toList $ WS.slurpResults =<< WS.submit client g Nothing)
    got `shouldBe` [ ("hoge", "hogeHOGE", 4, 16),
                     ("quux", "quuxHOGE", 4, 16)
                   ]
