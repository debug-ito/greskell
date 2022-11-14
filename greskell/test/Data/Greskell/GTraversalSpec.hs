{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.GTraversalSpec (main,spec) where

import Control.Category ((>>>), (<<<))
import Control.Monad (forM_)
import Data.Aeson (ToJSON(..), Value(Number))
import Data.Function ((&))
import Data.Text (Text, unpack)
import System.IO (stderr, hPutStrLn)

import Test.Hspec

import Data.Greskell.AsLabel (AsLabel)
import Data.Greskell.Gremlin
  ( oIncr, oDecr, oShuffle,
    pEq, pNeq, pInside, pGte
  )
import Data.Greskell.Graph
  ( Element, ElementID(..), AVertex,
    Key, key,
    tLabel, tId
  )
import Data.Greskell.GraphSON (nonTypedGValue, GValueBody(..))
import Data.Greskell.Greskell
  ( toGremlin, Greskell, gvalueInt)
import Data.Greskell.GTraversal
  ( Walk, Transform, Filter,
    source, (&.), ($.), sV', sE',
    gHas1, gHas2, gHas2', gHas2P, gHasLabelP, gHasIdP, gIs, gIs',
    gOut', gRange, gValues, gNot, gIn',
    gOrder,
    gProperties, gHasKeyP, gHasValueP,
    ByComparator(..), gBy2, gBy1, gBy,
    gRepeat, gTimes, gUntilHead, gUntilTail,
    gEmitHead, gEmitTail, gEmitHeadT, gEmitTailT,
    gLoops,
    gWhereP1, gAs, gLabel, gWhereP2,
    gMatch, mPattern, testExamples_GTraversal
  )
import Data.Greskell.Logic (Logic(..))


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_GraphTraversalSource
  spec_order_by
  spec_compose_steps
  spec_has
  spec_repeat
  spec_where
  spec_match
  testExamples_spec

spec_GraphTraversalSource :: Spec
spec_GraphTraversalSource = describe "GraphTraversalSource" $ do
  specify "g.V()" $ do
    (toGremlin $ sV' [] $ source "g") `shouldBe` ("g.V()")
  specify "g.V(1,2,3)" $ do
    let ids = map (fmap ElementID . gvalueInt) $ ([1,2,3] :: [Int])
    (toGremlin $ sV' ids $ source "g") `shouldBe` ("g.V(1,2,3)")

spec_order_by :: Spec
spec_order_by = describe "gOrder" $ do
  let gv = source "g" & sV' []
  specify "no arg" $ do
    toGremlin (gv &. gOrder []) `shouldBe` "g.V().order()"
  specify "empty projection" $ do
    -- This case is relatively rare (I think), so the API is not so convenient for now.
    toGremlin (gv &. gOrder [ByComparatorComp oIncr]) `shouldBe` "g.V().order().by(Order.incr)"
  specify "traversal projection" $ do
    toGremlin (gv &. gOrder [gBy2 (gOut' ["foo"] >>> gIn' ["bar"]) oShuffle])
      `shouldBe` "g.V().order().by(__.out(\"foo\").in(\"bar\"),Order.shuffle)"
  specify "value projection" $ do
    let nameKey :: Key e Text
        nameKey = "name"
    toGremlin (gv &. gOrder [gBy2 nameKey oDecr]) `shouldBe` "g.V().order().by(\"name\",Order.decr)"
  specify "T token projection" $ do
    toGremlin (gv &. gOrder [gBy2 tLabel oIncr]) `shouldBe` "g.V().order().by(T.label,Order.incr)"
  specify "multiple .by steps of different comparison types" $ do
    let ageKey :: Key e Int
        ageKey = "age"
    toGremlin (gv &. gOrder [gBy2 ageKey oDecr, gBy2 tId oDecr, gBy1 (gOut' ["foo"])])
      `shouldBe` "g.V().order().by(\"age\",Order.decr).by(T.id,Order.decr).by(__.out(\"foo\"))"
  specify "gBy1" $ do
    toGremlin (gv &. gOrder [gBy1 (key "name")]) `shouldBe` "g.V().order().by(\"name\")"
  specify "IsString instance of ByComparator" $ do
    toGremlin (gv &. gOrder ["age"]) `shouldBe` "g.V().order().by(\"age\")"
  specify "gBy" $ do
    toGremlin (gv &. gOrder [ByComparatorProj $ gBy tLabel]) `shouldBe` "g.V().order().by(T.label)"
  specify "IsString instance of ByProjection" $ do
    toGremlin (gv &. gOrder [ByComparatorProjComp ("name") oIncr])
      `shouldBe` "g.V().order().by(\"name\",Order.incr)"

spec_compose_steps :: Spec
spec_compose_steps = describe "DSL to compose steps" $ do
  specify "(&) and (&.)" $ do
    let gt = source "g" & sV' [] &. gHas2P ("x" :: Key e Int) (pEq 100) &. gOut' [] &. gRange 0 100
    toGremlin gt `shouldBe` "g.V().has(\"x\",P.eq(100)).out().range(0,100)"
  specify "(&) and (&.) and (>>>)" $ do
    let vid = fmap ElementID $ gvalueInt (200 :: Int)
        gt = source "g" & sV' [vid] &. (gOut' [] >>> gOut' ["friends_to"] >>> gValues ["name"])
    toGremlin gt `shouldBe` "g.V(200).out().out(\"friends_to\").values(\"name\")"
  specify "($) and ($.)" $ do
    let gt = gRange 20 30 $. gNot (gOut' ["friends_to"]) $. sV' [] $ source "g"
    toGremlin gt `shouldBe` "g.V().not(__.out(\"friends_to\")).range(20,30)"
  specify "($) and ($.) and (<<<)" $ do
    let gt = gHas2P (key "name" :: Key e Text) (pEq "hoge") <<< gIn' ["foo", "bar"] <<< gIn' [] $. sV' [] $ source "g"
    toGremlin gt `shouldBe` "g.V().in().in(\"foo\",\"bar\").has(\"name\",P.eq(\"hoge\"))"

spec_has :: Spec
spec_has = do
  describe "gHas1" $ do
    specify "IsString Key" $ do
      toGremlin (source "g" & sV' [] &. gHas1 "foo") `shouldBe` "g.V().has(\"foo\")"
  describe "gHas2" $ do
    specify "simple equality" $ do
      toGremlin (source "g" & sV' [] &. gHas2 "name" ("hoge" :: Greskell Text))
        `shouldBe` "g.V().has(\"name\",\"hoge\")"
  describe "gHas2P" $ do
    specify "IsString Key and P" $ do
      toGremlin (source "g" & sV' [] &. gHas2P ("name" :: Key e Text) (pNeq "hoge"))
        `shouldBe` "g.V().has(\"name\",P.neq(\"hoge\"))"
  describe "gHasLabelP" $ do
    specify "P" $ do
      toGremlin (source "g" & sE' [] &. gHasLabelP (pNeq "friends_to"))
        `shouldBe` "g.E().hasLabel(P.neq(\"friends_to\"))"
  describe "gHasIdP" $ do
    specify "P" $ do
      let toID :: Int -> Greskell (ElementID AVertex)
          toID = fmap ElementID . gvalueInt
      toGremlin (source "g" & sV' [] &. gHasIdP (pInside (toID 10) (toID 20)))
        `shouldBe` "g.V().hasId(P.inside(10,20))"
  describe "gHasKeyP, gProperties" $ do
    specify "P" $ do
      toGremlin (source "g" & sV' [] &. gProperties [] &. gHasKeyP (pEq "hoge"))
        `shouldBe` "g.V().properties().hasKey(P.eq(\"hoge\"))"
  describe "gHasValueP, gProperties" $ do
    specify "P" $ do
      toGremlin (source "g" & sV' [] &. gProperties ["age" :: Key e Int] &. gHasValueP (pGte 20))
        `shouldBe` "g.V().properties(\"age\").hasValue(P.gte(20))"

spec_repeat :: Spec
spec_repeat = do
  let hasName :: Greskell Text -> Walk Filter AVertex AVertex
      hasName v  = gHas2 keyName v
      keyName :: Key AVertex Text
      keyName = "name"
  describe "gRepeat" $ do
    specify "no modulation" $ do
      toGremlin (source "g" & sV' [] &. gRepeat Nothing Nothing Nothing (gOut' []))
        `shouldBe` "g.V().repeat(__.out())"
    specify "gTimes and gEmitHead" $ do
      toGremlin (source "g" & sV' [] &. gRepeat Nothing (gTimes 3) gEmitHead (gOut' []))
        `shouldBe` "g.V().times(3).emit().repeat(__.out())"
    specify "gUntilHead and gEmitTail" $ do
      toGremlin (source "g" & sV' [] &. gRepeat Nothing (gUntilHead $ hasName "foo") gEmitTail (gOut' []))
        `shouldBe` "g.V().until(__.has(\"name\",\"foo\")).repeat(__.out()).emit()"
    specify "gUntilTail and gEmitHeadT" $ do
      toGremlin (source "g" & sV' [] &. gRepeat Nothing (gUntilTail $ hasName "foo") (gEmitHeadT $ hasName "bar") (gOut' []))
        `shouldBe` "g.V().emit(__.has(\"name\",\"bar\")).repeat(__.out()).until(__.has(\"name\",\"foo\"))"
    specify "gUntilTail and gEmitTailT" $ do
      toGremlin (source "g" & sV' [] &. gRepeat Nothing (gUntilTail $ hasName "foo") (gEmitTailT $ hasName "bar") (gOut' []))
        `shouldBe` "g.V().repeat(__.out()).until(__.has(\"name\",\"foo\")).emit(__.has(\"name\",\"bar\"))"
    specify "gLoops without label" $ do
      toGremlin (source "g" & sV' [] &. gRepeat Nothing (gUntilTail $ gLoops Nothing >>> gIs 5) Nothing (gOut' []))
        `shouldBe` "g.V().repeat(__.out()).until(__.loops().is(5))"
    specify "gLoops with label" $ do
      let loop_label = "LP"
      toGremlin (source "g" & sV' [] &. gRepeat (Just loop_label) (gUntilTail $ gLoops (Just loop_label) >>> gIs 5) Nothing (gOut' []))
        `shouldBe` "g.V().repeat(\"LP\",__.out()).until(__.loops(\"LP\").is(5))"

spec_where :: Spec
spec_where = do
  describe "gWhereP1" $ do
    specify "no modulation" $ do
      let la :: AsLabel AVertex
          la = "a"
      toGremlin (source "g" & sV' [] &. gAs la &. gOut' [] &. gWhereP1 (pEq la) Nothing)
        `shouldBe` "g.V().as(\"a\").out().where(P.eq(\"a\"))"
    specify "with modulation (by traversal)" $ do
      let la :: AsLabel AVertex
          la = "a"
      toGremlin (source "g" & sV' [] &. gAs la &. gOut' [] &. gWhereP1 (pGte la) (Just $ gBy gLabel))
        `shouldBe` "g.V().as(\"a\").out().where(P.gte(\"a\")).by(__.label())"
  describe "gWhereP2" $ do
    specify "no modulation" $ do
      let la = ("a" :: AsLabel AVertex)
          lb = ("b" :: AsLabel AVertex)
          age = ("age" :: Key AVertex Int)
      toGremlin (source "g" & sV' [] &. gAs la &. gOut' [] &. gAs lb &. gValues [age] &. gWhereP2 la (pEq lb) Nothing)
        `shouldBe` "g.V().as(\"a\").out().as(\"b\").values(\"age\").where(\"a\",P.eq(\"b\"))"
    specify "with modulation" $ do
      let la = ("a" :: AsLabel AVertex)
          lb = ("b" :: AsLabel AVertex)
          age = ("age" :: Key AVertex Int)
          name = ("name" :: Key AVertex Text)
      toGremlin (source "g" & sV' [] &. gAs la &. gOut' [] &. gAs lb &. gValues [name] &. gWhereP2 la (pGte lb) (Just $ gBy age))
        `shouldBe` "g.V().as(\"a\").out().as(\"b\").values(\"name\").where(\"a\",P.gte(\"b\")).by(\"age\")"

spec_match :: Spec
spec_match = do
  describe "gMatch" $ do
    specify "top-level Leaf" $ do
      let pat = mPattern label (gOut' [])
          label = ("a" :: AsLabel AVertex)
      toGremlin (source "g" & sV' [] &. gMatch pat)
        `shouldBe` "g.V().match(__.as(\"a\").out())"
    specify "top-level And" $ do
      let pat = And
                ( mPattern label_a (gOut' [] >>> gAs label_b) )
                [ mPattern label_a (gHas2' the_key "foobar")
                ]
          label_a = ("a" :: AsLabel AVertex)
          label_b = "b"
          the_key = ("k" :: Key AVertex Text)
      toGremlin (source "g" & sV' [] &. gMatch pat)
        `shouldBe` "g.V().match(__.as(\"a\").out().as(\"b\"),__.as(\"a\").has(\"k\",\"foobar\"))"
    specify "top-level Or" $ do
      let pat = Or
                ( mPattern label (gHas2' key1 "foobar") )
                [ mPattern label (gHas2' key2 100)
                ]
          label = ("a" :: AsLabel AVertex)
          key1 = ("k1" :: Key AVertex Text)
          key2 = ("k2" :: Key AVertex Int)
      toGremlin (source "g" & sV' [] &. gMatch pat)
        `shouldBe` "g.V().match(__.or(__.as(\"a\").has(\"k1\",\"foobar\"),__.as(\"a\").has(\"k2\",100)))"
    specify "top-level Not" $ do
      let pat = Not ( mPattern label (gHas2' k "quux") )
          label = ("a" :: AsLabel AVertex)
          k = ("k" :: Key AVertex Text)
      toGremlin (source "g" & sV' [] &. gMatch pat)
        `shouldBe` "g.V().match(__.not(__.as(\"a\").has(\"k\",\"quux\")))"
    specify "heterogeneous patterns, nested And" $ do
      let pat = Or
                ( And
                  ( mPattern label_a (gOut' [] >>> gAs label_b) )
                  [ mPattern label_b (gValues [key_age] >>> gAs label_c),
                    mPattern label_c (gIs' 30)
                  ]
                )
                [ And
                  ( mPattern label_a (gValues [key_age] >>> gAs label_c) )
                  [ mPattern label_a (gValues [key_name] >>> gAs label_d),
                    Not ( mPattern label_d (gIs' "toshio") )
                  ]
                ]
          label_a = ("a" :: AsLabel AVertex)
          label_b = "b"
          label_c = "c"
          label_d = "d"
          key_age = ("age" :: Key AVertex Int)
          key_name = ("name" :: Key AVertex Text)
      toGremlin (source "g" & sV' [] &. gMatch pat)
        `shouldBe` "g.V().match(__.or(__.and(__.as(\"a\").out().as(\"b\"),__.as(\"b\").values(\"age\").as(\"c\"),__.as(\"c\").is(30)),__.and(__.as(\"a\").values(\"age\").as(\"c\"),__.as(\"a\").values(\"name\").as(\"d\"),__.not(__.as(\"d\").is(\"toshio\")))))"
    specify "history labels in pattern" $ do
      let pat = mPattern ext_label (gIn' [] >>> gHas2 the_key "foo")
          ext_label = "e"
          the_key = ("k" :: Key AVertex Text)
      toGremlin (source "g" & sV' [] &. gAs ext_label &. gOut' [] &. gMatch pat)
        `shouldBe` "g.V().as(\"e\").out().match(__.as(\"e\").in().has(\"k\",\"foo\"))"

testExamples_spec :: Spec
testExamples_spec = do
  describe "testExamples" $ do
    forM_ testExamples_GTraversal $ \(got, expected) -> do
      specify (unpack expected) $ do
        got `shouldBe` expected
