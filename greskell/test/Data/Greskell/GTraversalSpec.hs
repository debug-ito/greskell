{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.GTraversalSpec (main,spec) where

import Control.Category ((>>>), (<<<))
import Data.Aeson (ToJSON(..), Value(Number))
import Data.Function ((&))
import Data.Text (Text)
import System.IO (stderr, hPutStrLn)

import Test.Hspec

import Data.Greskell.Gremlin
  ( oIncr, oDecr, oShuffle,
    pEq, pNeq, pInside, pGte
  )
import Data.Greskell.Graph
  ( Element,
    Key, key,
    tLabel, tId
  )
import Data.Greskell.GraphSON (nonTypedGValue, GValueBody(..))
import Data.Greskell.Greskell
  ( toGremlin, Greskell, gvalueInt)
import Data.Greskell.GTraversal
  ( Walk, Transform,
    source, (&.), ($.), sV', sE',
    gHas1, gHas2, gHas2P, gHasLabelP, gHasIdP,
    gOut', gRange, gValues, gNot, gIn',
    gOrder,
    gProperties, gHasKeyP, gHasValueP,
    ByComparator(..), gBy2, gBy1, gBy
  )


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_GraphTraversalSource
  spec_order_by
  spec_compose_steps
  spec_has


spec_GraphTraversalSource :: Spec
spec_GraphTraversalSource = describe "GraphTraversalSource" $ do
  specify "g.V()" $ do
    (toGremlin $ sV' [] $ source "g") `shouldBe` ("g.V()")
  specify "g.V(1,2,3)" $ do
    let ids = map gvalueInt $ ([1,2,3] :: [Int])
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
    let gt = source "g" & sV' [gvalueInt (200 :: Int)] &. (gOut' [] >>> gOut' ["friends_to"] >>> gValues ["name"])
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
      toGremlin (source "g" & sV' [] &. gHasIdP (pInside (gvalueInt (10 :: Int)) (gvalueInt (20 :: Int))))
        `shouldBe` "g.V().hasId(P.inside(10,20))"
  describe "gHasKeyP, gProperties" $ do
    specify "P" $ do
      toGremlin (source "g" & sV' [] &. gProperties [] &. gHasKeyP (pEq "hoge"))
        `shouldBe` "g.V().properties().hasKey(P.eq(\"hoge\"))"
  describe "gHasValueP, gProperties" $ do
    specify "P" $ do
      toGremlin (source "g" & sV' [] &. gProperties ["age" :: Key e Int] &. gHasValueP (pGte 20))
        `shouldBe` "g.V().properties(\"age\").hasValue(P.gte(20))"
