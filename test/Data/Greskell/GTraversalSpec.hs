{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.GTraversalSpec (main,spec) where

import Control.Category ((>>>), (<<<))
import Data.Either (isRight)
import Data.Function ((&))
import Language.Haskell.Interpreter
  ( loadModules, OptionVal((:=)), set, searchPath,
    setTopLevelModules, runInterpreter, InterpreterError,
    typeOf
  )
import System.IO (stderr, hPutStrLn)

import Test.Hspec

-- import Data.Greskell.Greskell (runGreskell', raw, rawS)
-- import Data.Greskell.GTraversal
--   ( source, vertices, (&.), ($.),
--     gHas', gOut, gRange, gValues, gNot, gIn
--   )


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_WalkType_classes
  spec_GTraversalSource
  spec_compose_steps

spec_WalkType_classes :: Spec
spec_WalkType_classes = do
  describe "Split typeclass" $ do
    let c = checkSplitCompatible
    c "Filter" "Filter" True
    c "Filter" "Transform" True
    c "Filter" "SideEffect" True
    c "Transform" "Filter" True
    c "Transform" "Transform" True
    c "Transform" "SideEffect" True
    c "SideEffect" "Filter" False
    c "SideEffect" "Transform" False
    c "SideEffect" "SideEffect" True
  describe "Lift typeclass" $ do
    let c = checkLiftCompatible
    c "Filter" "Filter" True
    c "Filter" "Transform" True
    c "Filter" "SideEffect" True
    c "Transform" "Filter" False
    c "Transform" "Transform" True
    c "Transform" "SideEffect" True
    c "SideEffect" "Filter" False
    c "SideEffect" "Transform" False
    c "SideEffect" "SideEffect" True
  
toErrString :: Either InterpreterError a -> Either String a
toErrString (Right a) = Right a
toErrString (Left e) = Left $ show e

checkWalkTypeRelation :: (String -> String -> String) -> String -> String -> Bool -> Spec
checkWalkTypeRelation makeCode child parent expect_ok = specify label $ doCheck
  where
    label = child ++ " -> " ++ parent
    doCheck = do
      result <- fmap toErrString $ runInterpreter compiledParent
      -- hPutStrLn stderr ("## " ++ label ++ ": " ++ show result)
      isRight result `shouldBe` expect_ok
    compiledParent = do
      set [searchPath := ["src"]]
      loadModules ["src/Data/Greskell/GTraversal.hs"]
      setTopLevelModules ["Data.Greskell.GTraversal"]
      typeOf $ makeCode child parent

checkSplitCompatible :: String -> String -> Bool -> Spec
checkSplitCompatible = checkWalkTypeRelation makeCode
  where
    makeCode child parent =
      "let f :: Walk " ++ child ++ " s s -> Walk " ++ parent ++ " s s; "
      ++ "f = gFilter; "
      ++ "child :: Walk " ++ child ++ " s s; "
      ++ "child = undefined; "
      ++ "in f child"

checkLiftCompatible :: String -> String -> Bool -> Spec
checkLiftCompatible = checkWalkTypeRelation makeCode
  where
    makeCode child parent =
      "let f :: Walk " ++ child ++ " s e -> Walk " ++ parent ++ " s e; "
      ++ "f = liftType; "
      ++ "in f"

spec_GTraversalSource :: Spec
spec_GTraversalSource = describe "GTraversalSource" $ do
  specify "todo" $ True `shouldBe` False
--   specify "g.V()" $ do
--     (runGreskell' $ vertices [] $ source "g") `shouldBe` ("g.V()")
--   specify "g.V(1,2,3)" $ do
--     let ids = [1,2,3] :: [Int]
--     (runGreskell' $ vertices (map rawS ids) $ source "g") `shouldBe` ("g.V(1,2,3)")

spec_compose_steps :: Spec
spec_compose_steps = describe "DSL to compose steps" $ do
  specify "todo" $ True `shouldBe` False
--   specify "(&) and (&.)" $ do
--     let gt = source "g" & vertices [] &. gHas' "x" (raw "100") &. gOut [] &. gRange (raw "0") (raw "100")
--     runGreskell' gt `shouldBe` "g.V().has(\"x\",100).out().range(0,100)"
--   specify "(&) and (&.) and (>>>)" $ do
--     let gt = source "g" & vertices [raw "200"] &. (gOut [] >>> gOut ["friends_to"] >>> gValues ["name"])
--     runGreskell' gt `shouldBe` "g.V(200).out().out(\"friends_to\").values(\"name\")"
--   specify "($) and ($.)" $ do
--     let gt = gRange (raw "20") (raw "30") $. gNot (gOut ["friends_to"]) $. vertices [] $ source "g"
--     runGreskell' gt `shouldBe` "g.V().not(__.out(\"friends_to\")).range(20,30)"
--   specify "($) and ($.) and (<<<)" $ do
--     let gt = gHas' "name" "hoge" <<< gIn ["foo", "bar"] <<< gIn [] $. vertices [] $ source "g"
--     runGreskell' gt `shouldBe` "g.V().in().in(\"foo\",\"bar\").has(\"name\",\"hoge\")"
