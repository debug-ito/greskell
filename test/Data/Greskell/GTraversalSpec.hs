{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.GTraversalSpec (main,spec) where

import Data.Either (isRight)
import Language.Haskell.Interpreter
  ( loadModules, OptionVal((:=)), set, searchPath,
    setTopLevelModules, runInterpreter, InterpreterError,
    typeOf
  )
import System.IO (stderr, hPutStrLn)
import Test.Hspec

import Data.Greskell.Greskell (runGreskell', raw)
import Data.Greskell.GTraversal
  ( source, vertices
  )


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_StepType_classes
  spec_GTraversalSource

spec_StepType_classes :: Spec
spec_StepType_classes = do
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

checkStepTypeRelation :: (String -> String -> String) -> String -> String -> Bool -> Spec
checkStepTypeRelation makeCode child parent expect_ok = specify label $ doCheck
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
checkSplitCompatible = checkStepTypeRelation makeCode
  where
    makeCode child parent =
      "let f :: Step " ++ child ++ " s s -> Step " ++ parent ++ " s s; "
      ++ "f = gFilter; "
      ++ "child :: Step " ++ child ++ " s s; "
      ++ "child = undefined; "
      ++ "in f child"

checkLiftCompatible :: String -> String -> Bool -> Spec
checkLiftCompatible = checkStepTypeRelation makeCode
  where
    makeCode child parent =
      "let f :: Step " ++ child ++ " s e -> Step " ++ parent ++ " s e; "
      ++ "f = liftType; "
      ++ "in f"

spec_GTraversalSource :: Spec
spec_GTraversalSource = describe "GTraversalSource" $ do
  specify "g.V()" $ do
    (runGreskell' $ vertices [] $ source "g") `shouldBe` ("g.V()")
  specify "g.V(1,2,3)" $ do
    (runGreskell' $ vertices (map raw ["1","2","3"]) $ source "g") `shouldBe` ("g.V(1,2,3)")
