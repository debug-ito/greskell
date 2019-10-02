module Main (main,spec) where

import Data.Either (isRight)
import Language.Haskell.Interpreter
  ( loadModules, OptionVal((:=)), set, searchPath,
    setTopLevelModules, runInterpreter, InterpreterError,
    typeOf
  )
import System.IO (hPutStrLn, stderr)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = spec_WalkType_classes

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
      ++ "f = liftWalk; "
      ++ "in f"
