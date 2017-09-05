module Data.Greskell.GTraversalSpec (main,spec) where

import Data.Either (isRight)
import Language.Haskell.Interpreter
  ( loadModules, OptionVal((:=)), set, searchPath,
    setTopLevelModules, runInterpreter, InterpreterError,
    typeOf
  )
import System.IO (stderr, hPutStrLn)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Logic typeclass" $ do
    let c = checkLogicCompatible
    c "Filter" "Filter" True
    c "Filter" "Transform" True
    c "Filter" "(SideEffect Filter)" True
    c "Filter" "(SideEffect Transform)" True
    c "Transform" "Filter" True
    c "Transform" "Transform" True
    c "Transform" "(SideEffect Filter)" True
    c "Transform" "(SideEffect Transform)" True
    c "(SideEffect Filter)" "Filter" False
    c "(SideEffect Filter)" "Transform" False
    c "(SideEffect Filter)" "(SideEffect Filter)" True
    c "(SideEffect Filter)" "(SideEffect Transform)" True
    c "(SideEffect Transform)" "Filter" False
    c "(SideEffect Transform)" "Transform" False
    c "(SideEffect Transform)" "(SideEffect Filter)" True
    c "(SideEffect Transform)" "(SideEffect Transform)" True
  describe "Lift typeclass" $ do
    let c = checkLiftCompatible
    c "Filter" "Filter" True
    c "Filter" "Transform" True
    c "Filter" "(SideEffect Filter)" True
    c "Filter" "(SideEffect Transform)" True
    c "Transform" "Filter" False
    c "Transform" "Transform" True
    c "Transform" "(SideEffect Filter)" False
    c "Transform" "(SideEffect Transform)" True
    c "(SideEffect Filter)" "Filter" False
    c "(SideEffect Filter)" "Transform" False
    c "(SideEffect Filter)" "(SideEffect Filter)" True
    c "(SideEffect Filter)" "(SideEffect Transform)" True
    c "(SideEffect Transform)" "Filter" False
    c "(SideEffect Transform)" "Transform" False
    c "(SideEffect Transform)" "(SideEffect Filter)" False
    c "(SideEffect Transform)" "(SideEffect Transform)" True
  

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

checkLogicCompatible :: String -> String -> Bool -> Spec
checkLogicCompatible = checkStepTypeRelation makeCode
  where
    makeCode child parent =
      "let f :: GStep " ++ child ++ " s s -> GStep " ++ parent ++ " s s; "
      ++ "f = gFilter; "
      ++ "child :: GStep " ++ child ++ " s s; "
      ++ "child = undefined; "
      ++ "in f child"

checkLiftCompatible :: String -> String -> Bool -> Spec
checkLiftCompatible = checkStepTypeRelation makeCode
  where
    makeCode child parent =
      "let f :: GStep " ++ child ++ " s e -> GStep " ++ parent ++ " s e; "
      ++ "f = liftType; "
      ++ "in f"
