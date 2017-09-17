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

checkLogicCompatible :: String -> String -> Bool -> Spec
checkLogicCompatible = checkStepTypeRelation makeCode
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
