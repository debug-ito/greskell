module Data.Greskell.GraphSONSpec (main,spec) where

import Data.Aeson.Types (parseEither, Value(..), Parser)
import Data.List (isInfixOf)
import Data.Int (Int32)
import Test.Hspec

import Data.Greskell.GraphSON (GraphSON, parseTypedGraphSON)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  parseTypedGraphSON_spec

parseTypedGraphSON_spec :: Spec
parseTypedGraphSON_spec = describe "parseTypedGraphSON" $ do
  let parse = parseEither parseTypedGraphSON
  describe "Int32" $ do
    let expected :: Int32
        expected = 255
    specify "valid bare value" $ do
      let (Left got) = parse (Number 255) :: Either String (GraphSON Int32)
      got `shouldSatisfy` (`isInfixOf` "Not a valid typed JSON")
      
