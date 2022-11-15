module ExamplesSpec
    ( main
    , spec
    ) where

import qualified Data.Greskell.GraphSON as GraphSON
import qualified Data.Greskell.Greskell as Greskell

import           Control.Monad          (forM_)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "examples" $ do
  makeSpec "GraphSON" GraphSON.examples
  makeSpec "Greskell" Greskell.examples

makeSpec :: (Show a) => String -> [(a, a)] -> Spec
makeSpec label exs = describe label $ forM_ exs $ \(got, expected) -> specify (show expected) $ show got `shouldBe` show expected
