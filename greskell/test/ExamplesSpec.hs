module ExamplesSpec
    ( main
    , spec
    ) where

import qualified Data.Greskell.Graph      as Graph
import qualified Data.Greskell.Gremlin    as Gremlin
import qualified Data.Greskell.GTraversal as GTraversal

import           Control.Monad            (forM_)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "examples" $ do
  makeSpec "Graph" Graph.examples
  makeSpec "Gremlin" Gremlin.examples
  makeSpec "GTraversal" GTraversal.examples

makeSpec :: (Show a) => String -> [(a, a)] -> Spec
makeSpec label exs = describe label $ forM_ exs $ \(got, expected) -> specify (show expected) $ show got `shouldBe` show expected
