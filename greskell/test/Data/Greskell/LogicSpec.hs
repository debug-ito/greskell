module Data.Greskell.LogicSpec
    ( main
    , spec
    ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (forM_)
import           Data.Foldable       (toList)
import           Data.Monoid         ((<>))
import           Data.Traversable    (traverse)
import           Test.Hspec

import           Data.Greskell.Logic (Logic (..), runBool)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let tree_int :: Logic Int
      tree_int =
        ( And
          ( Or (Leaf 10) [Leaf 20, Not (Leaf 30)])
          [ Leaf 40,
            Not
            ( Or (Leaf 50) [And (Or (Leaf 60) []) [], Not (Leaf 70)]
            )
          ]
        )
  describe "Functor" $ do
    specify "fmap on tree" $ do
      fmap (\n -> 30 <= n && n <= 60) tree_int
        `shouldBe`
        ( And
          ( Or (Leaf False) [Leaf False, Not (Leaf True)])
          [ Leaf True,
            Not
            ( Or (Leaf True) [And (Or (Leaf True) []) [], Not (Leaf False)]
            )
          ]
        )
  describe "Applicative" $ do
    specify "<*> should append the right tree to the left tree" $ do
      let tree_right :: Logic Int
          tree_right = And (Leaf 5) [Leaf 3, Or (Leaf 2) [Leaf 4]]
          plusRight n = fmap (+ n) tree_right
      ((+) <$> tree_int <*> tree_right)
        `shouldBe`
        ( And
          ( Or (plusRight 10) [plusRight 20, Not (plusRight 30)])
          [ plusRight 40,
            Not
            ( Or (plusRight 50) [And (Or (plusRight 60) []) [], Not (plusRight 70)]
            )
          ]
        )
  describe "Foldable" $ do
    specify "toList should preserve the look of the structure" $ do
      toList tree_int
        `shouldBe`
        [10, 20, 30, 40, 50, 60, 70]
  describe "Traversable" $ do
    specify "traverse should preserve the structure" $ do
      traverse Just tree_int
        `shouldBe`
        Just tree_int
    specify "traverse should run the action of the applicative" $ do
      traverse (\n -> if n == 20 then Nothing else Just n) tree_int `shouldBe` Nothing
  describe "runBool" $ do
    specify "Leaf" $ do
      runBool (Leaf True) `shouldBe` True
      runBool (Leaf False) `shouldBe` False
    specify "Not" $ do
      runBool (Not $ Leaf True) `shouldBe` False
      runBool (Not $ Leaf False) `shouldBe` True
    describe "And" $ do
      forM_ [True, False] $ \b1 -> do
        specify ("single " <> show b1) $ do
          runBool (And (Leaf b1) []) `shouldBe` b1
        forM_ [True, False] $ \b2 -> do
          specify ("double " <> show (b1, b2)) $ do
            runBool (And (Leaf b1) [Leaf b2]) `shouldBe` b1 && b2
    describe "Or" $ do
      forM_ [True, False] $ \b1 -> do
        specify ("single " <> show b1) $ do
          runBool (Or (Leaf b1) []) `shouldBe` b1
        forM_ [True, False] $ \b2 -> do
          specify ("double " <> show (b1, b2)) $ do
            runBool (Or (Leaf b1) [Leaf b2]) `shouldBe` b1 || b2
