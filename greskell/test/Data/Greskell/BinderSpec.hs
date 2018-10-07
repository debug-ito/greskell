module Data.Greskell.BinderSpec (main,spec) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_)
import Data.Aeson (toJSON)
import qualified Data.HashMap.Strict as HM
import Data.Text (unpack)
import Test.Hspec

import Data.Greskell.AsLabel (AsLabel)
import Data.Greskell.Greskell (toGremlin, unsafeGreskell, Greskell)
import Data.Greskell.Binder (Binder, newBind, runBinder, newAsLabel)

main :: IO ()
main = hspec spec

shouldBeVariable :: Greskell a -> IO ()
shouldBeVariable got_greskell =
  case unpack $ toGremlin got_greskell of
   [] -> expectationFailure "Expect a Gremlin variable, but got empty script."
   (h : rest) -> do
     h `shouldSatisfy` (`elem` variableHeads)
     forM_ rest (`shouldSatisfy` (`elem` variableRests))
  where
    variableHeads = '_' : (['a' .. 'z'] ++ ['A' .. 'Z'])
    variableRests = variableHeads ++ ['0' .. '9']

spec :: Spec
spec = describe "Binder" $ do
  it "should keep bound values" $ do
    let b = do
          v1 <- newBind (100 :: Int)
          v2 <- newBind "hogehoge"
          return (v1, v2)
        ((got_v1, got_v2), got_bind) = runBinder b
    toGremlin got_v1 `shouldNotBe` toGremlin got_v2
    shouldBeVariable got_v1
    shouldBeVariable got_v2
    got_bind `shouldBe` HM.fromList [ (toGremlin got_v1, toJSON (100 :: Int)),
                                      (toGremlin got_v2, toJSON "hogehoge")
                                    ]
  it "should compose and produce new variables" $ do
    let b = newBind "foobar"
        ((got_v1, got_v2), got_bind) = runBinder $ ((,) <$> b <*> b)
    toGremlin got_v1 `shouldNotBe` toGremlin got_v2
    shouldBeVariable got_v1
    shouldBeVariable got_v2
    got_bind `shouldBe` HM.fromList [ (toGremlin got_v1, toJSON "foobar"),
                                      (toGremlin got_v2, toJSON "foobar")
                                    ]
  it "should also be able to produce AsLabels" $ do
    let newIntLabel :: Binder (AsLabel Int)
        newIntLabel = newAsLabel
        newVar = newBind "foobar"
        ((got_v1, got_l1, got_v2, got_l2), _) =
          runBinder $ ((,,,) <$> newVar <*> newIntLabel <*> newVar <*> newIntLabel)
    shouldBeVariable got_v1
    shouldBeVariable got_v2
    toGremlin got_v1 `shouldNotBe` toGremlin got_v2
    got_l1 `shouldNotBe` got_l2
