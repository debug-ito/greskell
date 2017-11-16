module Data.Greskell.BinderSpec (main,spec) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (toJSON)
import qualified Data.HashMap.Strict as HM
import Test.Hspec

import Data.Greskell.Greskell (toPlaceHolderVariable, toGremlin, unsafeGreskell)
import Data.Greskell.Binder (newBind, runBinder)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Binder" $ do
  it "should keep bound values" $ do
    let b = do
          v1 <- newBind (100 :: Int)
          v2 <- newBind "hogehoge"
          return (v1, v2)
        ((got_v1, got_v2), got_bind) = runBinder b
    got_v1 `shouldBe` (unsafeGreskell $ toPlaceHolderVariable 0)
    got_v2 `shouldBe` (unsafeGreskell $ toPlaceHolderVariable 1)
    got_bind `shouldBe` HM.fromList [ (toGremlin got_v1, toJSON (100 :: Int)),
                                      (toGremlin got_v2, toJSON "hogehoge")
                                    ]
  it "should compose and produce new variables" $ do
    let b = newBind "foobar"
        ((got_v1, got_v2), got_bind) = runBinder $ ((,) <$> b <*> b)
    got_v1 `shouldBe` (unsafeGreskell $ toPlaceHolderVariable 0)
    got_v2 `shouldBe` (unsafeGreskell $ toPlaceHolderVariable 1)
    got_bind `shouldBe` HM.fromList [ (toGremlin got_v1, toJSON "foobar"),
                                      (toGremlin got_v2, toJSON "foobar")
                                    ]
