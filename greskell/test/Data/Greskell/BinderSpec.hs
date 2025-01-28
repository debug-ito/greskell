{-# LANGUAGE OverloadedStrings #-}
module Data.Greskell.BinderSpec
    ( main
    , spec
    ) where

import           Control.Applicative    ((<$>), (<*>))
import           Control.Monad          (forM_)
import           Data.Aeson             (toJSON)
import qualified Data.Aeson.Key         as Key
import qualified Data.Aeson.KeyMap      as KM
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Test.Hspec

import           Data.Greskell.AsLabel  (AsLabel)
import           Data.Greskell.Binder   (Binder, newAsLabel, newBind, runBinder)
import           Data.Greskell.Greskell (Greskell, toGremlin, unsafeGreskell)

main :: IO ()
main = hspec spec

extractVarName :: Greskell a -> IO Text
extractVarName got_greskell = checkVarName =<< (stripParens $ toGremlin got_greskell)
  where
    stripParens v =
      case T.stripPrefix "((" =<< T.stripSuffix "))" v of
        Nothing -> do
          expectationFailure "Binder should produce an expression of a variable wrapped with double parens"
          return ""
        Just a  -> return a
    checkVarName v =
      case T.unpack v of
        [] -> do
          expectationFailure "Expect a Gremlin variable, but got empty script."
          return ""
        (h : rest) -> do
          h `shouldSatisfy` (`elem` variableHeads)
          forM_ rest (`shouldSatisfy` (`elem` variableRests))
          return v
    variableHeads = '_' : (['a' .. 'z'] ++ ['A' .. 'Z'])
    variableRests = variableHeads ++ ['0' .. '9']

spec :: Spec
spec = describe "Binder" $ do
  it "should keep bound values" $ do
    let b = do
          v1 <- newBind (100 :: Int)
          v2 <- newBind ("hogehoge" :: Text)
          return (v1, v2)
        ((got_v1, got_v2), got_bind) = runBinder b
    toGremlin got_v1 `shouldNotBe` toGremlin got_v2
    v1Name <- extractVarName got_v1
    v2Name <- extractVarName got_v2
    got_bind `shouldBe` KM.fromList [ (Key.fromText v1Name, toJSON (100 :: Int)),
                                      (Key.fromText v2Name, toJSON ("hogehoge" :: Text))
                                    ]
  it "should compose and produce new variables" $ do
    let b = newBind ("foobar" :: Text)
        ((got_v1, got_v2), got_bind) = runBinder $ ((,) <$> b <*> b)
    toGremlin got_v1 `shouldNotBe` toGremlin got_v2
    v1Name <- extractVarName got_v1
    v2Name <- extractVarName got_v2
    got_bind `shouldBe` KM.fromList [ (Key.fromText v1Name, toJSON ("foobar" :: Text)),
                                      (Key.fromText v2Name, toJSON ("foobar" :: Text))
                                    ]
  it "should also be able to produce AsLabels" $ do
    let newIntLabel :: Binder (AsLabel Int)
        newIntLabel = newAsLabel
        newVar = newBind ("foobar" :: Text)
        ((got_v1, got_l1, got_v2, got_l2), _) =
          runBinder $ ((,,,) <$> newVar <*> newIntLabel <*> newVar <*> newIntLabel)
    _ <- extractVarName got_v1
    _ <- extractVarName got_v2
    toGremlin got_v1 `shouldNotBe` toGremlin got_v2
    got_l1 `shouldNotBe` got_l2
