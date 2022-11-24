{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main
    ( main
    , spec
    ) where
import           Test.Hspec

import qualified ServerTest.Client     as Client
import qualified ServerTest.Connection as Conn

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Conn.spec
  Client.spec
