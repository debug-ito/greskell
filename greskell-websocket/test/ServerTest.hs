{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
module Main (main,spec) where
import Test.Hspec

import qualified ServerTest.Connection as Conn

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Conn.spec
