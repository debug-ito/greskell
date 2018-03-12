-- |
-- Module: Database.TinkerPop
-- Description: A mock module for the real Database.TinkerPop from gremlin-haskell package
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- Because gremlin-haskell fails to build in new GHCs, we use this
-- module just for test-readme.
module Database.TinkerPop
       ( Connection,
         run,
         submit
       ) where

import Data.Text (Text)
import Data.Aeson (Object, Value)

data Connection

run :: String -> Int -> (Connection -> IO ()) -> IO ()
run = error "This is just for compilation test. Do not execute it."

type Gremlin = Text
type Binding = Object

submit :: Connection -> Gremlin -> Maybe Binding -> IO (Either String [Value])
submit = error "This is just for compilation test. Do not execute it."
