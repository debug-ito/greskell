-- |
-- Module: Network.Greskell.WebSocket.Util
-- Description: Common utility
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __Internal module__.
module Network.Greskell.WebSocket.Util
    ( slurp
    , drain
    ) where

import           Data.Monoid ((<>))
import qualified Data.Vector as V

slurp :: Monad m => m (Maybe a) -> m (V.Vector a)
slurp act = go mempty
  where
    go got = do
      mres <- act
      case mres of
       Nothing  -> return got
       Just res -> go $! (V.snoc got res)

drain :: Monad m => m (Maybe a) -> m ()
drain act = go
  where
    go = do
      mres <- act
      case mres of
       Nothing -> return ()
       Just _  -> go
