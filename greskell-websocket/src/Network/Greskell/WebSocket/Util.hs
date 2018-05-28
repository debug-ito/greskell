-- |
-- Module: Network.Greskell.WebSocket.Util
-- Description: Common utility
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __Internal module__.
module Network.Greskell.WebSocket.Util
       ( slurp
       ) where

import Data.Monoid ((<>))
import qualified Data.DList as DL

slurp :: Monad m => m (Maybe a) -> m [a]
slurp act = fmap DL.toList $ go mempty
  where
    go got = do
      mres <- act
      case mres of
       Nothing -> return got
       Just res -> go (got <> DL.singleton res)
