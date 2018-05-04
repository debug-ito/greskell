-- |
-- Module: TestUtil.TCounter
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module TestUtil.TCounter
       ( TCounter,
         new,
         modify,
         history,
         count
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.STM
  ( TVar, newTVarIO, modifyTVar, readTVar,
    atomically
  )

-- | Transactional counter
data TCounter = TCounter { tcCurrent :: TVar Int,
                           tcHistory :: TVar [Int]
                         }
  
new :: IO TCounter
new = TCounter <$> newTVarIO 0 <*> newTVarIO []

modify :: TCounter -> (Int -> Int) -> IO ()
modify tc f = atomically $ do
  modifyTVar (tcCurrent tc) f
  conc <- readTVar (tcCurrent tc)
  modifyTVar (tcHistory tc) (conc :)

history :: TCounter -> IO [Int]
history tc = reverse <$> (atomically $ readTVar $ tcHistory tc)

count :: TCounter -> IO a -> (a -> IO b) -> IO b
count tc start_act finish_act = do
  tx <- start_act
  modify tc (+ 1)
  ret <- finish_act tx
  modify tc (subtract 1)
  return ret
