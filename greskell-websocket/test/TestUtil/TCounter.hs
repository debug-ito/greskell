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
         now,
         waitFor,
         history,
         count
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.STM
  ( TVar, newTVarIO, modifyTVar, readTVar,
    STM, atomically, retry
  )

-- | Transaction counter.
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

now :: TCounter -> IO Int
now tc = atomically $ nowSTM tc

nowSTM :: TCounter -> STM Int
nowSTM tc = readTVar $ tcCurrent tc

waitFor :: TCounter -> (Int -> Bool) -> IO ()
waitFor tc p = atomically $ do
  cur <- nowSTM tc
  if p cur
    then return ()
    else retry

history :: TCounter -> IO [Int]
history tc = reverse <$> (atomically $ readTVar $ tcHistory tc)

count :: TCounter -> IO a -> (a -> IO b) -> IO b
count tc start_act finish_act = do
  tx <- start_act
  modify tc (+ 1)
  ret <- finish_act tx
  modify tc (subtract 1)
  return ret
