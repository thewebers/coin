module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad
import Data.Time.Clock.POSIX

import qualified Crypto.PubKey.RSA as RSA

import Lib
import Miner
import User

main :: IO ()
-- main = do
--   person <- createPerson
--   print person
--   mineGenesis

-- Test case:
-- user A mines one coin
-- in the next block, A tries to send 2 coins to B
-- and the transaction is rejected

-- main = do
--   person <- createPerson
--   print person
--   timestamp <- round `fmap` getPOSIXTime
--   let block = mineGenesis (psnPublicKey person) timestamp
--   print block

-- mineLoop :: Person -> Chain -> IO Chain
-- mineLoop person chain = do
--   timestamp <- round `fmap` getPOSIXTime
--   let chain' = mine person timestamp [] chain
--   print $ length (blocks chain')
--   print $ "hash = " ++ show (head (blocks chain'))
--   mineLoop person chain'

-- main = do
--   personA <- createPerson
--   personB <- createPerson
--   chain <- mineNBlocks personA emptyChain 10
--   let tx = send chain personA (psnPublicKey personB) 5
--   chain' <- mineSingle personA [tx] chain
--   printAllTransactions chain'
--   -- print $ transactions $ snd $ head $ blocks chain'
--   return ()

forkThread :: IO () -> IO (MVar ())
forkThread proc = do
    handle <- newEmptyMVar
    _ <- forkFinally proc (\_ -> putMVar handle ())
    return handle

{-
Creates list of sublists from the given list where each sublist is the
original list with exactly one user removed.
The removed user is saved as a full `Person` in the tuple and the sublist
stores only the public keys of other users.
-}
userThreadArgs :: [Person] -> [(Person, [RSA.PublicKey])]
userThreadArgs [] = []
userThreadArgs (p:ps) =
  (p, (map psnPublicKey ps)) : map (\(p', ks) -> (p', (psnPublicKey p) : ks)) (userThreadArgs ps)

main = do
  users <- replicateM 2 createPerson
  -- designate the first user to also be a miner
  let miners = [head users]
  chainVar <- atomically emptyChain
  txChan <- atomically newTChan
  userThreads <- forM (userThreadArgs users) $ \(person, others) -> forkThread $ userMain person others chainVar txChan
  minerThreads <- forM miners $ \miner -> forkThread $ minerMain miner chainVar txChan
  -- wait for threads to finish
  mapM_ takeMVar userThreads
  mapM_ takeMVar minerThreads
  return ()