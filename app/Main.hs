module Main where

import Control.Monad
import Data.Time.Clock.POSIX

import Lib
import Worker

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
--   let block = mineGenesis (publicKey person) timestamp
--   print block

-- mineLoop :: Person -> Chain -> IO Chain
-- mineLoop person chain = do
--   timestamp <- round `fmap` getPOSIXTime
--   let chain' = mine person timestamp [] chain
--   print $ length (blocks chain')
--   print $ "hash = " ++ show (head (blocks chain'))
--   mineLoop person chain'

main :: IO ()
main = do
  personA <- createPerson
  personB <- createPerson
  chain <- mineNBlocks personA emptyChain 10
  let tx = send chain personA (publicKey personB) 5
  chain' <- mineSingle personA [tx] chain
  printAllTransactions chain'
  -- print $ transactions $ snd $ head $ blocks chain'
  return ()