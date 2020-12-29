module Main where

import Control.Monad
import Data.Time.Clock.POSIX

import Lib

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
--   let block = mineGenesis (publicKey person) timestamp
--   print block

mineSingle :: Person -> Chain -> IO Chain
mineSingle person chain = do
  timestamp <- round `fmap` getPOSIXTime
  let chain' = mine person timestamp [] chain
  print $ length (blocks chain')
  print $ "hash = " ++ show (fst $ head (blocks chain'))
  return chain'

-- mineLoop :: Person -> Chain -> IO Chain
-- mineLoop person chain = do
--   timestamp <- round `fmap` getPOSIXTime
--   let chain' = mine person timestamp [] chain
--   print $ length (blocks chain')
--   print $ "hash = " ++ show (head (blocks chain'))
--   mineLoop person chain'

mineNBlocks :: Person -> Chain -> Int -> IO Chain
mineNBlocks person chain 0 = return chain
mineNBlocks person chain n = do
  chain' <- mineSingle person chain
  mineNBlocks person chain' (n-1)

main = do
  personA <- createPerson
  personB <- createPerson
  chain <- mineNBlocks personA emptyChain 10
  let tx = send chain personA (publicKey personB) 5
  return ()