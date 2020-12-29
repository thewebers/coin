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

mineSingle :: Person -> [Transaction] -> Chain -> IO Chain
mineSingle person txs chain = do
  timestamp <- round `fmap` getPOSIXTime
  let chain' = mine person timestamp txs chain
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
  chain' <- mineSingle person [] chain
  mineNBlocks person chain' (n-1)

printAllTransactions :: Chain -> IO ()
printAllTransactions chain =
  aux (zip [0..] $ reverse $ map snd $ blocks chain)
  where aux [] = return ()
        aux ((i,b) : bs) = do
          putStrLn $ "block " ++ show i
          forM_ (transactions b) (\tx -> do { putStr "  "; printTransaction tx }) 
          aux bs

printTransaction :: Transaction -> IO ()
printTransaction tx = do
  let srcStr = userAbbrev $ changeOutput tx
  let destStr = userAbbrev $ mainOutput tx
  putStrLn $ srcStr ++ " -- " ++ show (amount tx) ++ " --> " ++ destStr


main = do
  personA <- createPerson
  personB <- createPerson
  chain <- mineNBlocks personA emptyChain 10
  let tx = send chain personA (publicKey personB) 5
  chain' <- mineSingle personA [tx] chain
  printAllTransactions chain'
  -- print $ transactions $ snd $ head $ blocks chain'
  return ()