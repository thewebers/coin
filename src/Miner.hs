module Miner where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Time.Clock.POSIX

import Lib

mineSingle :: Person -> [Transaction] -> Chain -> IO Chain
mineSingle person txs chain = do
  timestamp <- round `fmap` getPOSIXTime
  txs' <- filterInvalidTransactions txs chain
  let chain' = mine person timestamp txs' chain
  print $ length (blocks chain')
  putStrLn $ "hash = " ++ show (fst $ head (blocks chain'))
  return chain'

filterInvalidTransactions :: [Transaction] -> Chain -> IO [Transaction]
filterInvalidTransactions txs chain = aux [] txs
  where
    aux acc [] = return []
    aux acc (tx:txs) = do
      let allTxs = concatMap (transactions . snd) (blocks chain)
      let ghostTxs = filter (`notElem` allTxs) (inputs tx)
      let dupInputs = filter (`elem` acc) (inputs tx)
      if null dupInputs && null ghostTxs
      then do
        txs' <- aux (acc ++ inputs tx) txs
        return $ tx : txs'
      else do
        unless (null dupInputs) $ do
          putStrLn "attempted double-spend(s) with following input transaction(s):"
          forM_ dupInputs (\tx' -> do { putStr "  "; printTransaction tx' })
        unless (null ghostTxs) $ do
          putStrLn "attempted use of following nonexistent transaction(s):"
          forM_ ghostTxs (\tx' -> do { putStr "  "; printTransaction tx' })
        aux acc txs


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

collectTransactions :: TChan Transaction -> STM [Transaction]
collectTransactions txChan = do
  maybeTx <- tryReadTChan txChan
  case maybeTx of
    Just tx -> do
      txs <- collectTransactions txChan
      return $ tx : txs
    Nothing -> return []

minerMain :: Person -> TVar Chain -> TChan Transaction -> IO ()
minerMain person chainVar txChan = forever $ do
  txs <- atomically $ collectTransactions txChan
  putStrLn $ "we got " ++ show (length txs) ++ " txs"
  unless (null txs) $ do
    putStrLn $ "committing " ++ show (length txs) ++ " transactions"
    forM_ txs (\tx -> do { putStr "  "; printTransaction tx })
    putStrLn ""
  chain <- readTVarIO chainVar
  chain' <- mineSingle person txs chain
  -- TODO need to handle checking if the chain was updated while we were mining
  -- and see which transactions we still need to commit in the next block.
  -- don't need to worry about this right now because there's only one miner
  atomically $ writeTVar chainVar chain'
  threadDelay 1000
  -- tx <- atomically $ readTChan txChan
  -- print tx
  return ()
