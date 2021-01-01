module Miner where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Time.Clock.POSIX

import Lib

mineSingle :: Person -> [Transaction] -> Chain -> IO Chain
mineSingle person txs chain = do
  timestamp <- round `fmap` getPOSIXTime
  unless (transactionsValid txs) $ do
    putStrLn "GOT INVALID TRANSACTION"
    -- TODO ignore invalid transactions and carry on
    fail "invalid transactions"
  let chain' = mine person timestamp txs chain
  print $ length (blocks chain')
  putStrLn $ "hash = " ++ show (fst $ head (blocks chain'))
  return chain'

-- TODO check also that the transactions actually exist in the blockchain via
-- unspent transactions given by `getWallet`
transactionsValid :: [Transaction] -> Bool
transactionsValid txs = snd $ foldl (\(acc, valid) t -> (acc ++ inputs t, valid && all (`notElem` acc) (inputs t))) ([], True) txs

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
  atomically $ writeTVar chainVar chain'
  -- TODO need to handle checking if the chain was updated while we were mining
  -- and see which transactions we still need to commit in the next block
  threadDelay 1000
  -- tx <- atomically $ readTChan txChan
  -- print tx
  return ()
