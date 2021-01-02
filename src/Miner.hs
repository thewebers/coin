module Miner where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

import Lib

mineSingle :: Person -> [Transaction] -> Chain -> IO Chain
mineSingle person txs chain = do
  timestamp <- mkTimestamp
  txs' <- filterInvalidTransactions txs chain
  let chain' = mine person timestamp txs' chain
  putStrLn $ "num blocks in chain: " ++ show (length (blocks chain'))
  -- putStrLn $ "hash = " ++ show (fst $ head (blocks chain'))
  return chain'

filterInvalidTransactions :: [Transaction] -> Chain -> IO [Transaction]
filterInvalidTransactions txs chain = aux [] txs
  where
    aux acc [] = return []
    aux acc (tx:txs) = do
      -- TODO make sure users can't spend other users' transactions

      -- pair the inputs of the transaction `tx` with the spender of the
      -- transaction (i.e., `txSender tx`), so we can check for
      -- double-spends and later update the accumulator
      let userPairedTxs = map (\inputTx -> (txSender tx, inputTx)) $ txInputs tx
      let dupInputs = filter (`elem` acc) userPairedTxs
      let ghostTxs = filter (`notElem` allTransactions chain) (txInputs tx)
      if null dupInputs && null ghostTxs
      then do
        let acc' = acc ++ userPairedTxs
        txs' <- aux acc' txs
        return $ tx : txs'
      else do
        unless (null dupInputs) $ do
          putStrLn "attempted double-spend in transaction"
          putStr "  "
          printTransaction tx
          putStrLn "with following double-spent input transaction(s):"
          forM_ dupInputs (\(_, tx') -> do { putStr "  "; printTransaction tx' })
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
          forM_ (blkTransactions b) (\tx -> do { putStr "  "; printTransaction tx })
          aux bs

printTransaction :: Transaction -> IO ()
printTransaction tx = do
  let srcStr = userAbbrev $ txSender tx
  let destStr = userAbbrev $ txReceiver tx
  putStrLn $ "[" ++ take 5 (show (hash tx)) ++ "] " ++ srcStr ++ " -- " ++ show (txAmount tx) ++ " --> " ++ destStr
  putStrLn $ foldl (\acc x ->  acc ++ take 5 (show (hash x)) ++ ", ") "\tinputs: " (txInputs tx)
  -- putStrLn $ srcStr ++ " -- " ++ show (txAmount tx) ++ " --> " ++ destStr

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
  (chain, txs) <- atomically $ do
    chain <- readTVar chainVar
    txs <- collectTransactions txChan
    return (chain, txs)
  putStrLn $ "we got " ++ show (length txs) ++ " txs"
  chain' <- mineSingle person txs chain
  let committedTxs = blkTransactions . snd . head . blocks $ chain'
  putStrLn $ "committed " ++ show (length committedTxs) ++ " transactions"
  forM_ committedTxs (\tx -> do { putStr "  "; printTransaction tx })
  putStrLn ""
  -- TODO need to handle checking if the chain was updated while we were mining
  -- and see which transactions we still need to commit in the next block.
  -- don't need to worry about this right now because there's only one miner
  atomically $ writeTVar chainVar chain'
  threadDelay 1000
  -- tx <- atomically $ readTChan txChan
  -- print tx
  return ()
