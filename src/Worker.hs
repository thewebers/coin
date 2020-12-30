module Worker where

import Control.Monad
import Data.Time.Clock.POSIX

import Lib

mineSingle :: Person -> [Transaction] -> Chain -> IO Chain
mineSingle person txs chain = do
  timestamp <- round `fmap` getPOSIXTime
  unless (transactionsValid txs) $ fail "invalid transactions"
  let chain' = mine person timestamp txs chain
  print $ length (blocks chain')
  print $ "hash = " ++ show (fst $ head (blocks chain'))
  return chain'

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
