module User where

import Debug.Trace

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Random
import Data.Time.Clock.POSIX
import Data.Int ( Int32 )

import Lib
import qualified Crypto.PubKey.RSA as RSA

{-
verify :: Chain -> RSA.PublicKey -> Int -> Int
-- TODO:  Verify that a given amount has been sent to you?
-}

foreverM :: Monad m => b -> (b -> m b) -> m ()
foreverM x f = do
   x' <- f x
   foreverM x' f
   return ()

-- TODO why are there still double spends
trySendTo :: Person -> RSA.PublicKey -> Int32 -> [Transaction] -> TVar Chain -> TChan Transaction -> STM [Transaction]
trySendTo self other amount pendingSends chainVar txChan = do
  wallet <- getWallet chainVar self
  -- TODO until we select the minimum number of unspent transactions to fulfill
  -- a send, each transaction will render users unable to send another until
  -- their most recent (and now only unspent) transaction is committed
  let unspentTxs = unspentTransactions wallet
  let localUnspentTxs = filter (`notElem` concatMap inputs pendingSends) unspentTxs
  let localBalance = sum (map (`transactionAmount` publicKey self) localUnspentTxs)
  -- return $ trace $ "num localUnspentTxs : " ++ show (length localUnspentTxs)
  -- return $ trace $ "localBalance : " ++ show (localBalance)
  let wallet' = trace ("[" ++ userAbbrev (publicKey self) ++ "] num unspentTxs : " ++ show (length unspentTxs) ++ "\n  num localUnspentTxs : " ++ show (length localUnspentTxs) ++ "\n  localBalance : " ++ show localBalance) $ Wallet {
    person = self,
    unspentTransactions = localUnspentTxs,
    walletAmount = localBalance
  }
  when (walletAmount wallet' < amount) retry
  tx <- mkTransaction wallet' self other amount
  writeTChan txChan tx
  chain <- readTVar chainVar
  -- filter out all sends that have been committed to the blockchain and add
  -- the transaction we just made
  let pendingSends' = tx : filter (`notElem` allTransactions chain) pendingSends
  return pendingSends'


userMain :: Person -> [RSA.PublicKey] -> TVar Chain -> TChan Transaction -> IO ()
userMain self others chainVar txChan = foreverM [] $ \pendingSends -> do
  threadDelay 1000000
  other <- uniform others
  putStrLn $ userAbbrev (publicKey self) ++ " trying to send 1 to " ++ userAbbrev other
  atomically $ do
    -- first, try to send the amount
    trySendTo self other 1 pendingSends chainVar txChan
    `orElse`
    -- if the transaction fails, the pending sends are unmodified
    return pendingSends
