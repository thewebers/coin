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

-- TODO I think we generate double spends with this impl, because we're checking
-- the blockchain to detect whether we can send a certain amount, and we're not
-- including any pending transactions we might have sent

foreverM :: Monad m => b -> (b -> m b) -> m ()
foreverM x f = do
   x' <- f x
   foreverM x' f
   return ()

sendTo :: Person -> RSA.PublicKey -> Int32 -> TVar Chain -> TChan Transaction -> STM ()
sendTo self other amount chainVar txChan = do
  wallet <- getWallet chainVar self
  when (walletAmount wallet < amount) retry
  tx <- mkTransaction wallet self other amount
  writeTChan txChan tx

userMain :: Person -> [RSA.PublicKey] -> TVar Chain -> TChan Transaction -> IO ()
userMain self others chainVar txChan = foreverM [] $ \pendingTransactions -> do
  threadDelay 1000000
  other <- uniform others
  putStrLn $ userAbbrev (publicKey self) ++ " trying to send 1 to " ++ userAbbrev other
  atomically $ sendTo self other 1 chainVar txChan
  -- TODO update pending transactions according to those committed
  return pendingTransactions
