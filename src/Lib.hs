{-# LANGUAGE NamedFieldPuns, DeriveGeneric, FlexibleInstances #-}

module Lib where

{-
Assumptions:
- each user only ever has one RSA key pair
- we're not using merkle trees to compress transactions
- we use all transactions for user A astxInputs to any transaction from A to B
  (including for coin transactions, where A = B), rather than calculating the
  minimum number of transactions required to fulfill the amount being sent
- people don't own quantum computers yet
-}

import Control.Concurrent.STM
import Control.Exception

import GHC.Generics

import Debug.Trace

import Data.Binary
import Data.Bits ( Bits(xor) )
import Data.Int ( Int32, Int64 )
import Data.List
import qualified Data.Serialize as Cereal
import Data.Time
import Data.Time.Clock.POSIX

import qualified Data.ByteString as BS
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as LB

import Crypto.Hash
    ( digestFromByteString,
      hashFinalize,
      hashInitWith,
      hashUpdate,
      SHA512(..),
      Context,
      Digest )
import qualified Crypto.PubKey.RSA as RSA

rsaBitLen = 512     -- size of RSA keys
rsaPrime = 3        -- RSA seed prime (methinks)
blockLen = 1000     -- number of transactions in a block

mkTimestamp :: IO Int64
mkTimestamp = do
    u <- getCurrentTime
    return $ floor . (1e9 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds $ u

data Block = Block {
    blkTimestamp :: Int64,
    blkPrevHash :: Digest SHA512,
    blkNonce :: Int32,
    blkTransactions :: [Transaction]
} deriving (Show, Eq, Generic)

instance Cereal.Serialize Block where

data Transaction = Transaction {
    txTimestamp :: Int64,
    txInputs :: [Transaction],
    txAmount :: Int32,
    txMainOutput :: RSA.PublicKey,
    txChangeOutput :: RSA.PublicKey
} deriving (Show, Eq, Generic)

txSender :: Transaction -> RSA.PublicKey
txSender = txChangeOutput

txReceiver :: Transaction -> RSA.PublicKey
txReceiver = txMainOutput

instance Cereal.Serialize Transaction where

data Person = Person {
    psnPublicKey :: RSA.PublicKey,
    psnPrivateKey :: RSA.PrivateKey
} deriving (Show, Eq, Generic)

instance Cereal.Serialize Person where

instance Cereal.Serialize RSA.PublicKey where
    put RSA.PublicKey { RSA.public_size = size, RSA.public_n = n, RSA.public_e = e } =
        Cereal.put size
        >> Cereal.put n
        >> Cereal.put e

    get = do
        public_size <- Cereal.get
        public_n <- Cereal.get
        public_e <- Cereal.get
        return RSA.PublicKey { RSA.public_size, RSA.public_n, RSA.public_e }

instance Cereal.Serialize RSA.PrivateKey where
    put RSA.PrivateKey {
        RSA.private_pub,
        RSA.private_d,
        RSA.private_p,
        RSA.private_q,
        RSA.private_dP,
        RSA.private_dQ,
        RSA.private_qinv
        } =
        Cereal.put private_pub
        >> Cereal.put private_d
        >> Cereal.put private_p
        >> Cereal.put private_q
        >> Cereal.put private_dP
        >> Cereal.put private_dQ
        >> Cereal.put private_qinv

    get = do
        private_pub <- Cereal.get
        private_d <- Cereal.get
        private_p <- Cereal.get
        private_q <- Cereal.get
        private_dP <- Cereal.get
        private_dQ <- Cereal.get
        private_qinv <- Cereal.get
        return RSA.PrivateKey {
                RSA.private_pub,
                RSA.private_d,
                RSA.private_p,
                RSA.private_q,
                RSA.private_dP,
                RSA.private_dQ,
                RSA.private_qinv
                }

userAbbrev :: RSA.PublicKey -> String
userAbbrev pubKey = take 5 (show (RSA.public_n pubKey))

newtype Chain = Chain {
    blocks :: [(Digest SHA512, Block)]
} deriving (Show, Eq, Generic)
instance Cereal.Serialize Chain where

toByteString :: BA.ByteArrayAccess a => a -> BS.ByteString
toByteString = BS.pack . BA.unpack

instance Cereal.Serialize (Digest SHA512) where
    put digest = Cereal.put $ toByteString digest
    get = do
        byteStr <- Cereal.get :: Cereal.Get BS.ByteString
        case digestFromByteString byteStr of
            Just digest -> return digest
            Nothing -> fail ""

emptyChain :: STM (TVar Chain)
emptyChain = newTVar $ Chain { blocks = [] }

data Wallet = Wallet {
    wltPerson :: Person,
    wltUnspentTransactions :: [Transaction],
    wltAmount :: Int32
} deriving (Show)

-- hashBlock :: Block -> Digest SHA512
-- hash block == hashBlock block
-- hash transaction
hash :: Hashable a => a -> Digest SHA512
hash x =
    let initCtx = hashInitWith SHA512
        ctx = buildHashCtx x initCtx
    in hashFinalize ctx

class Hashable a where
    buildHashCtx :: a -> Context SHA512 -> Context SHA512

instance Hashable Block where
    buildHashCtx block ctx =
        let ctx' = hashUpdate ctx $ blkPrevHash block
            ctx'' = hashUpdate ctx' $ LB.toStrict $ encode $ blkNonce block
            ctx''' = hashUpdate ctx'' $ LB.toStrict $ encode $ blkTimestamp block
        in foldl (flip buildHashCtx) ctx''' (blkTransactions block)

instance Hashable Transaction where
    buildHashCtx tx ctx =
        let ctx' = hashUpdate ctx $ LB.toStrict $ encode $ txTimestamp tx
            ctx'' = hashUpdate ctx' $ LB.toStrict $ encode $ txAmount tx
            ctx''' = buildHashCtx (txMainOutput tx) ctx''
            ctx'''' = buildHashCtx (txChangeOutput tx) ctx'''
        in foldl (flip buildHashCtx) ctx'''' (txInputs tx)

instance Hashable RSA.PublicKey where
    buildHashCtx RSA.PublicKey {RSA.public_size, RSA.public_n, RSA.public_e} ctx =
        let ctx' = hashUpdate ctx $ LB.toStrict $ encode public_size
            ctx'' = hashUpdate ctx' $ LB.toStrict $ encode public_n
        in hashUpdate ctx'' $ LB.toStrict $ encode public_e

getLatestBlock :: Chain -> Block
getLatestBlock chain = snd $ head $ blocks chain

transactionAmount :: Transaction -> RSA.PublicKey -> Int32
transactionAmount tx person
    | txMainOutput tx == person = txAmount tx
    | txChangeOutput tx == person =
        sum (map (`transactionAmount` person) $ txInputs tx) - txAmount tx
    | otherwise = 0

getWallet :: TVar Chain -> Person -> STM Wallet
getWallet chainVar person = do
    chain <- readTVar chainVar
    let unspentTransactions = getUnspentTransactions (map snd $ blocks chain) person
    let wltAmount = sum (map (`transactionAmount` psnPublicKey person) unspentTransactions)
    return $ Wallet {
        wltPerson = person,
        wltUnspentTransactions = unspentTransactions,
        wltAmount = wltAmount
    }

allTransactions :: Chain -> [Transaction]
allTransactions chain = concatMap (blkTransactions . snd) (blocks chain)

getUnspentTransactions :: [Block] -> Person -> [Transaction]
getUnspentTransactions [] person = []
getUnspentTransactions (b:bs) person =
    let unspent = getUnspentTransactions bs person
        involvesPerson t = psnPublicKey person `elem` [txMainOutput t, txChangeOutput t]
        txs = filter involvesPerson (blkTransactions b)
        currInputs = concatMap txInputs txs
        -- TODO verify there are no double-spends
        unspent' = filter (`notElem` currInputs) unspent
    in unspent' ++ txs

mkTransaction :: Wallet -> Person -> RSA.PublicKey -> Int32 -> Int64 -> STM Transaction
mkTransaction wallet sender receiverPublicKey amount timestamp = do
    -- TODO: select just enough transactions to satisfy the amount
    let selectedTxs = wltUnspentTransactions wallet
    let selectedAmount = sum (map (`transactionAmount` psnPublicKey sender) selectedTxs)
    let _ = assert (selectedAmount >= amount) ()
    return $ Transaction {
        txTimestamp = timestamp,
        txInputs = selectedTxs,
        txAmount = amount,
        txMainOutput = receiverPublicKey,
        txChangeOutput = psnPublicKey sender
    }

rsaPublicKeyExponent :: Integer
rsaPublicKeyExponent = 257

createPerson :: IO Person
createPerson = do
    (pub, priv) <- RSA.generate rsaBitLen rsaPublicKeyExponent
    return $ Person { psnPublicKey = pub, psnPrivateKey = priv }

-- TODO keep a penultimate hash ctx with everything except the `nonce` in it,
-- then each leading zero check only needs to update the penultimate ctx with
-- the new nonce, rather than rehashing everything in each recursive call
mine :: Person -> Int64 -> [Transaction] -> Chain -> Chain
mine person timestamp txs Chain { blocks = blocks' } =
    let prevHash = if null blocks' then hashFinalize (hashInitWith SHA512) else fst (head blocks')
    in Chain { blocks = aux prevHash 0 : blocks' }
  where
    aux prevHash nonce = let
      mintTx = Transaction {
          txTimestamp = timestamp,
          txInputs = [],
          txAmount = 1,
          txMainOutput = psnPublicKey person,
          txChangeOutput = psnPublicKey person
      }
      currBlock = Block {
        blkTimestamp = timestamp,
        blkPrevHash = prevHash,
        blkNonce = nonce,
        blkTransactions = mintTx : txs
      }
      currHash = hash currBlock
      in
        -- check if has the right number of zeros
        if hasNLeadingZeros currHash 2
        -- if so, that's our block
        then (currHash, currBlock)
        -- otherwise, just increment the nonce and try hashing again
        else aux prevHash (nonce + 1)

-- TODO: shouldn't need to convert to string to impl this, because slow
hasNLeadingZeros :: Digest SHA512 -> Int -> Bool
hasNLeadingZeros x n =
    let xStr = show x
    in length (takeWhile (== '0') xStr) >= n
