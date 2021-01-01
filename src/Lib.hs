{-# LANGUAGE NamedFieldPuns, DeriveGeneric, FlexibleInstances #-}

module Lib where

{-
Assumptions:
- each user only ever has one RSA key pair
- we're not using merkle trees to compress transactions
- we use all transactions for user A as inputs to any transaction from A to B
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
import Data.Int ( Int32 )
import Data.List
import qualified Data.Serialize as Cereal

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

max' :: [Int] -> Int
max' [x] = x
max' (x:xs)
    | max' xs > x = max' xs
    | otherwise = x

xor' :: [Int32] -> Int32
xor' = foldl xor 0

data Block = Block {
    timestamp :: Int32,
    prevHash :: Digest SHA512,
    nonce :: Int32,
    transactions :: [Transaction]
} deriving (Show, Eq, Generic)

instance Cereal.Serialize Block where

data Transaction = Transaction {
    inputs :: [Transaction],
    txAmount :: Int32,
    mainOutput :: RSA.PublicKey,
    changeOutput :: RSA.PublicKey
} deriving (Show, Eq, Generic)

instance Cereal.Serialize Transaction where

data Person = Person {
    publicKey :: RSA.PublicKey,
    privateKey :: RSA.PrivateKey
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
    person :: Person,
    unspentTransactions :: [Transaction],
    walletAmount :: Int32
} deriving (Show)

hash :: Hashable a => a -> Digest SHA512
hash x =
    let initCtx = hashInitWith SHA512
        ctx = buildHashCtx x initCtx
    in hashFinalize ctx

class Hashable a where
    buildHashCtx :: a -> Context SHA512 -> Context SHA512

instance Hashable Block where
    buildHashCtx block ctx =
        let ctx' = hashUpdate ctx $ prevHash block
            ctx'' = hashUpdate ctx' $ LB.toStrict $ encode $ nonce block
            ctx''' = hashUpdate ctx'' $ LB.toStrict $ encode $ timestamp block
        in foldl (flip buildHashCtx) ctx''' (transactions block)

instance Hashable Transaction where
    buildHashCtx tx ctx =
        let ctx' = hashUpdate ctx $ LB.toStrict $ encode $ txAmount tx
            ctx'' = buildHashCtx (mainOutput tx) ctx'
            ctx''' = buildHashCtx (changeOutput tx) ctx''
        in foldl (flip buildHashCtx) ctx''' (inputs tx)

instance Hashable RSA.PublicKey where
    buildHashCtx RSA.PublicKey {RSA.public_size, RSA.public_n, RSA.public_e} ctx =
        let ctx' = hashUpdate ctx $ LB.toStrict $ encode public_size
            ctx'' = hashUpdate ctx' $ LB.toStrict $ encode public_n
        in hashUpdate ctx'' $ LB.toStrict $ encode public_e

getLatestBlock :: Chain -> Block
getLatestBlock chain = snd $ head $ blocks chain

transactionAmount :: Transaction -> RSA.PublicKey -> Int32
transactionAmount tx person
    | mainOutput tx == person = txAmount tx
    | changeOutput tx == person =
        sum (map (`transactionAmount` person) $ inputs tx) - txAmount tx
    | otherwise = 0

getWallet :: TVar Chain -> Person -> STM Wallet
getWallet chainVar person = do
    chain <- readTVar chainVar
    let unspentTransactions = getUnspentTransactions (map snd $ blocks chain) person
    let walletAmount = sum (map (`transactionAmount` publicKey person) unspentTransactions)
    return $ Wallet {
        person = person,
        unspentTransactions = unspentTransactions,
        walletAmount = walletAmount
    }

allTransactions :: Chain -> [Transaction]
allTransactions chain = concatMap (transactions . snd) (blocks chain)

getUnspentTransactions :: [Block] -> Person -> [Transaction]
getUnspentTransactions [] person = []
getUnspentTransactions (b:bs) person =
    let unspent = getUnspentTransactions bs person
        involvesPerson t = publicKey person `elem` [mainOutput t, changeOutput t]
        txs = filter involvesPerson (transactions b)
        currInputs = concatMap inputs txs
        -- TODO verify there are no double-spends
        unspent' = filter (`notElem` currInputs) unspent
    in unspent' ++ txs

mkTransaction :: Wallet -> Person -> RSA.PublicKey -> Int32 -> STM Transaction
mkTransaction wallet sender receiverPublicKey amount = do
    -- TODO: select just enough transactions to satisfy the amount
    let selectedTxs = unspentTransactions wallet
    let selectedAmount = sum (map (`transactionAmount` publicKey sender) selectedTxs)
    let _ = assert (selectedAmount >= amount) ()
    return $ Transaction {
        inputs = selectedTxs,
        txAmount = amount,
        mainOutput = receiverPublicKey,
        changeOutput = publicKey sender
    }

rsaPublicKeyExponent :: Integer
rsaPublicKeyExponent = 257

createPerson :: IO Person
createPerson = do
    (pub, priv) <- RSA.generate rsaBitLen rsaPublicKeyExponent
    return $ Person { publicKey = pub, privateKey = priv }

-- Mine
mine :: Person -> Int32 -> [Transaction] -> Chain -> Chain
mine person timestamp txs Chain { blocks = blocks' } =
    let prevHash = if null blocks' then hashFinalize (hashInitWith SHA512) else fst (head blocks')
    in Chain { blocks = aux prevHash 0 : blocks' }
  where
    aux prevHash nonce = let
      mintTx = Transaction {
          inputs = [],
          txAmount = 1,
          mainOutput = publicKey person,
          changeOutput = publicKey person
      }
      currBlock = Block {
        timestamp = timestamp,
        prevHash = prevHash,
        nonce = nonce,
        transactions = mintTx : txs
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
