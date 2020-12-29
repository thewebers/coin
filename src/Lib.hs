{-# LANGUAGE NamedFieldPuns #-}

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

import Control.Exception

import Debug.Trace

import Data.Bits ( Bits(xor) )
import Data.Int ( Int32 )
import Data.Binary

import Data.List

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import qualified Crypto.PubKey.RSA as RSA
import Crypto.Hash
    ( hashFinalize,
      hashInitWith,
      hashUpdate,
      SHA512(..),
      Context,
      Digest )

-- type Hash = Int32
-- type Nonce = Int32

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
} deriving (Show)

data Transaction = Transaction {
    inputs :: [Transaction],
    amount :: Int32,
    mainOutput :: RSA.PublicKey,
    changeOutput :: RSA.PublicKey
} deriving (Show, Eq)

data Person = Person {
    publicKey :: RSA.PublicKey,
    privateKey :: RSA.PrivateKey
} deriving (Show)

newtype Chain = Chain {
    blocks :: [(Digest SHA512, Block)]
} deriving (Show)

emptyChain :: Chain
emptyChain = Chain { blocks = [] }

data Wallet = Wallet {
    person :: Person,
    unspentTransactions :: [Transaction],
    walletAmount :: Int32
} deriving (Show)

getLatestBlock :: Chain -> Block
getLatestBlock chain = snd $ head $ blocks chain

transactionAmount :: Transaction -> RSA.PublicKey -> Int32
transactionAmount tx person
    | mainOutput tx == person = amount tx
    | changeOutput tx == person =
        sum (map (`transactionAmount` person) $ inputs tx) - amount tx
    | otherwise = 0

getWallet :: Chain -> Person -> Wallet
getWallet chain person =
    let
        unspentTransactions = getUnspentTransactions (map snd $ blocks chain) person
        walletAmount = sum (map (`transactionAmount` publicKey person) unspentTransactions)
    in
        Wallet {
            person = person,
            unspentTransactions = unspentTransactions,
            walletAmount = walletAmount
        }

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

send :: Chain -> Person -> RSA.PublicKey -> Int32 -> Transaction
send chain sender receiverPublicKey amount =
    let
        -- TODO: select enough to satisfy the amount
        selectedTxs = unspentTransactions $ getWallet chain sender
        selectedAmount = sum (map (`transactionAmount` publicKey sender) selectedTxs)
        _ = assert (selectedAmount >= amount) ()
    in
        Transaction {
            inputs = selectedTxs,
            amount = amount,
            mainOutput = receiverPublicKey,
            changeOutput = publicKey sender
        }

{-
verify :: Chain -> RSA.PublicKey -> Int -> Int
-- TODO:  Verify that a given amount has been sent to you?
-}

hashBlock :: Block -> Digest SHA512
hashBlock b =
    let ctx = hashInitWith SHA512
        ctx' = hashBlock' b ctx
    in hashFinalize ctx'

hashBlock' :: Block -> Context SHA512 -> Context SHA512
hashBlock' block ctx =
    let ctx' = hashUpdate ctx $ prevHash block
        ctx'' = hashUpdate ctx' $ LB.toStrict $ encode $ nonce block
        ctx''' = hashUpdate ctx'' $ LB.toStrict $ encode $ timestamp block
    in foldl (flip hashTransaction') ctx''' (transactions block)

hashTransaction' :: Transaction -> Context SHA512 -> Context SHA512
hashTransaction' t ctx =
    let ctx' = hashUpdate ctx $ LB.toStrict $ encode $ amount t
        ctx'' = hashPubKey' (mainOutput t) ctx'
        ctx''' = hashPubKey' (changeOutput t) ctx''
    in foldl (flip hashTransaction') ctx''' (inputs t)

hashPubKey' :: RSA.PublicKey -> Context SHA512 -> Context SHA512
hashPubKey' RSA.PublicKey {RSA.public_size, RSA.public_n, RSA.public_e} ctx =
    let ctx' = hashUpdate ctx $ LB.toStrict $ encode public_size
        ctx'' = hashUpdate ctx' $ LB.toStrict $ encode public_n
    in hashUpdate ctx'' $ LB.toStrict $ encode public_e

rsaPublicKeyExponent :: Integer
rsaPublicKeyExponent = 257

createPerson :: IO Person
createPerson = do
    (pub, priv) <- RSA.generate rsaBitLen rsaPublicKeyExponent
    return $ Person { publicKey = pub, privateKey = priv }

-- make `createTransaction` function that takes `src`, `amount`, and `dest`, and
-- it finds transactions in `src`s wallet to back the new transaction

-- Mine
mine :: Person -> Int32 -> [Transaction] -> Chain -> Chain
mine person timestamp txs Chain { blocks = blocks' } =
    let prevHash = if null blocks' then hashFinalize (hashInitWith SHA512) else fst (head blocks')
    in Chain { blocks = aux prevHash 0 : blocks' }
  where
    aux prevHash nonce = let
      currBlock = Block {
        timestamp = timestamp,
        prevHash = prevHash,
        nonce = nonce,
        transactions = [
            Transaction {
                inputs = [],
                amount = 1,
                mainOutput = publicKey person,
                changeOutput = publicKey person
            }
        ]
      }
      currHash = hashBlock currBlock
      in
        -- check if has the right number of zeros
        if hasNLeadingZeros currHash 2
        -- if so, that's our block
        then (currHash, currBlock)
        -- otherwise, just increment the nonce and try hashing again
        else aux prevHash (nonce + 1)

{-
for nonce in nonce_candidates:
    if str(hash(Block { prevHash = prevHash, nonce = nonce, transactions = transactions })).startswith('0' * num_required_zeros):
        print("we've have mined it")
-}

-- TODO: shouldn't need to convert to string to impl this, because slow
hasNLeadingZeros :: Digest SHA512 -> Int -> Bool
hasNLeadingZeros x n =
    let xStr = show x
    in length (takeWhile (== '0') xStr) >= n
    -- in trace ("numZeros = " ++ show numZeros) (numZeros >= n)