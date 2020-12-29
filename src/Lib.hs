{-# LANGUAGE NamedFieldPuns #-}

module Lib where

{-
Assumptions:
- each user only ever has one RSA key pair
- we're not using merkle trees to compress transactions
- we use all transactions for user A as inputs to any transaction from A to B
  (including for coin transactions, where A = B), rather than calculating the
  minimum number of transactions required to fulfill the amount being sent
-}

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
    -- ident :: Int32,
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

data Wallet = Wallet {
    person :: Person,
    unspentTransactions :: [Transaction],
    walletAmount :: Int
} deriving (Show)

getLatestBlock :: Chain -> Block
getLatestBlock chain = snd $ head $ blocks chain

-- getWallet :: Chain -> Person -> Wallet
-- getWallet chain person =
--     let
--         unspentTransactions =
--             filter (\x -> (mainOutput x == publicKey person) || (changeOutput x == publicKey person)) $
--             concatMap (transactions . snd) (blocks chain)
--         walletAmount = foldl (. changeOutput) 0 unspentTransactions
--     in
--         Wallet {
--             person = person,
--             unspentTransactions = unspentTransactions,
--             walletAmount = walletAmount
--         }

-- send :: Chain -> RSA.PublicKey -> RSA.PublicKey -> Rational -> Chain
-- send chain senderPublicKey receiverPublicKey amount =
--     let wallet = getWallet chain

-- hashFromBytestrings :: [ByteString] -> Digest SHA512
-- hashFromByteStrings xs =
--     let ctx = hashInitWith SHA512
--         ctx = hashUpdate ctx ([prevHash block, nonce block] ++ map hashTransaction (transactions block))
--     in hashFinalize ctx

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
    let ctx' = hashUpdate ctx $ LB.toStrict $ encode $ public_size
        ctx'' = hashUpdate ctx' $ LB.toStrict $ encode $ public_n
    in hashUpdate ctx'' $ LB.toStrict $ encode $ public_e

-- hashPerson' :: Person -> Context SHA512 -> Context SHA512
-- hashPerson' p ctx = hashUpdate ctx $ publicKey p
    -- let ctx' = hashUpdate ctx $ publicKey p
    -- in hashUpdate ctx' $ privateKey p

rsaPublicKeyExponent :: Integer
rsaPublicKeyExponent = 257

-- max :: Ord x => x -> x -> x
-- max a b = if a `le` b then b else a

-- class Ord x where
--     le :: x -> x -> Bool
--     ge :: x -> x -> Bool

-- instance Ord Double where
--     le a b = a <= b

-- instance Ord Float where
--     le a b = a <= b

-- data Floop = ...

-- ayy :: Floop -> Bool

-- id :: a -> a
-- id x = x

-- instance Ord Floop where
--     le a b = undefined

-- generate :: MonadRandom m => Int -> Integer -> m (PublicKey, PrivateKey)

createPerson :: IO Person
createPerson = do
    (pub, priv) <- RSA.generate rsaBitLen rsaPublicKeyExponent
    return $ Person { publicKey = pub, privateKey = priv }

mineGenesis :: RSA.PublicKey -> Int32 -> Block
mineGenesis myPubKey timestamp = mineGenesisAux myPubKey timestamp 0

-- make `createTransaction` function that takes `src`, `amount`, and `dest`, and
-- it finds transactions in `src`s wallet to back the new transaction

mineGenesisAux :: RSA.PublicKey -> Int32 -> Int32 -> Block
mineGenesisAux myPubKey timestamp nonce =
    let currBlock = Block {
          timestamp = timestamp,
          prevHash = hashFinalize (hashInitWith SHA512),
          nonce = nonce,
          transactions = [
              Transaction {
                  inputs = [],
                  amount = 1,
                  mainOutput = myPubKey,
                  changeOutput = myPubKey
              }]
        }
        currHash = hashBlock currBlock
    in
        trace ("timestamp = " ++ show timestamp ++ ", nonce = " ++ show nonce ++ ", hash = " ++ show currHash)
        -- check if has the right number of zeros
        (if hasNLeadingZeros currHash 2
        -- if so, that's our block
        then currBlock
        -- otherwise, just increment the nonce and try hashing again
        else mineGenesisAux myPubKey timestamp (nonce + 1))

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





--     let timestamp =
-- mineGenesisAux timestamp nonce =
--     let currBlock = Block { timestamp = timestamp, prevHash = hashFinalize (hashInitWith SHA512), nonce = nonce, transactions = [] }
--         currHash = hashBlock currBlock

-- let currBlock = Block {
--     timestamp = timestamp,
--     prevHash = hashFinalize (hashInitWith SHA512),
--     nonce = nonce,
--     transactions = []
-- }

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