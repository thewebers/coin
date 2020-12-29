{-# LANGUAGE NamedFieldPuns #-}

module Lib where

{-
Assumptions:
- each user only ever has one RSA key pair
- we're not using merkle trees to compress transactions
-}

import Debug.Trace

import Data.Bits ( Bits(xor) )
import Data.Int ( Int32 )
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import qualified Crypto.PubKey.RSA as RSA
import Crypto.Hash

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
    output :: RSA.PublicKey,
    change :: RSA.PublicKey
} deriving (Show)

data Person = Person {
    publicKey :: RSA.PublicKey,
    privateKey :: RSA.PrivateKey
} deriving (Show)

newtype Chain = Chain {
    blocks :: [(Digest SHA512, Block)]
} deriving (Show)

getLatestBlock :: Chain -> Block
getLatestBlock chain = snd $ head $ blocks chain

-- TODO:  Crawl through all unspent transactions sent to a given person.
getWallet :: Person -> [Transaction]
getWallet person = undefined

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
        ctx'' = hashPubKey' (output t) ctx'
        ctx''' = hashPubKey' (change t) ctx''
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
    -- let ((pub, priv), g1) =
    return $ Person { publicKey = pub, privateKey = priv }

mineGenesis :: Int32 -> Block
mineGenesis timestamp = mineGenesisAux timestamp 0

mineGenesisAux :: Int32 -> Int32 -> Block
mineGenesisAux timestamp nonce =
    let currBlock = Block { timestamp = timestamp, prevHash = hashFinalize (hashInitWith SHA512), nonce = nonce, transactions = [] }
        currHash = hashBlock currBlock
    in
        trace ("timestamp = " ++ show timestamp ++ ", nonce = " ++ show nonce ++ ", hash = " ++ show currHash)
        -- check if has the right number of zeros
        (if hasNLeadingZeros currHash 4
        -- if so, that's our block
        then currBlock
        -- otherwise, just increment the nonce and try hashing again
        else mineGenesisAux timestamp (nonce + 1))

-- Mine
mine :: [Transaction] -> Chain -> Chain
mine chain txs = undefined
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