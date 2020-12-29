{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import Data.Bits ( Bits(xor) )
import Data.Int ( Int32 )

import qualified Crypto.PubKey.RSA
import qualified Crypto.Hash

type Hash = Int32
-- type Nonce = Int32

rsaBitLen = 512
rsaPrime = 3

max' :: [Int] -> Int
max' [x] = x
max' (x:xs)
    | max' xs > x = max' xs
    | otherwise = x

xor' :: [Int32] -> Int32
xor' = foldl xor 0

data Block = Block {
    timestamp :: Int32,
    prevHash :: Int32,
    nonce :: Int32,
    transactions :: [Transaction]
} deriving (Show)
-- that auto generates a method `show` that we can use to get a string representation of the data type
-- using some metaprogramming shit or something

-- there's another haskellism too that cleans code up.
-- when you have something that's a parenthesis soup, like (getFolds terry (numFolds (belly terry)))
-- you can remove some of those by using `$`, like `getFolds terry $ numFolds $ belly terry`
-- basically, whenever you have `func (<some complicated expression with lots of
-- parens>)`, you can clean it up as `func $ <complicated expr>`, and you can do that recursively.

-- schön

data Transaction = Transaction {
    -- ident :: Int32,
    inputs :: [Transaction],
    amount :: Int32,
    output :: Person,
    change :: Person
} deriving (Show)
data Person = Person {
    publicKey :: Int32,
    privateKey :: Int32
} deriving (Show)
data Chain = Chain {
    blocks :: [(Hash, Block)]
} deriving (Show)

getLatestBlock :: Chain -> Block
getLatestBlock chain = snd $ head $ blocks chain

getWallet :: Person -> [Transaction]
getWallet = undefined

hashBlock :: Block -> Int32
hashBlock block = hashUpdates hashInit []
-- hashBlock block = xor' ([prevHash block, nonce block] ++ map hashTransaction (transactions block))

hashTransaction :: Transaction -> Int32
hashTransaction t = xor' ([amount t, hashPerson (output t), hashPerson (change t)] ++ map hashTransaction (inputs t))

hashPerson :: Person -> Int32
hashPerson p = xor' [publicKey p, privateKey p]
createPerson = 
    let ((pub, priv), g1) = generate g rsaBitLen rsaPrime
    in Person { publicKey = pub, privateKey = priv }

mineGenesis timestamp = mineGenesisAux timestamp 0

    -- whoever mined it
    let currBlock = Block { timestamp, prevHash = 0, nonce, transactions = [] }
    in
        -- check if has the right number of zeros
        if hasNLeadingZeros (hashBlock currBlock) 1
        -- if so, that's our block
        then currBlock
        -- otherwise, just increment the nonce and try hashing again
        else mineGenesisAux timestamp (nonce + 1)

{-
for nonce in nonce_candidates:
    if str(hash(Block { prevHash = prevHash, nonce = nonce, transactions = transactions })).startswith('0' * num_required_zeros):
        print("we've have mined it")
-}

-- TODO shouldn't need to convert to string to impl this, because slow
hasNLeadingZeros :: Int32 -> Int32 -> Bool
hasNLeadingZeros x n =
    let xStr = show x
        numZeros = fromIntegral (32 - length xStr) :: Int32
    in numZeros >= n