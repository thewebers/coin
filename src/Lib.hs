{-# LANGUAGE ViewPatterns #-}

import Data.Bits

module Lib where

-- type Hash = Int
-- type Nonce = Int

{-
for nonce in nonce_candidates:
    if str(hash(Block { prevHash = prevHash, nonce = nonce, transactions = transactions })).startswith('0' * num_required_zeros):
        print("we've have mined it")
-}

getWallet :: Person -> [Transaction]
getWallet = undefined

data Block = Block {
    prevHash :: Int,
    nonce :: Int,
    transactions :: [Transaction]
}
data Transaction = Transaction {
    ident :: Int,
    amount :: Int,
    inputs :: [Transaction],
    output :: Person,
    change :: Person
}
data Person = Person {
    publicKey :: Int,
    privateKey :: Int
}

xor' :: [Int] -> Int
xor' = foldl xor 0

hashBlock :: Block -> Int
-- hashBlock block = foldl (\acc elem -> xor acc elem) 0 [(prevHash block), (nonce block), (hashTransaction )]
hashBlock block = xor' ([(prevHash block), (nonce block)] ++ [map hashTransaction (transactions block))])

hashTransaction :: Transaction -> Int
hashTransaction t = xor' [(amount t), (map )]

hashPerson :: Person -> Int
hashPerson = hash (privateKey person)
