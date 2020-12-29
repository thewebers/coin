module Main where

import Data.Time.Clock.POSIX

import Lib

main :: IO ()
-- main = do
--   person <- createPerson
--   print person
--   mineGenesis

-- Test case:
-- user A mines one coin
-- in the next block, A tries to send 2 coins to B
-- and the transaction is rejected

-- main = do
--   person <- createPerson
--   print person
--   timestamp <- round `fmap` getPOSIXTime
--   let block = mineGenesis (publicKey person) timestamp
--   print block

mineLoop :: Person -> Chain -> IO Chain
mineLoop person chain = do
  timestamp <- round `fmap` getPOSIXTime
  putStrLn "ayy lmao"
  let chain = mine person timestamp [] chain
  print $ length (blocks chain)
  print $ "hash = " ++ show (head (blocks chain))
  mineLoop person chain


main = do
  person <- createPerson
  -- print person
  -- let block = mine person timestamp []
  mineLoop person (Chain { blocks = [] })
  return ()