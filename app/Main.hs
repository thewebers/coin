module Main where

import Data.Time.Clock.POSIX

import Lib

main :: IO ()
main = do
  print createPerson

-- main = do
--   timestamp <- round `fmap` getPOSIXTime
--   let block = mineGenesis timestamp
--   print block