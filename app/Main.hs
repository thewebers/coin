module Main where

import Data.Time.Clock.POSIX

import Lib

main :: IO ()
-- main = do
--   person <- createPerson
--   print person
--   mineGenesis

main = do
  timestamp <- round `fmap` getPOSIXTime
  let block = mineGenesis timestamp
  print block