{-# LANGUAGE ViewPatterns #-}
module Lib
    ( someFunc
    ) where

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

someFunc :: IO ()
x = 20
someFunc = putStrLn (show (fib x))
