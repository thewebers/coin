{-# LANGUAGE ViewPatterns #-}

module Lib
    ( someFunc
    ) where

-- Fibonacci
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- ...
someFunc :: IO ()
x = 22
someFunc = putStrLn (show (fib x))

-- ...

