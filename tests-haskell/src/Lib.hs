module Lib ( slowFib, fib ) where

slowFib :: Int -> Integer
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n - 1) + slowFib (n - 2)

fib :: Int -> Integer
fib n =
    last $ go (n + 1)
  where
    go n =
      let lst = go n in
      take n $ 0 : 1 : zipWith (+) lst (tail lst)