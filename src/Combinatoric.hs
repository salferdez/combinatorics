module Combinatoric
    (
      factorial
    , combination
    , permutation
    , combinationR
    , length'
    , sum'
    , permutationRepKnown
    , stirlingSecondKind

    ) where

factorial :: Integer -> Integer
factorial n
    | n < 0     = error "input must be non negative"
    | n == 0    = 1
    | otherwise = accum n 1
        where
            accum 1 a = a
            accum n a = accum (n-1) (a*n)

combination :: Integer -> Integer -> Integer
combination n k = factorial n `div` (factorial k * factorial (n-k))

permutation :: Integer -> Integer -> Integer
permutation n k = factorial n `div` factorial (n-k)

combinationR :: Integer -> Integer -> Integer
combinationR n k = combination (n+k-1) k

length' :: [a] -> Integer
length' x = helper 0 x
    where
        helper n []     = n
        helper n (x:xs) = helper (n+1) xs

sum' :: [Integer] -> Integer
sum' x = accum 0 x
    where
        accum n []     = n
        accum n (x:xs) = accum (n+x) xs

permutationRepKnown :: Integer -> [Integer] -> Integer
permutationRepKnown n (x:xs)
    | length' (x:xs) /= n = error "lenght of the list must be equal to the first parameter"
    | otherwise = factorial k `div` factProd 1 (x:xs)
        where
            k = sum' (x:xs)
            factProd  y []     = y
            factProd  y (x:xs) = factProd ( y * factorial x) xs

stirlingSecondKind :: Integer -> Integer ->  Integer
stirlingSecondKind n 1 = 1
stirlingSecondKind n 2 = ((2 ^n)- 2) `div` 2
stirlingSecondKind n k
    | k > n      = 0
    | k == n     = 1
    | k == (n-1) = combination n 2
    | otherwise  = stirling (n-1) (k-1) +  k * stirling (n-1) k
