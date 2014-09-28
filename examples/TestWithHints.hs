module Main where

import Test.QuickCheck.TestWithHints
import Test.QuickCheck.Test

-- Faulty implementation of divisibleBy
divisibleBy :: Int -> Int -> Bool
divisibleBy x y = x `mod` y == 0
-- divisibleBy x y = error "foo"

prop_factorsDivideProduct a b = (a*b) `divisibleBy` a

prop_id :: Int -> Bool
prop_id x = (id x) /= x

main = do
       quickCheck prop_id
--  quickCheckWithHints prop_factorsDivideProduct
