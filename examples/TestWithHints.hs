module Main where

import Test.QuickCheck.TestWithHints
import Test.QuickCheck.Test

-- Faulty implementation of divisibleBy
divisibleBy :: Int -> Int -> Bool
divisibleBy x y = x `mod` y == 0
-- divisibleBy x y = error "foo"

prop_factorsDivideProduct a b = (a*b) `divisibleBy` a


main = do
  quickCheckWithHints prop_factorsDivideProduct
