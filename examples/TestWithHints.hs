module Main where

import Test.QuickCheck.TestWithHints
import Test.QuickCheck
import System.Random ( Random )
import Control.Applicative

data Tree a = Leaf | Node (Tree a) a (Tree a)
            deriving Show

insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf x = Node Leaf x Leaf
insert (Node left y right) x  
    | x < y = Node (insert left x) y right
    | x > y = Node left y (insert right x)
    | otherwise = (Node left y right)

contains :: (Ord a) => Tree a -> a -> Bool
contains Leaf x = False
contains (Node left y right) x
    | x < y = contains left x
    | x > y = contains right x -- OOPS
    | otherwise = True

flatten :: (Ord a) => Tree a -> [a]
flatten Leaf = []
flatten (Node left y right) = (flatten left) ++ (y : (flatten right))

instance (Ord a,Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = (foldl insert Leaf) <$> arbitrary

prop_treeContainsInserted :: Tree Int -> Int -> Bool
prop_treeContainsInserted tree x = contains (insert tree x) x

isOrdered :: (Ord a) => [a] -> Bool
isOrdered list = and $ zipWith (<=) list (drop 1 list)

prop_treeIsOrdered :: Tree Int -> Bool
prop_treeIsOrdered tree = isOrdered $ flatten tree


tests = prop_treeContainsInserted

args = HintArgs { mixPath = ["./dist/hpc/mix/test-quickcheck","./dist/hpc/mix/QuickCheck-2.7.6"]
                , moduleNames = ["Main"] }

main = do
       quickCheckWithHints args tests
