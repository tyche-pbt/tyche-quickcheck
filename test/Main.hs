{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck
import Test.Tyche (labelCategory, labelNumber)
import qualified Test.Tyche as Tyche

data Tree = Leaf | Node Tree Int Tree
  deriving (Show)

toList :: Tree -> [Int]
toList Leaf = []
toList (Node l x r) = toList l ++ [x] ++ toList r

isBST :: Tree -> Bool
isBST Leaf = True
isBST (Node l x r) = all (< x) (toList l) && all (> x) (toList r) && isBST l && isBST r

insert :: Int -> Tree -> Tree
insert x Leaf = Node Leaf x Leaf
insert x (Node l y r)
  | x < y = Node (insert x l) y r
  | x > y = Node l y (insert x r)
  | otherwise = Node l y r

size :: Tree -> Int
size Leaf = 0
size (Node l _ r) = 1 + size l + size r

member :: Int -> Tree -> Bool
member _ Leaf = False
member x (Node l y r)
  | x < y = member x l
  | x > y = member x r
  | otherwise = True

instance Arbitrary Tree where
  arbitrary = aux (3 :: Int)
    where
      aux 0 = return Leaf
      aux n =
        frequency
          [ (1, return Leaf),
            ( n,
              do
                x <- arbitrary
                l <- aux (n - 1)
                r <- aux (n - 1)
                return (Node l x r)
            )
          ]

genBST :: (Int, Int) -> Gen Tree
genBST (lo, hi) | lo > hi = return Leaf
genBST (lo, hi) =
  frequency
    [ (1, return Leaf),
      ( 1,
        do
          x <- choose (lo, hi)
          l <- genBST (lo, x - 1)
          r <- genBST (x + 1, hi)
          return (Node l x r)
      )
    ]

prop_insertValid :: Property
prop_insertValid =
  Tyche.visualize "prop_insert_valid" $
    forAll ((,) <$> arbitrary <*> genBST (-10, 10)) $ \(x, t) ->
      label ("size:" ++ show (size t)) $
        isBST (insert x t)

prop_insertPost :: Int -> Tree -> Property
prop_insertPost x t =
  Tyche.visualize "prop_insert_post" $
    labelNumber "size" (size t) $
      labelNumber "value" x $
        labelCategory "isBST" (show (isBST t)) $
          isBST t
            ==> member x (insert x t)

return []

main :: IO Bool
main = $quickCheckAll