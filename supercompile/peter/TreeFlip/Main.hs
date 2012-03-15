module Main where

import System.Environment

main :: IO ()
main = do
    [n] <- fmap (map read) getArgs
    print (root n)

flip t = case t of
    (Leaf x) -> Leaf x
    (Branch l r) -> Branch (flip r) (flip l)

sumtr t = case t of
    Leaf x -> x
    Branch l r -> sumtr l + sumtr r

buildTree n t = case n == 0 of
    True -> t
    False -> buildTree (n-1) (Branch t t)

{-# SUPERCOMPILE root #-}
root :: Int -> Int
root n = sumtr (flip (flip (buildTree n (Leaf 1))))

data Tree a = Leaf a | Branch (Tree a) (Tree a)