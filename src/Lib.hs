module Lib where

import Util

someFunc :: IO ()
someFunc = putStrLn "someFunc"

sumSquares :: Int -> Int
sumSquares n = sum [i^2 | i <- [1..n]]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [x..n], z <- [y..n],
           x^2 + y^2 == z^2]

euclid :: Int -> Int -> Int
euclid a b | a == 0 = 0
           | b == 0 = 0
           | a == b = a
           | a < b = euclid a (b - a)
           | otherwise = euclid (a - b) b

myand :: [Bool] -> Bool
myand [] = True
myand (x:xs) = x && myand xs

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xss@(x:xs) yss@(y:ys) = if x < y
  then x : merge xs yss else y : merge xss ys

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve [x] = ([x], [])
halve (x1:x2:xs) = (x1:xs1, x2:xs2)
  where (xs1, xs2) = halve xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort xs1) (msort xs2)
  where (xs1, xs2) = halve xs

filtermap1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtermap1 f p xs = [f x | x <- xs, p x]

filtermap2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtermap2 f p = map f . filter p 

mymap :: (a -> b) -> [a] -> [b]
mymap f = foldr ((:) . f) []

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter p xs = foldr (\x -> if p x then ((:) x) else id) [] xs

dec2int :: [Int] -> Int
dec2int ds = foldl ((+) . (10*)) 0 ds

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f _ [x] = [f x]
altMap f g (x1:x2:xs) = f x1 : g x2 : altMap f g xs

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xss@(x:xs) (y:ys)
  | x == y = isSubseqOf xs ys
  | otherwise = isSubseqOf xss ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords "" = []
capitalizeWords w = [(w, capitalise w)]


