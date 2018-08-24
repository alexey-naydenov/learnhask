module Lib where

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
merge xss@(x:xs) yss@(y:ys) = if x < y then x : merge xs yss else y : merge xss ys

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

