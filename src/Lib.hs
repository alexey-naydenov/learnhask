module Lib where

import Util
import Data.Char (toUpper, toLower)
import Data.List (intercalate)

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


accumulateCapitalized :: String -> [(String, String)] -> [(String, String)]
accumulateCapitalized "" words = words
accumulateCapitalized word words = (word, capitalise word) : words

capitalizeWords :: String -> [(String, String)]
capitalizeWords text = foldr accumulateCapitalized [] words
  where words = split ' ' text

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (c:cs)
  | c == ' ' = ' ' : capitalizeWord cs
  | otherwise = toUpper c : cs

capitalizeParagraph :: String -> String
capitalizeParagraph text = intercalate "." sentences
  where sentences = map capitalizeWord $ split '.' text

notThe :: String -> String
notThe "the" = "a"
notThe x = x

replaceThe :: String -> String
replaceThe text  = intercalate " " words
  where words = map notThe $ split ' ' text

isVowel :: Char -> Bool
isVowel x
  | c == 'a' = True
  | c == 'e' = True
  | c == 'i' = True
  | c == 'o' = True
  | c == 'y' = True
  | c == 'u' = True
  | otherwise = False
  where c = toLower x

countVowels :: String -> Int
countVowels = length . filter isVowel

catMaybe :: [Maybe a] -> [a]
catMaybe = foldr updateAcc []
  where updateAcc Nothing xs = xs
        updateAcc (Just x) xs = x:xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr updateAcc (Just [])
  where updateAcc Nothing _ = Nothing
        updateAcc _ Nothing = Nothing
        updateAcc (Just x) (Just xs) = Just (x:xs)

lefts' :: [Either a b] -> [a]
lefts' = foldr acc []
  where acc (Left x) xs = x:xs
        acc (Right _) xs = xs

rights' :: [Either a b] -> [b]
rights' = foldr acc []
  where acc (Left _) xs = xs
        acc (Right x) xs = x:xs

partitionEither' :: [Either a b] -> ([a], [b])
partitionEither' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just $ f x

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

maybeIncrement :: Int -> Maybe (Int, Int)
maybeIncrement x
  | x < 10 = Just (x, x + 1)
  | otherwise = Nothing

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x =
  case result of
    Nothing -> []
    Just (y, z) -> y : myUnfoldr f z
  where result = f x

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr justF
  where justF x = Just (x, f x)


