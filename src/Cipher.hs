module Cipher where

import Data.Char

lettersInAlphabet :: Int
lettersInAlphabet = 26

combineLetters :: (Int -> Int -> Int) -> Char -> Char -> Char
combineLetters op x y = chr code
  where codeA = ord 'A'
        ix = ord x - codeA
        iy = ord y - codeA
        letterIndex = (ix `op` iy) `mod` lettersInAlphabet
        code = letterIndex + codeA

addLetters :: Char -> Char -> Char
addLetters = combineLetters (+)

subtractLetters :: Char -> Char -> Char
subtractLetters = combineLetters (-)

vigenereEncode' :: (Char -> Char -> Char) -> String -> String -> String
vigenereEncode' _ _ "" = ""
vigenereEncode' _ "" _ = undefined
vigenereEncode' op ss (' ':xs) = ' ' : vigenereEncode' op ss xs
vigenereEncode' op (s:ss) (x:xs) = op s x : vigenereEncode' op ss xs

vigenereEncode :: String -> String -> String
vigenereEncode shift text = vigenereEncode' addLetters (cycle shift) text

vigenereDecode :: String -> String -> String
vigenereDecode shift text = vigenereEncode' reverseSub (cycle shift) text
  where reverseSub = flip subtractLetters
