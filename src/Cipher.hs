module Cipher where

import Data.Char

lettersInAlphabet :: Int
lettersInAlphabet = 26

addLetters :: Char -> Char -> Char
addLetters x y = chr code
  where codeA = ord 'A'
        ix = ord x - codeA
        iy = ord y - codeA
        letterIndex = (ix + iy) `mod` lettersInAlphabet
        code = letterIndex + codeA

vigenereEncode :: String -> String -> String
vigenereEncode _ "" = ""
vigenereEncode shiftWord text = fmap (uncurry shiftLetter) shiftAndText
  where shiftAndText = zip (cycle shiftWord) text
