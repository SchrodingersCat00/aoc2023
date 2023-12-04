module Day1 where

import Data.List (find, isInfixOf)
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Day

-- Dont ask
replaceNumberStrings :: String -> String
replaceNumberStrings ('o':'n':'e':xs) = '1':(replaceNumberStrings ('e':xs))
replaceNumberStrings ('t':'w':'o':xs) = '2':(replaceNumberStrings ('o':xs))
replaceNumberStrings ('t':'h':'r':'e':'e':xs) = '3':(replaceNumberStrings ('e':xs))
replaceNumberStrings ('f':'o':'u':'r':xs) = '4':(replaceNumberStrings xs)
replaceNumberStrings ('f':'i':'v':'e':xs) = '5':(replaceNumberStrings ('e':xs))
replaceNumberStrings ('s':'i':'x':xs) = '6':(replaceNumberStrings xs)
replaceNumberStrings ('s':'e':'v':'e':'n':xs) = '7':(replaceNumberStrings ('n':xs))
replaceNumberStrings ('e':'i':'g':'h':'t':xs) = '8':(replaceNumberStrings ('t':xs))
replaceNumberStrings ('n':'i':'n':'e':xs) = '9':(replaceNumberStrings ('e':xs))
replaceNumberStrings (x:xs) = x:replaceNumberStrings xs
replaceNumberStrings [] = []

part :: [String] -> Int
part ls = sum $ do
    l <- ls
    let 
        firstNum = (fromJust . find isDigit) l
        lastNum = (fromJust . find isDigit . reverse) l
        numString = firstNum:lastNum:[]
        num = read numString
    return num


day1 :: Day [String] Int Int
day1 = Day
    1
    (makeParser lines)
    part
    (part . map replaceNumberStrings)