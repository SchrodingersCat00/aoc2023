module Day2 where

import Data.Char (isDigit)
import Day

data Hand = Hand
    { red :: Int
    , green :: Int
    , blue :: Int } deriving (Show)

type Game = [Hand]

emptyHand = Hand { red = 0, green = 0, blue = 0 }

sumHands :: Hand -> Hand -> Hand
sumHands (Hand r1 g1 b1) (Hand r2 g2 b2) = Hand (r1+r2) (g1 + g2) (b1 + b2)

parseGame :: Parser Game
parseGame = do
    string "Game "
    many (satisfy isDigit)
    char ':'
    parseHand `sepBy` (string "; ")

blueParser :: Parser Hand
blueParser = do
    x <- decimal
    space
    string "blue"
    return $ Hand 0 0 x

redParser :: Parser Hand
redParser = do
    x <- decimal
    space
    string "red"
    return $ Hand x 0 0

greenParser :: Parser Hand
greenParser = do
    x <- decimal
    space
    string "green"
    return $ Hand 0 x 0

parseHand :: Parser Hand
parseHand = do
    spaces
    colors <- (choice [greenParser, blueParser, redParser]) `sepBy` (string ", ")
    let summedHand = foldr sumHands emptyHand colors
    return summedHand


day2 :: Day [Game] Int Int
day2 = Day
    2
    (parseGame `sepBy` newline)
    (\_ -> 1)
    (\_ -> 1)