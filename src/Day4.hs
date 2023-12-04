module Day4 where

import Day
import Data.Char (isDigit)
import Debug.Trace (trace)
import qualified Data.Set as S
import qualified Data.Map as M

data Card = Card 
    { number :: Int
    , winning :: S.Set Int
    , hand :: [Int]
    } deriving (Show)

type Table = M.Map Int (Int, Card)

parseCard :: Parser Card
parseCard = do
    lexeme $ string "Card"
    number <- decimal
    lexeme $ string ":"
    winning <- many $ lexeme $ decimal
    lexeme $ string "|"
    hand <- many $ lexeme $ decimal
    return $ Card number (S.fromList winning) hand

part1 :: [Card] -> Int
part1 cards = (sum . map countPoints) cards
  where
    countPoints :: Card -> Int
    countPoints c = let wins = countWins c in 
        if wins == 0 then 0 else 2 ^ (wins - 1)  

countWins :: Card -> Int
countWins (Card _ winning hand) = (length . filter (`S.member` winning)) hand

fromCardList :: [Card] -> Table
fromCardList cards = M.fromList [(number card, (1, card)) | card <- cards]

countCards :: [Card] -> Int
countCards cards = 
    let 
        finishedTable = foldl updateTable (fromCardList cards) cards 
    in
        M.foldr (\(count, _) acc -> acc+count) 0 finishedTable
  where
    updateTable :: Table -> Card -> Table
    updateTable table card@(Card cardNr _ _) = 
        let
            wins = countWins card
            cardCount = fst (table M.! cardNr)
        in
            foldl   
                (\table cardNr -> M.adjust (\(count, card) -> (count+cardCount, card)) cardNr table) 
                table 
                [cardNr+1..cardNr+wins]
    

day4 :: Day [Card] Int Int
day4 = Day
    4
    (many $ lexeme parseCard)
    part1
    countCards