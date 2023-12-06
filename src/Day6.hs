module Day6 where

import Day

type Race = (Int, Int)

parser :: Parser [Race]
parser = do
    symbol "Time:"
    times <- parseIntList
    symbol "Distance:"
    dists <- parseIntList
    return $ zip times dists
  where
    parseIntList = many $ decimal <* spaces

countWins :: Race -> Int
countWins (t, d) = (length . filter (>d) . map (distCharge t)) (enumFromTo 1 (t - 1))
  where
    distCharge :: Int -> Int -> Int
    distCharge maxTime chargeTime
        | chargeTime >= maxTime = 0
        | chargeTime == 0 = 0 
        | otherwise = (maxTime - chargeTime)*chargeTime

concatTuples :: (Monoid a, Monoid b) => [(a, b)] -> (a, b)
concatTuples = go mempty
  where
    go acc [] = acc
    go (x', y') ((x, y):xs) = go (x' <> x, y' <> y) xs 

convertToPart2 :: [Race] -> [Race]
convertToPart2 races = 
    [((\(x, y) -> (read x, read y)) 
    . concatTuples 
    . map (\(x, y) -> (show x, show y))) races] 

part1 :: [Race] -> Int
part1 = product . map countWins

day6 :: Day [Race] Int Int
day6 = Day
    6
    parser
    part1
    (part1 . convertToPart2)