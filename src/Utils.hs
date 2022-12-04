{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Utils where

import Data.List (intercalate, sort, intersect)
import Data.Char (isLower)
import Data.List.Split
----day 1
getCalsPerElf :: String -> IO [Int]
getCalsPerElf path = readFile path >>= (\x -> return $ reverse $ sort ( map (foldr (\a acc -> (read a :: Int) + acc ) 0 . splitOn  ",") $ splitOn  ",," (  intercalate "," (lines x))))

day1p1 :: IO Int
day1p1 = head <$> getCalsPerElf "inputs/day1.txt"

day1p2 :: IO Int
day1p2 = sum . take 3 <$> getCalsPerElf "inputs/day1.txt"
-------day 2

data Outcome = Win | Loss | Draw deriving (Eq)

data Move = Rock | Paper | Scissors deriving (Eq, Enum, Bounded)

encoding :: String -> Move
encoding x | x == "A" || x == "X" = Rock
           | x == "B" || x == "Y" = Paper
           | x == "C" || x == "Z" = Scissors

wrapSucc :: (Enum a, Bounded a, Eq a) => a -> a
wrapSucc x | x == maxBound = minBound
           | otherwise = succ x

moveForOutcome :: Outcome -> Move -> Move
moveForOutcome outcome opponent | outcome == Draw = opponent
                                | outcome == Loss = wrapSucc $ wrapSucc opponent
                                | otherwise = wrapSucc opponent

play :: (Move, Move) -> Outcome

play moves@(opponent, player) | opponent == player = Draw
                              | player == moveForOutcome Win opponent = Win
                              | otherwise = Loss


outcomeEncoding :: String -> Outcome
outcomeEncoding "X" = Loss
outcomeEncoding "Y" = Draw
outcomeEncoding "Z" = Win

points :: Outcome -> Int
points Win = 6
points Loss = 0
points Draw = 3

pointsForMove :: Move -> Int
pointsForMove Rock = 1
pointsForMove Paper = 2
pointsForMove Scissors = 3

codes ::  IO [(String, String)]
codes = readFile "inputs/day2.txt" >>= return . (map ((\x -> ( head x, head $ tail x)) . words) . lines)

day2p1 :: IO Int
day2p1 = sum . map (\(opponent, player) -> pointsForMove (encoding player) + points (play (encoding opponent, encoding player)) ) <$> codes

day2p2 :: IO Int
day2p2 = sum . map (\(opponent, outcome) -> points (outcomeEncoding outcome) + pointsForMove (moveForOutcome (outcomeEncoding outcome) (encoding opponent))) <$> codes


-----day 3

splitIntoGroups :: Int -> [a] -> [[a]]
splitIntoGroups _ [] = []
splitIntoGroups n xs = g : splitIntoGroups n gs where (g, gs) = splitAt n xs

intersect3 :: Eq a => [a] -> [a] -> [a] -> [a]
intersect3 as bs cs = as `intersect` bs `intersect` cs

sacks :: IO [String]
sacks = lines <$> readFile "inputs/day3.txt"

getRepeating :: String -> Char
getRepeating xs = head $ intersect fh sh where
    fh = take midway xs
    sh = drop midway xs
    midway = length xs `div` 2

priority :: Char -> Int
priority x | isLower x = fromEnum x - 96
           | otherwise = fromEnum x - 38

day3p1 :: IO Int
day3p1 = sum . map (priority . getRepeating) <$> sacks

day3p2 :: IO Int
day3p2 = sum . map (\[a, b, c] -> priority $ head $ intersect3 a b c) . splitIntoGroups 3 <$> sacks

--day 4

tuplify :: String -> (Int, Int)
tuplify s = (from, to) where
    from = read (head $ splitOn "-" s) :: Int
    to = read (head $ tail $ splitOn "-" s) :: Int

rangeContains :: (Int, Int) -> (Int, Int) -> Bool
rangeContains (fromA, toA) (fromB, toB) = fromA <= fromB && toA >= toB || fromB <= fromA && toB >= toA

rangeOverlaps :: (Int, Int) -> (Int, Int) -> Bool
rangeOverlaps as bs =  not (null ([smA .. lgA] `intersect` [smB .. lgB])) where
    smA = uncurry min as
    lgA = uncurry max as
    smB = uncurry min bs
    lgB = uncurry max bs

ranges :: IO [[(Int, Int)]]
ranges = (readFile "inputs/day4.txt" >>= return . map (splitOn ",") . lines) >>= return . map (map tuplify)

day4p1 :: IO Int
day4p1 = sum . map (\x -> if rangeContains (head x) (head $ tail x) then 1 else 0) <$> ranges

day4p2 :: IO Int
day4p2 = sum . map (\x -> if rangeOverlaps (head x) (head $ tail x) then 1 else 0) <$> ranges