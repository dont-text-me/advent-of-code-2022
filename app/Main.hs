module Main (main) where

import Utils (day1p1, day1p2, day2p1, day2p2, day3p1, day3p2, day4p1, day4p2)

days :: [IO Int]
days = [day1p1, day1p2, day2p1, day2p2, day3p1, day3p2, day4p1, day4p2]

main :: IO ()
main = do
    putStrLn "choose day"
    day <- read <$> getLine
    putStrLn "choose part"
    part <- read <$> getLine
    ans <- days !! (day + part + 1)
    print ans
