import Data.List (intercalate, sort)
import Data.Text (splitOn, pack, unpack, Text)
import Distribution.Simple.Utils (xargs)


----day 1
getCalsPerElf :: String -> IO [Int]
getCalsPerElf path = readFile path >>= (\x -> return $ reverse $ sort ( map (foldr (\a acc -> (read (unpack a) :: Int) + acc ) 0 . splitOn (pack ",")) $ splitOn (pack ",,") (pack  (intercalate "," (lines x)))))

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
codes = readFile "inputs/day2.txt" >>= return . map ((\x -> ( head x, head $ tail x)) . words) . lines

day2p1 :: IO Int
day2p1 = sum . map (\(opponent, player) -> (pointsForMove (encoding player)) + (points $ play (encoding opponent, encoding player)) ) <$> codes

day2p2 :: IO Int
day2p2 = sum . map (\(opponent, outcome) -> (points $ outcomeEncoding outcome) + (pointsForMove $ moveForOutcome (outcomeEncoding outcome) (encoding opponent))) <$> codes
