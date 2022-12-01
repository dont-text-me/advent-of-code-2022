import Data.List (intercalate, sort)
import Data.Text (splitOn, pack, unpack, Text)

getCalsPerElf :: String -> IO [Int]
getCalsPerElf path = readFile path >>= (\x -> return $ reverse $ sort ( map (foldr (\a acc -> (read (unpack a) :: Int) + acc ) 0 . splitOn (pack ",")) $ splitOn (pack ",,") (pack  (intercalate "," (lines x)))))

day1p1 :: IO Int
day1p1 = head <$> getCalsPerElf "inputs/day1.txt"

day1p2 :: IO Int
day1p2 = sum . take 3 <$> getCalsPerElf "inputs/day1.txt"