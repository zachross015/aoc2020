import Data.List

binaryArrayToInt :: [Int] -> Int
binaryArrayToInt = foldl (\acc x -> acc * 2 + x) 0

seatNumber :: String -> Int
seatNumber line = row * 8 + column
    where bArr = map (\c -> if c == 'F' || c == 'L' then 0 else 1) line
          row = binaryArrayToInt . take 7 $ bArr
          column = binaryArrayToInt . drop 7 $ bArr

seats :: String -> [Int]
seats = map seatNumber . lines

part1 :: String -> Int
part1 = maximum . seats

part2 :: String -> Int
part2 inp = head $ [(minimum s)..(maximum s)] \\ s
    where s = seats inp

main = do
    contents <- readFile "inputs/day5.txt"
    print $ part1 contents
    print $ part2 contents

