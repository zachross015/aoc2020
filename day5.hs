import Data.List

binaryArrayToInt :: [Int] -> Int
binaryArrayToInt = foldl (\acc x -> acc * 2 + x) 0

row :: String -> Int
row = binaryArrayToInt . map (\c -> if c == 'F' then 0 else 1) . take 7

column :: String -> Int
column = binaryArrayToInt . map (\c -> if c == 'L' then 0 else 1) . drop 7

seatNumber :: String -> Int
seatNumber line = (row line) * 8 + (column line)

seats :: String -> [Int]
seats = map seatNumber . lines

part1 :: String -> Int
part1 = maximum . seats

part2 :: String -> Int
part2 inp = head $ [n..m] \\ seats inp
    where m = maximum . seats $ inp
          n = minimum . seats $ inp

main = do
    contents <- readFile "inputs/day5.txt"
    print $ part1 contents
    print $ part2 contents

