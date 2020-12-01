convert :: String -> [Int]
convert = map read . lines

genTuples2 :: [Int] -> [(Int, Int)]
genTuples2 x = [(y1, y2) | y1 <- x, y2 <- x]

genTuples3 :: [Int] -> [(Int, Int, Int)]
genTuples3 x = [(y1, y2, y3) | y1 <- x, y2 <- x, y3 <- x]

part1 :: [Int] -> Int
part1 = part1' . head . filter (\(x, y) -> x + y == 2020) . genTuples2
    where part1' (x, y) = x * y

part2 :: [Int] -> Int
part2 = part1' . head . filter (\(x, y, z) -> x + y + z == 2020) . genTuples3
    where part1' (x, y, z) = x * y * z


main = do
    contents <- readFile "inputs/day1"
    putStrLn $ show . part1 . convert $ contents
    putStrLn $ show . part2 . convert $ contents

