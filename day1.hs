convert :: String -> [Int]
convert = map read . lines

genTuples2 :: [Int] -> [[Int]]
genTuples2 x = [[y1, y2] | y1 <- x, y2 <- x]

genTuples3 :: [Int] -> [[Int]]
genTuples3 x = [[y1, y2, y3] | y1 <- x, y2 <- x, y3 <- x]

day1 :: ([Int] -> [[Int]]) -> [Int] -> Int
day1 g = product . head . filter (\x -> sum x == 2020) . g

main = do
    contents <- readFile "inputs/day1.txt"
    putStrLn $ show . day1 genTuples2 . convert $ contents
    putStrLn $ show . day1 genTuples3 . convert $ contents

