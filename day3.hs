
ski :: (Int, Int) -> [[Int]] -> Int
ski (n, m) x = sum . map (ski' n) $ zip [0..] x
    where ski' c (x, y) = if ((mod x c) /= 0) then 0 else y !! (mod (m * (div x c)) (length y))


input :: String -> [[Int]]
input = map (map (\x -> if x == '#' then 1 else 0)) . lines 

part1 = [(1,3)]
part2 = [(1,1), (1,3), (1,5), (1,7), (2,1)]

main = do 
    contents <- readFile "inputs/day3.txt"
    print $ product . map (\f -> f $ input contents) $ map (\a -> ski a) part1
    print $ product . map (\f -> f $ input contents) $ map (\a -> ski a) part2
