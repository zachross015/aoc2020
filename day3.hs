
ski :: (Int, Int) -> [[Int]] -> Int
ski (dx, dy) inp = sum . map ski' $ zip inp [0..]
    where ski' (x, y) = if ((mod y dy) /= 0) 
                          then 0 
                          else x !! (mod (dx * (div y dy)) (length x))


input :: String -> [[Int]]
input = map (map (\x -> if x == '#' then 1 else 0)) . lines 

part1 = [(3,1)]
part2 = [(1,1), (3,1), (5,1), (7,1), (1,2)]


day2 c = product . map ((\f -> f $ input c) . (\a -> ski a))

main = do 
    contents <- readFile "inputs/day3.txt"
    print $ day2 contents part1
    print $ day2 contents part2
