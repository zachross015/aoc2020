slope (width, height) (dx, dy) = [(dy * i * width) + (mod (dx * i) width) | i <- [0..height], dy * i < height]

ski :: [Int] -> [Int] -> Int
ski hill = sum . map (\x -> hill !! x)

input :: String -> ([Int], Int, Int)
input inp = (map (\x -> if x == '#' then 1 else 0) (filter (\x -> x /= '\n') inp), width inp, height inp)
    where height = length . lines
          width = length . head . lines

part1 = [(3,1)]
part2 = [(1,1), (3,1), (5,1), (7,1), (1,2)]

day3 part content = let (hill, width, height) = input content in 
        product . map ((ski hill) . slope (width, height)) $ part

main = do 
    contents <- readFile "inputs/day3.txt"
    print $ day3 part1 contents
    print $ day3 part2 contents
