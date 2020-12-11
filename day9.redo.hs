
preamble :: [a] -> [a]
preamble = take 25

element :: [a] -> a
element = head . drop 25

twoCombinationSums :: (Num a, Eq a) => [a] -> [a]
twoCombinationSums xs = [a + b | a <- xs, b <- xs, a /= b]

elementIsValid :: (Num a, Eq a) => [a] -> a -> Bool
elementIsValid p e = e `elem` (twoCombinationSums p) 

nonValidElement :: (Num a, Eq a) => [a] -> a
nonValidElement (x:xs) = if elementIsValid p e then nonValidElement xs else e
    where p = preamble (x:xs)
          e = element (x:xs)

slidingWindow n s xs = if (length xs) < s
                       then []
                       else (if (sum . take s) xs == n 
                             then take s xs 
                             else slidingWindow n s (tail xs))

contiguousList :: (Num a, Ord a) => a -> [a] -> [a]
contiguousList n xs = head . filter (/=[]) . map (\x -> slidingWindow n x xs) $ [1..(length xs)]

main = do
    contents <- readFile "inputs/day9.txt"
    let inputs = map read . lines $ contents
    let nonValid = nonValidElement inputs
    let cl = contiguousList nonValid (filter (<nonValid) inputs)
    print $ nonValid
    print $ (minimum cl) + (maximum cl)
