
preamble :: [a] -> [a]
preamble = take 25

element :: [a] -> a
element = head . drop 25

twoCombinationSums :: (Num a, Eq a) => [a] -> [a]
twoCombinationSums xs = [a + b | a <- xs, b <- xs, a /= b]

elementIsValid :: (Num a, Eq a) => [a] -> a -> Bool
elementIsValid p e =  e `elem` (twoCombinationSums p)

nonValidElement :: (Num a, Eq a) => [a] -> a
nonValidElement (x:xs) = if elementIsValid p e then nonValidElement xs else e
    where p = preamble (x:xs)
          e = element (x:xs)

contiguousSublist :: (Num a, Ord a) => a -> [a] -> Maybe [a]
contiguousSublist n (x:xs) = if (n - x) < 0 
                                then Nothing 
                                else (if (n - x) == 0 
                                    then Just [x]
                                    else fmap (x:) (contiguousSublist (n - x) xs))

contiguousList :: (Num a, Ord a) => a -> [a] -> [a]
contiguousList n (x:xs) = contiguousList' (contiguousSublist n (x:xs))
    where contiguousList' Nothing = contiguousList n (xs)
          contiguousList' (Just a) = a

main = do
    contents <- readFile "inputs/day9.txt"
    let inputs = map read . lines $ contents
    let nonValid = nonValidElement inputs
    let cl = contiguousList nonValid inputs 
    print $ nonValid
    print $ (minimum cl) + (maximum cl)
