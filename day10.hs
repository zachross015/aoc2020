import Data.List
import Data.Function (fix)

adapterRating :: Integral a => [a] -> a
adapterRating = (+3) . maximum

differencesOf :: Integral a => [a] -> [(a, Integer)]
differencesOf a = map (foldr (\x (_, acc) -> (x, acc + 1) ) (0, 0)) . groupBy (==) $ za
    where sa = sort a
          za = sort . zipWith (\a b -> b - a) sa $ (drop 1 sa)

memoize :: (Int -> Int) -> (Int -> Int)
memoize f = (map f [0..] !!)

adapterPathCount :: [Int] -> Int -> (Int -> Int) -> Int ->  Int
adapterPathCount adapters end mem start 
    | start == end = 1
    | otherwise = (foldr (+) 0) . (map mem) $ connected
    where connected = filter (\x -> x >= (start - 3) && x <= (start - 1)) adapters

memoAdapterPathCount :: [Int] -> Int -> Int -> Int
memoAdapterPathCount adapters end = fix (memoize . (adapterPathCount adapters end))

main = do
    contents <- readFile "inputs/day10.txt"
    let inputs = map read . lines $ contents
    let adapters = (inputs ++ [0, adapterRating inputs])
    let diff = differencesOf adapters
    print $ foldr ((*) . snd) 1 diff
    print $ memoAdapterPathCount adapters 0 (adapterRating inputs)
