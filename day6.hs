import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S

part1 :: [String] -> Set Char
part1 p = foldr (\x acc -> S.union (S.fromList x) acc) S.empty p

part2 :: [String] -> Set Char
part2 p = foldr (\x acc -> S.intersection (S.fromList x) acc) (S.fromList ['a'..'z']) p

input = map lines . splitOn "\n\n"
day6 f = sum . map (S.size . f) . input

main = do 
    contents <- readFile "inputs/day6.txt"
    print $ day6 part1 contents
    print $ day6 part2 contents
