import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S

structureList :: (Ord a) => (Set a -> Set a -> Set a) -> [a] -> [[a]] -> Set a
structureList f i p = foldr f (S.fromList i) (map S.fromList p)

day6 :: ([String] -> Set Char) -> String -> Int
day6 f = sum . map (S.size . f) . map lines . splitOn "\n\n"

part1 :: [String] -> Set Char
part1 = structureList S.union []

part2 :: [String] -> Set Char
part2 = structureList S.intersection ['a'..'z']

main = do 
    contents <- readFile "inputs/day6.txt"
    print $ day6 part1 contents
    print $ day6 part2 contents
