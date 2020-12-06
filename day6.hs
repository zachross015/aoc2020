import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S

structureList f init p = foldr (\x acc -> f (S.fromList x) acc) init p

day6 f = sum . map (S.size . f) . map lines . splitOn "\n\n"

part1 :: [String] -> Set Char
part1 = structureList S.union S.empty

part2 :: [String] -> Set Char
part2 = structureList S.intersection (S.fromList ['a'..'z'])

main = do 
    contents <- readFile "inputs/day6.txt"
    print $ day6 part1 contents
    print $ day6 part2 contents
