import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S


input = map lines . splitOn "\n\n"

validatedPuzzles :: [String] -> Set Char
validatedPuzzles p = foldr (\x acc -> S.union (S.fromList x) acc) S.empty p

validatedPuzzles2 :: [String] -> Set Char
validatedPuzzles2 p = foldr (\x acc -> S.intersection (S.fromList x) acc) (S.fromList ['a'..'z']) p

part1 :: String -> Int
part1 = sum . map (S.size . validatedPuzzles) . input

part2 :: String -> Int
part2 = sum . map (S.size . validatedPuzzles2) . input

main = do 
    contents <- readFile "inputs/day6.txt"
    print $ part1 contents
    print $ part2 contents
