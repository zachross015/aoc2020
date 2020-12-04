import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split


necessaryValues = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
eyeColors = Set.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
hexValues = Set.fromList (['0'..'9'] ++ ['a'..'f'])

inRange :: (Ord a) => a -> a -> a -> Bool
inRange x y z = x <= y && y <= z

propertyValidation :: String -> String -> Bool
propertyValidation "byr" x = inRange 1920 (read x :: Int) 2002
propertyValidation "iyr" x = inRange 2010 (read x :: Int) 2020
propertyValidation "eyr" x = inRange 2020 (read x :: Int) 2030
propertyValidation "hgt" x = hTypeVal (hType x) (hVal x)
    where hType val = (last . init $ val) : [last val]
          hVal = init . init
          hTypeVal size val 
            | size == "cm" = inRange 150 (read val :: Int) 193
            | size == "in" = inRange 59 (read val :: Int) 76          
            | otherwise    = False
propertyValidation "hcl" x = (length x == 7) && (head x == '#') && hexValueValid
    where hexValueValid = (and $ map (\a -> Set.member a hexValues) (tail x))
propertyValidation "ecl" x = Set.member x eyeColors
propertyValidation "pid" x = length x == 9
propertyValidation "cid" _ = True

mapify :: [[String]] -> [Map.Map String String]
mapify = map (foldr (\item mp -> Map.insert (key item) (value item) mp) Map.empty) 
    where key = take 3
          value = drop 4

qualifies :: Map String String -> Bool
qualifies m = necessaryValues `Set.isSubsetOf` (Set.fromList $ Map.keys m)

validates :: Map String String -> Bool
validates m = (Map.foldrWithKey (\k a b -> (propertyValidation k a) && b) True m)

input :: String -> [[String]]
input = map words . splitOn "\n\n"

part1 :: (Map String String -> Bool)
part1 = qualifies

part2 :: (Map String String -> Bool)
part2 x = (qualifies x) && (validates x)

day4 :: (Map String String -> Bool) -> [[String]] -> Int
day4 check = sum . map (\p -> if p then 1 else 0) . map check . mapify

main = do
    contents <- readFile "inputs/day4.txt"
    let inp = input contents
    print $ day4 part1 inp
    print $ day4 part2 inp
    

