import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List

parseBag :: [String] -> [(Int, String)]
parseBag (["no", "other", "bags."]) = []
parseBag (x1:x2:x3:x4:xs) = ((read x1), x2 ++ " " ++ x3):(if last x4 == ',' then (parseBag xs) else []) 

makeBags :: String -> Map String [(Int, String)]
makeBags s = foldr M.union M.empty . map (makeBag' . words) . lines $ s
    where makeBag' s = M.singleton (intercalate " " . take 2 $ s) (parseBag . drop 4 $ s)

transitiveClosure :: String -> Map String [(Int, String)] -> Set String
transitiveClosure s m = (S.fromList directBags) `S.union` (foldr closureFold S.empty directBags)
    where directBags = map snd (m M.! s)
          closureFold x acc = acc `S.union` transitiveClosure x m

howManyCanHold :: String -> Map String [(Int, String)] -> Int
howManyCanHold bag m = sum $ map (\x -> if bag `S.member` (transitiveClosure x m) then 1 else 0) (M.keys m)

howManyDoesItHold :: String -> Map String [(Int, String)] -> Int
howManyDoesItHold bag m = foldr howManyDoesItHold' 0 (m M.! bag)
    where howManyDoesItHold' (x, s) acc = acc + x + (x * (howManyDoesItHold s m))

main = do
    contents <- readFile "inputs/day7.txt"
    print $ howManyCanHold "shiny gold" (makeBags contents)
    print $ howManyDoesItHold "shiny gold" (makeBags contents)
