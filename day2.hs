import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

inputParser :: GenParser Char st (Int, Int, Char, String)
inputParser = do
    min <- many digit
    _ <- char '-'
    max <- many digit
    _ <- char ' '
    a <- anyChar
    _ <- string ": "
    last <- many anyChar
    return (read min, read max, a, last)

countOccurences str c = length $ filter (==c) str

unwrap :: Either ParseError (Int, Int, Char, String) -> (Int, Int, Char, String)
unwrap (Left _) = (0,0,' ',"")
unwrap (Right x) = x

validPassword :: (Int, Int, Char, String) -> Bool
validPassword (x, y, c, s) = let a = countOccurences s c in x <= a && a <= y

validPassword2 :: (Int, Int, Char, String) -> Bool
validPassword2 (x, y, c, s) = (s !! (x - 1) == c) /= (s !! (y - 1) == c)

inputs :: String -> [(Int, Int, Char, String)]
inputs = map (unwrap . (parse inputParser "(unknown)")) . lines

day2 verifier = length . filter (==True) . map verifier . inputs

main = do
    contents <- readFile "inputs/day2.txt"
    putStrLn $ show . day2 validPassword $ contents
    putStrLn $ show . day2 validPassword2 $ contents

