import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as S

data InstructionType = NOP | ACC | JMP deriving (Eq)
data Instruction = Instruction { word :: InstructionType, amount :: Int }
data Computer = Computer { instruction :: [Instruction], pc :: Int, acc :: Int }

readI s = Instruction (read' . head . words $ s) ((read . tail . last . words $ s) * (pm $ head . last . words $ s))
    where read' "acc" = ACC
          read' "jmp" = JMP
          read' _ = NOP
          pm a = if a == '+' then 1 else -1

replaceInstructionType (Instruction w a) i = (Instruction i a)

replaceInstructionAt :: Int -> [Instruction] -> InstructionType -> [Instruction]
replaceInstructionAt 0 (x:xs) p = ((Instruction p (amount x)):xs)
replaceInstructionAt i (x:xs) p = (x:(replaceInstructionAt (i - 1) xs p))

replaceInstInComp :: Int -> Computer -> InstructionType -> Computer
replaceInstInComp i (Computer is p a) it = Computer (replaceInstructionAt i is it) p a
    
runInstruction :: Computer -> Computer
runInstruction (Computer is p a)
  | ct == NOP = (Computer is (p + 1) a)
  | ct == ACC = (Computer is (p + 1) (a + (amount ci)))
  | ct == JMP = (Computer is (p + (amount ci)) a)
    where ci = is !! p
          ct = word ci

part1 :: Computer -> Set Int -> Int
part1 (Computer is p a) s = if S.member p s then a else part1 (runInstruction (Computer is p a)) (S.insert p s)

tryPath :: Computer -> Set Int -> Maybe Int
tryPath (Computer is p a) s = if p == ((length is) - 1) then (Just a)
                              else (if S.member p s 
                                    then Nothing 
                                    else tryPath (runInstruction (Computer is p a)) (S.insert p s))

part2 :: Computer -> Set Int -> Int
part2 c s = if ct /= ACC then decisionPath ((nextInstWith NOP) <|> (nextInstWith JMP)) else runNext
     where p = pc c 
           ci = (instruction c) !! p
           ct = word ci
           decisionPath Nothing  = runNext
           decisionPath (Just a) = a
           runNext = part2 (runInstruction c) (S.insert p s)
           nextInstWith inst = tryPath (replaceInstInComp p c inst) s

main = do
    contents <- readFile "inputs/day8.txt"
    let instruction = map readI . lines $ contents
    print $ part1 (Computer instruction 0 0) S.empty
    print $ part2 (Computer instruction 0 0) S.empty
