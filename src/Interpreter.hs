module Interpreter
    ( interpret
    ) where

import Tape

data BFInstruction = MemoryRight | MemoryLeft | Increment | Decrement | Output | Input | Loop [BFInstruction] deriving (Eq, Show)
type BFProgram = [BFInstruction]

newtype BFMemoryCell = BFMemoryCell Int deriving (Eq, Show)
type BFMemory = Tape BFMemoryCell

makeProgram :: String -> BFProgram
makeProgram = makeProgram'

makeProgram' :: String -> BFProgram
makeProgram' "" = []
makeProgram' (x:xs) = case x of
    '>' -> continue MemoryRight
    '<' -> continue MemoryLeft
    '+' -> continue Increment
    '-' -> continue Decrement
    '.' -> continue Output
    ',' -> continue Input
    '[' -> do
        let (loop, rest) = splitOnLoopEnd xs
        Loop (makeProgram loop):makeProgram' rest
    ']' -> []
    _   -> makeProgram' xs
    where
        continue instr = instr:makeProgram' xs

splitOnLoopEnd :: String -> (String, String)
splitOnLoopEnd = splitOnLoopEnd' 0

splitOnLoopEnd' :: Int -> String -> (String, String)
splitOnLoopEnd' _ "" = error "No matching ] found"
splitOnLoopEnd' 0 (']':xs') = ([], xs')
splitOnLoopEnd' nesting (x:xs') = case x of
    ']' -> (x:ys, zs) where (ys, zs) = next (subtract 1)
    '[' -> (x:ys, zs) where (ys, zs) = next (+1)
    _   -> (x:ys, zs) where (ys, zs) = next id
    where
        next func = splitOnLoopEnd' (func nesting) xs'


interpret :: String -> IO (BFProgram, BFMemory)
interpret program = return (makeProgram program, Tape [] (BFMemoryCell 0) [])

