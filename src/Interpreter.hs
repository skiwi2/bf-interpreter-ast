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
        let (loop, rest) = breakLoopEnd xs
        Loop (makeProgram loop):makeProgram' (drop 1 rest)
    ']' -> []
    _   -> makeProgram' xs
    where
        continue instr = instr:makeProgram' xs

breakLoopEnd :: String -> (String, String)
breakLoopEnd = breakLoopEnd' 0

breakLoopEnd' :: Int -> String -> (String, String)
breakLoopEnd' _ "" = error "No matching ] found"
breakLoopEnd' 0 xs@(']':_) = ([], xs)
breakLoopEnd' nesting (x:xs') = case x of
    ']' -> (x:ys, zs) where (ys, zs) = next (subtract 1)
    '[' -> (x:ys, zs) where (ys, zs) = next (+1)
    _   -> (x:ys, zs) where (ys, zs) = next id
    where
        next func = breakLoopEnd' (func nesting) xs'


interpret :: String -> IO (BFProgram, BFMemory)
interpret program = return (makeProgram program, Tape [] (BFMemoryCell 0) [])

