module Interpreter
    ( interpret
    ) where

import Data.Word
import System.IO

import Tape

data BFInstruction = MemoryRight | MemoryLeft | Increment | Decrement | Output | Input | Loop [BFInstruction] deriving (Eq, Show)
type BFProgram = [BFInstruction]

newtype BFMemoryCell = BFMemoryCell Word8 deriving (Eq, Show)
type BFMemory = Tape BFMemoryCell

cellValue :: BFMemoryCell -> Word8
cellValue (BFMemoryCell val) = val

onCellValue :: (Word8 -> Word8) -> BFMemoryCell -> BFMemoryCell
onCellValue func (BFMemoryCell val) = BFMemoryCell $ func val

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
interpret input = do
    let program = makeProgram input
    let memory = makeTape (BFMemoryCell 0)
    memory' <- execute program memory
    return (program, memory')

execute :: BFProgram -> BFMemory -> IO BFMemory
execute [] memory = return memory
execute xs@(x:xs') memory = case x of
    MemoryRight     -> continue $ forwardTape (BFMemoryCell 0) memory
    MemoryLeft      -> continue $ reverseTape (BFMemoryCell 0) memory
    Increment       -> continue $ onTapeValue (onCellValue (+1)) memory
    Decrement       -> continue $ onTapeValue (onCellValue (subtract 1)) memory
    Output          -> do
        putChar $ toEnum . fromEnum . cellValue . tapeValue $ memory
        hFlush stdout
        continue memory
    Input           -> do
        ch <- getChar
        continue $ onTapeValue (\_ -> BFMemoryCell . toEnum . fromEnum $ ch) memory
    Loop program'   -> if cellValue (tapeValue memory) == 0
        then continue memory
        else do
            memory' <- execute program' memory
            execute xs memory'
    where
        continue = execute xs'