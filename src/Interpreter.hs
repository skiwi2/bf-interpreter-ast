module Interpreter
    ( interpret
    ) where

import Tape

data BFInstruction = Right | Left | Increment | Decrement | Output | Input | Loop [BFInstruction] | Stop deriving (Eq, Show)
type BFProgram = [BFInstruction]

newtype BFMemoryCell = BFMemoryCell Int deriving (Eq, Show)
type BFMemory = Tape BFMemoryCell

interpret :: String -> IO (BFProgram, BFMemory)
interpret program = return ([Stop], Tape [] (BFMemoryCell 0) [])


