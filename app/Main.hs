module Main where

import qualified Data.Map as Map
import System.Environment

import Lib

data SimpleOption = ShowProgram | ShowMemory deriving (Enum, Eq, Ord, Show)
data AdvancedOption = Program | File deriving (Enum, Eq, Ord, Show)
data Options = Options [SimpleOption] (Map.Map AdvancedOption String) deriving (Show)

addSimpleOption :: SimpleOption -> Options -> Options
addSimpleOption opt (Options sOpts aOpts) = Options (opt:sOpts) aOpts

addAdvancedOption :: AdvancedOption -> String -> Options -> Options
addAdvancedOption opt val (Options sOpts aOpts) = Options sOpts (Map.insert opt val aOpts)

isOptionsEmpty :: Options -> Bool
isOptionsEmpty (Options sOpts aOpts) = null sOpts && Map.null aOpts

main :: IO ()
main = do
    args <- getArgs
    let opts = parseArgs args (Options [] Map.empty)
    print opts

parseArgs :: [String] -> Options -> Options
parseArgs [] opts =                     if isOptionsEmpty opts
    then error "Usage: bf-interpreter-ast-exe [-sp] [-sm] [-f file] [-p program | program]"
    else opts
parseArgs [program] opts =              addAdvancedOption Program program opts
parseArgs ("-sp":args) opts =           parseArgs args (addSimpleOption ShowProgram opts)
parseArgs ("-sm":args) opts =           parseArgs args (addSimpleOption ShowMemory opts)
parseArgs ("-p":program:args) opts =    parseArgs args (addAdvancedOption Program program opts)
parseArgs ("-f":file:args) opts =       parseArgs args (addAdvancedOption File file opts)
parseArgs _ _ =                         error "Usage: bf-interpreter-ast-exe [-sp] [-sm] [-f file] [-p program | program]"