module Main where

import Control.Monad
import qualified Data.Map as Map
import System.Environment

import Interpreter

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
    case parseArgs args (Options [] Map.empty) >>= validateOptions of
        Left err    -> putStrLn err
        Right opts  -> execute opts

execute :: Options -> IO ()
execute opts@(Options sOpts _) = do
    program <- getProgram opts
    (program', memory) <- interpret program
    when (ShowProgram `elem` sOpts) $ putStrLn ("\n" ++ show program')
    when (ShowMemory `elem` sOpts) $ putStrLn ("\n" ++ show memory)

getProgram :: Options -> IO String
getProgram (Options _ aOpts) = case Map.lookup Program aOpts of
    Nothing         -> case Map.lookup File aOpts of
        Just file       -> readFile file
    Just program    -> return program

parseArgs :: [String] -> Options -> Either String Options
parseArgs [] opts                   = if isOptionsEmpty opts then Left usage else Right opts
parseArgs [program] opts            = Right $ addAdvancedOption Program program opts
parseArgs ("-sp":args) opts         = parseArgs args (addSimpleOption ShowProgram opts)
parseArgs ("-sm":args) opts         = parseArgs args (addSimpleOption ShowMemory opts)
parseArgs ("-p":program:args) opts  = parseArgs args (addAdvancedOption Program program opts)
parseArgs ("-f":file:args) opts     = parseArgs args (addAdvancedOption File file opts)
parseArgs _ _                       = Left usage

usage :: String
usage = "Usage: bf-interpreter-ast-exe [-sp] [-sm] [-f file] [-p program | program]"

validateOptions :: Options -> Either String Options
validateOptions opts@(Options _ aOpts)
  | Program `Map.member` aOpts && File `Map.member` aOpts   = Left "Error: Only one of the options File and Program can be present"
  | Program `Map.member` aOpts || File `Map.member` aOpts   = Right opts
  | otherwise                                               = Left "Error: One of the options File and Program must be present"