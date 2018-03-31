module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
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
    case validateOptions <$> parseArgs args (Options [] Map.empty) of
        Left err            -> putStrLn err
        Right (Left err)    -> putStrLn err
        Right (Right opts)  -> print opts

parseArgs :: [String] -> Options -> Either String Options
parseArgs [] opts =                     if isOptionsEmpty opts then Left usage else Right opts
parseArgs [program] opts =              Right $ addAdvancedOption Program program opts
parseArgs ("-sp":args) opts =           parseArgs args (addSimpleOption ShowProgram opts)
parseArgs ("-sm":args) opts =           parseArgs args (addSimpleOption ShowMemory opts)
parseArgs ("-p":program:args) opts =    parseArgs args (addAdvancedOption Program program opts)
parseArgs ("-f":file:args) opts =       parseArgs args (addAdvancedOption File file opts)
parseArgs _ _ =                         Left usage

usage :: String
usage = "Usage: bf-interpreter-ast-exe [-sp] [-sm] [-f file] [-p program | program]"

validateOptions :: Options -> Either String Options
validateOptions opts@(Options _ aOpts) = if Set.isSubsetOf (Set.fromList [Program, File]) (Map.keysSet aOpts)
    then Left "Error: Only one of the options File and Program can be present"
    else Right opts