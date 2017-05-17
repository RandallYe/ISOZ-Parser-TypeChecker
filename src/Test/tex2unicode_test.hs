module Main where

import System.IO (hPutStrLn, stderr, stdout)
import System.Environment (getArgs, getEnv)
import System.Console.GetOpt 
import Language.ISOZ.Tex2Unicode (tex2Unicode, printResult, outputUnicode2File, tokensToUnicodeStringSpan)
import System.Exit (ExitCode(..), exitWith)
import Data.Maybe (fromMaybe)

data Flag = Verbose | Version | Help
        | UniOutput String 
      deriving (Eq, Show)

options :: [OptDescr Flag]
options =
    [ Option ['v']     ["verbose"] (NoArg Verbose)       "chatty output on stderr"
    , Option ['V']     ["version"] (NoArg Version)       "show version number"
    , Option ['h','?'] ["help"] (NoArg Help)             "show help"
    , Option ['o']     ["unicode"] (OptArg (UniOutput. fromMaybe "stdout") "FILE")             "Output unicode code to FILE"
    ]

notUniOutput :: Flag -> Bool
notUniOutput (UniOutput _)  = True 
notUniOutput _              = False 
-------------------------------------------------------------------
-- main 
-------------------------------------------------------------------
main = do
    args <- getArgs
    case parse args of 
        ([], [file], [])        -> do ret <- (tex2Unicode file); printResult ret 
        (opts, files, [])
            | Help `elem` opts  -> help
            | (length files) > 1 -> die ["Expect one argument only!\n"]
            | [UniOutput file] <- filter (notUniOutput) opts -> do 
                                    ret <- (tex2Unicode (head files))
                                    print ("---- Unicode output => [" ++ file ++ "] ----")
                                    case ret of
                                        Left err -> do printResult ret 
                                        Right toks -> do 
                                            outputUnicode2File toks file 
                                            print ("Location map:" ++ (show (tokensToUnicodeStringSpan toks 1 1)))
        (_,_,errs)              -> die errs
  where parse argv = getOpt Permute options argv
        header = "Usage: tex2unicode [-h] [-v] [-V] [file]"
        info = usageInfo header options
        dump = hPutStrLn stderr
        die errs = dump (concat errs ++ info) >> exitWith (ExitFailure 1) 
        help = dump info >> exitWith ExitSuccess 
