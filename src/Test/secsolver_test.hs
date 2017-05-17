module Main where

import System.IO
import System.Environment (getArgs, getEnv)
import Language.ISOZ.SecSolver (secSolve)

-------------------------------------------------------------------
-- main 
-------------------------------------------------------------------
main = do
    args <- getArgs
    case args of 
        [input] -> do ret <- secSolve input
                      case ret of
                        Left err    -> putStrLn err
                        Right toks  -> print toks
        _       -> putStrLn "Error: expect one argument only"
