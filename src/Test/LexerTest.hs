module Main (main) where

import System.IO
import System.Exit ( exitFailure, exitSuccess )
import System.Environment (getArgs)
import Language.ISOZ.Lexer.ISOZLexer  (scanner)

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    ]
  exitFailure

-------------------------------------------------------------------
-- main 
-------------------------------------------------------------------
main = do
--    args <- getArgs
--    case args of 
--        [input] -> readAndScanner input
--        _       -> putStrLn "Error: expect one argument only"
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> readAndScannerStdin
    fs -> mapM_ (readAndScanner) fs
  where readAndScanner filename = do 
          h <- openFile filename ReadMode
          content <- hGetContents h
          let result = scanner content
          print result
        readAndScannerStdin = do
          content <- hGetContents stdin
          let result = scanner content
          print result
