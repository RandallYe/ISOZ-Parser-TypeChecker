module Main where

import System.IO (hPutStrLn, stderr, stdout, stdin, hGetContents, hPutStr, IOMode(..), openFile, hClose)
import System.Environment ( getArgs, getProgName, getEnv )
import System.Console.GetOpt 
import System.Exit ( exitFailure, exitSuccess, exitWith, ExitCode(..) )
import Data.Bits ((.&.)) 
import Data.Char (isDigit, isSpace)

import Language.ISOZ.Lexer.ISOZLexer 
import Language.ISOZ.Parser.ISOZParser 
import Language.ISOZ.Utils.PrintIsoz (Print, printTree)
import Language.ISOZ.Common.AbsIsoz
import Language.ISOZ.Parser.SynTransformerZero (synTraverseZeroTree, SynTraverse0)
import Language.ISOZ.Parser.SynTransformerOne (synTraverse1Tree, SynTraverse1)
import Language.ISOZ.Parser.SynTransformerTwo (synTraverse2Tree, SynTraverse2)
import Language.ISOZ.Parser.SynTransformExpr (synTraverseExprTree, SynTraverseExpr)
import Language.ISOZ.TypeChecker.TypeInfer (typeInferTraverseTree, TypeInferTraverse) 
import Language.ISOZ.Tex2Unicode (tex2Unicode, printResult, outputUnicode2File, tokensToUnicodeString, tokensToUnicodeStringSpan, locToSourceSpan)

-- a pretty show 
import Text.Show.Pretty
import Language.Haskell.Exts.SrcLoc

--type ParseFun a = [Token] -> Err a
type ParseFun a = [Token] -> Alex a

myLLexer = myLexer

type Verbosity = Int

printToks :: Either String [Token] -> String 
printToks (Left str) = ("Error: " ++ str)
printToks (Right toks) = "-------------------------------- Start ----------------------------------\n" ++ pprt toks 
    where pprt [] = "-------------------------------- End ----------------------------------" 
          pprt (x:xs) = (show x) ++ "\n" ++ (pprt xs)

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v >= 1 then putStrLn s else return ()

runFile :: (Print a, Show a, SynTraverse0 a, SynTraverse1 a, SynTraverseExpr a, SynTraverse2 a, TypeInferTraverse a) 
    => Verbosity     -- ^ verbosity
    -> ParseFun a    -- ^ the parse function given in ISOZParser.y as "%name pSpecification Specification"
    -> FilePath      -- ^ filename of input Unicode specification 
    -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p []

runFileTex :: (Print a, Show a, SynTraverse0 a, SynTraverse1 a, SynTraverseExpr a, SynTraverse2 a, TypeInferTraverse a) 
    => Verbosity    -- ^ verbosity
    -> ParseFun a   -- ^ the parse function given in ISOZParser.y as "%name pSpecification Specification"
    -> FilePath     -- ^ filename for output of internal representation in unicode
    -> FilePath     -- ^ filename of input Latex specification 
    -> IO ()
runFileTex v p outfile infile = do 
    ret <- (tex2Unicode infile)
    case ret of
        Left err -> putStrLn ("Error [" ++ err ++ "] in conversion from LaTeX markup to Unicode.") 
        Right toks -> let (unicode_string, span) = (tokensToUnicodeStringSpan toks 1 1)
                      -- in 
                      in case outfile of 
                            ""          -> run v p span unicode_string  
                            "stdout"    -> putStrLn (unicode_string) >> run v p span unicode_string
                            _           -> do  
                                        outh <- openFile outfile WriteMode
                                        hPutStr outh unicode_string
                                        hClose outh
                                        run v p span unicode_string 

run :: (Print a, Show a, SynTraverse0 a, SynTraverse1 a, SynTraverseExpr a, SynTraverse2 a, TypeInferTraverse a) 
    => Verbosity     -- ^ verbosity
    -> ParseFun a    -- ^ the parse function given in ISOZParser.y as "%name pSpecification Specification"
    -> [(SrcSpan, SrcSpanInfo)] -- ^ a list of location map from Unicode to source Latex specification. Empty for Unicode specification input
    -> String        -- ^ unicode representation of specification
    -> IO ()
run v p span s = let ts = myLLexer s in case ts of
           Left er   -> do putStrLn "\nParse              Failed...\n"
                           putStrV (v .&. 1) "Tokens:"
                           putStrLn er
                           putStrLn (errmsgToErrWithLocation er span)
                           exitFailure
           Right tos -> let pret = p tos 
                        in case unAlex pret (AlexState {alex_pos = alexStartPos,
                                                        alex_inp = s,
                                                        alex_chr = '\n',
                                                        alex_bytes = [],
                                                        alex_ust = alexInitUserState,
                                                        alex_scd = 0}) of 
                                    Left r -> do putStrLn "\nParse              Failed...\n"
                                                 putStrV (v .&. 1) "Tokens:"
--                                                 putStrV v $ show ts
                                                 putStrV (v .&. 1) $ (printToks ts) 
                                                 putStrLn "\n" 
                                                 putStrLn r
                                                 putStrLn (errmsgToErrWithLocation r span)
                                                 exitFailure
                                    Right (_, tree) -> do putStrLn "\n[ParseCircus] Parse Successful!" 
                                                          putStrV (v .&. 1)"Tokens:"
                                                          putStrV (v .&. 1) $ show ts
                                                          putStrV (v .&. 1) $ (printToks ts)
                                                          showTree v tree span 
                                                          exitSuccess


showTree :: (Show a, Print a, SynTraverse0 a, SynTraverse1 a, SynTraverseExpr a, SynTraverse2 a, TypeInferTraverse a) 
    => Int 
    -> a 
    -> [(SrcSpan, SrcSpanInfo)] -- ^ a list of location map from Unicode to source Latex specification. Empty for Unicode specification input
    -> IO ()
showTree v tree span = do
    case (tree0) of
        Left err -> do putStrLn err; putStrLn (errmsgToErrWithLocation err span)
        Right tree0' -> do 
            putStrV v $ "\n\n[Concrete Syntax Tree]\n\n"
            putStrV (v .&. 0x02) $ ppShow tree0'
            putStrV (v .&. 0x02) $ "\n\n[Linearized concrete tree]\n\n" ++ printTree tree0'
            putStrV v $ "\n\n[Syntactically Transformed tree (Step 1: )]\n\n"
            putStrV (v .&. 0x04) $ ppShow (tree1 tree0')
            putStrV (v .&. 0x04) $ "\n\n[Linearized tree (Step 1: )]\n\n" ++ printTree (tree1 tree0')
            putStrV v $ "\n\n[Syntactically Transformed tree (Step 2: Expression Transformation)]\n\n"
            putStrV (v .&. 0x08) $ ppShow (treeE (tree1 tree0'))
            case (treeE (tree1 tree0')) of 
                Left err -> do putStrLn err; putStrLn (errmsgToErrWithLocation err span)
                Right te -> 
                    let tree2 = (synTraverse2Tree te)
                    in do 
                        putStrV (v .&. 0x08) $ "\n\n[Linearized tree (Step 2: )]\n\n" ++ printTree te
                        putStrV v $ "\n\n[Syntactically Transformed tree (Step 3: )]\n\n"
                        putStrV (v .&. 0x10) $ ppShow tree2 
                        putStrV v $ "\n\n[Syntactically Transformed tree (Step 4: Type Checked)]\n\n"
                        case (typeInferTraverseTree tree2) of 
                            Left err -> do putStrLn err; putStrLn (errmsgToErrWithLocation err span)
                            Right t ->  do
                                    putStrV (v .&. 0x20) $ (ppShow t)
                                    putStrLn "\n[ParseCircus] Parse and typecheck successfully."
  where tree1 t = (synTraverse1Tree t)
        treeE t = (synTraverseExprTree t)
        tree0 = (synTraverseZeroTree tree)

-- | get location from error message and then get source location information
errmsgToErrWithLocation :: 
       String   -- ^ error message with location information, like "at (1:3)" [format (line:column)]
    -> [(SrcSpan, SrcSpanInfo)] -- ^ a list of location map from Unicode to source Latex specification. Empty for Unicode specification input
    -> String 
errmsgToErrWithLocation err span = scanLoc err span 0 [] []

-- | string to be scanned, mode (0-initial, 1-line, 2-col), processed line string, processed col string
scanLoc :: String 
    -> [(SrcSpan, SrcSpanInfo)] -- ^ a list of location map from Unicode to source Latex specification. Empty for Unicode specification input
    -> Int
    -> String
    -> String
    -> String
scanLoc [] span m line col = ("No location in the error message.")
scanLoc ('a':'t':' ':'(':x:xs) span 0 line col = if isDigit x then scanLoc xs span 1 [x] [] else scanLoc xs span 0 line col
scanLoc (x:xs) span 0 line col = scanLoc xs span 0 line col
scanLoc (x:xs) span 1 line col 
    | isDigit x                     = scanLoc xs span 1 (line++[x]) col   -- (12.
 -- | isSpace x && (null line)      = scanLoc xs 1 (line) col        -- (12.
 -- TODO: how to cope with space before line, within line and after line
    | isSpace x                     = scanLoc xs span 1 (line) col        -- (12.
    | x == ':' && not (null line)   = scanLoc (xs) span 2 line col        -- (12,
    | otherwise                     = scanLoc (x:xs) span 0 [] []    -- (12a
scanLoc (x:xs) span 2 line col 
    | isDigit x                     = scanLoc xs span 2 (line) (col++[x]) 
    | isSpace x                     = scanLoc xs span 2 (line) col         -- (12.
    | x == ')' && not (null line)  && not (null col)  = let l = (read line)::Int
                                                            c = (read col)::Int
                                                            r = locToSourceSpan span l c  
                                                        in  case r of
                                                              Left error -> error
                                                              Right (SrcSpanInfo (SrcSpan f l1 c1 l2 c2) _) -> "at (" ++ (show l1) ++ ", " ++ (show c1) ++ ") to (" ++ (show l2) ++ ", " ++ (show c2) ++ ") in the file [" ++ (show f) ++ "]" 
    | otherwise                     = scanLoc (x:xs) span 0 [] []    -- (12a

data Options = Options {
        optHelp :: Bool
      , optVersion :: Bool
      , optVerbose :: Bool
        -- 6 bit-wise (0b111111) 
        --      1st bit: Tokens
        --      2nd bit: Concrete syntax
        --      3rd bit: Syntax Transformation (Stage 1)
        --      4th bit: Syntax Transformation (Stage 2: Expression)
        --      5th bit: Syntax Transformation (Stage 3: )
        --      6th bit: Syntax Transformation (Stage 4: Type checked)
      , optStage :: String  
      , optShow :: Bool
      , optUnicode :: Bool
      , optOutUnicode :: String  
      } deriving Show
   
defaultOptions = Options {
      optHelp = False 
    , optVersion = False 
    , optVerbose = False 
    , optStage = "all" 
    , optShow = False
    , optUnicode = False
    , optOutUnicode = ""
}

options :: [OptDescr (Options -> Options)]
options = [ Option ['h', '?'] ["help"]
      (NoArg (\opts -> opts { optHelp = True }))
      "show help"
  , Option ['l'] ["show"]
      (NoArg (\opts -> opts { optShow = True }))
      "Show options"
  , Option ['o'] ["output-unicode"]
      (ReqArg (\s opts -> opts { optOutUnicode = s }) "OutputFile")
      ("Output internal representation of specification in Unicode to the file")
  , Option ['s'] ["stage"]
      (ReqArg (\s opts -> opts { optStage = s }) "Stage")
      ("Stages to print: \n" ++ 
       "    6 bit-wise (0b111111) \n" ++
       "        1st bit: Tokens \n" ++ 
       "        2nd bit: Concrete syntax \n" ++
       "        3rd bit: Syntax Transformation (Stage 1) \n" ++ 
       "        4th bit: Syntax Transformation (Stage 2: Expression) \n" ++ 
       "        5th bit: Syntax Transformation (Stage 3: ) \n" ++ 
       "        6th bit: Syntax Transformation (Stage 4: Type checked)\n")
  , Option ['u'] ["unicode"]
      (NoArg (\opts -> opts { optUnicode = True }))
      "Input file is Unicode (instead of default LaTeX markup)"
  , Option ['v'] ["verbose"]
      (NoArg (\opts -> opts { optVerbose = True }))
      "verbose output"
  , Option ['V'] ["version"]
      (NoArg (\opts -> opts { optVersion = True }))
      "show version number"
  ]

-------------------------------------------------------------------
-- main 
-------------------------------------------------------------------
main = do
    args <- getArgs
    prog <- getProgName 
    case parse args of 
        ([], [file], [])        -> runFileTex 2 pSpecification "stdout" file
        (opts, [], [])          -> help prog 
        (opts, files, [])      
            | optHelp ops       -> help prog 
            | optVersion ops    -> putStrLn $ "Current Version of " ++ prog ++ " is \t v1.0"  
            | optShow ops       -> putStrLn $ show ops 
            | otherwise         -> if optUnicode ops
                                    then 
                                        case optStage ops of
                                            "all"       ->  mapM_ (runFile 0x3F pSpecification) files
                                            stage       ->  mapM_ (runFile ((read stage) :: Int) pSpecification) files
                                    else
                                        case optStage ops of
                                            "all"       ->  mapM_ (runFileTex 0x3F pSpecification (optOutUnicode ops)) files 
                                            stage       ->  mapM_ (runFileTex ((read stage) :: Int) pSpecification (optOutUnicode ops)) files
          where ops = (foldl (flip id) defaultOptions opts)
        (_,_,errs)              -> die errs prog
  where parse argv = getOpt Permute options argv
        header prog = "Usage: " ++ prog ++ " [OPTIONS...] [file [...]]"
        info prog = usageInfo (header prog) options
        dump = hPutStrLn stderr
        die errs prog = dump (concat errs ++ (info prog)) >> exitWith (ExitFailure 1) 
        help prog = dump (info prog) >> exitWith ExitSuccess 
