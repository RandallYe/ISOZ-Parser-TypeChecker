-- -*- haskell -*-
{
{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE GADTs #-}

{- |
Module      : SecSolver
Description : 
Copyright   : (C) Kangfeng Ye, 2017
License     : BSD3
Maintainer  : ye.randall@gmail.com 
Stability   : experimental

SecSolver is a module having a lexer to parse and solve one input Z specification's all parent sections, including all standard toolkits. The order of paths to be checked for standard toolkits are

- 1. current path (the same folder with the input latex file)
- 2. ~\/.ZParser\/toolkits or env{HOME}\/.ZParser\/toolkits
- 3. env{ZPARSER_DIR}\/toolkits

-}
module Language.ISOZ.SecSolver (secSolve) where

import Language.ISOZ.Common.Error
import Control.Monad
import qualified Data.Bits
import Data.Word (Word8)
import Data.List (sort, drop, isPrefixOf)
import System.Directory (doesFileExist, getCurrentDirectory)
-- import Data.List.Utils (replace)
import System.IO
import System.Environment (getArgs, getEnv)
import System.FilePath (pathSeparator, takeFileName, takeDirectory, takeBaseName )
import Control.Exception (try, SomeException)
}

%wrapper "monadUserState"

$SPACE   = [\ \t]    -- space ' ' and \t
$COMMENT = \x0025  -- %
-- schema name in schema environment or generic schema environment
-- @SCHNAME = ([^\}]*)
@Id = ([A-Za-z]([A-Za-z0-9] | [\\][_])*)
-- begin without an alphabet
@NotId1 = (([^$white\,\\\~] # [A-Za-z])[^$white\,\\\~]*) 
-- begin with an alphabet but not valid after
@NotId2 = ([A-Za-z]([^$white\,\\\~] # [A-Za-z0-9\\_])+) 
@NotId3 = ([A-Za-z]([^$white\,\\\~])*([^\\][_])([^$white\,\\\~])*) 
@NotId4 = ([A-Za-z]([^$white\,\\\~])*([\\][^_\ ])([^$white\,\\\~])*) 
@NotId = (@NotId1 | @NotId2 | @NotId3 | @NotId4)
@EmptyParId = ($white* [\,])
@EmptySecId = ($white* [\\] "parents")

-- space around sections and parents 
@SPACES = ([\\][\ ] | [\~])

-- environment
@BENVZSEC = ([\\] "begin" [\{] $SPACE* "zsection" $SPACE* [\}])
@EENVZSEC = ([\\] "end" [\{] $SPACE* "zsection" $SPACE* [\}])
@NOTENVNAME = ([^z] | 
                [z] ([^s] | 
                     [s] ([^e] |  -- zsection
                          [e] ([^c] | 
                               [c] ([^t] | 
                                    [t] ([^i] | 
                                         [i] ([^o] | 
                                              [o] ([^n] | 
                                                   [n] [^$SPACE\}] ))))))))

-- not a defined environment
@BENVNOTDEFD = ([\\] "begin" [\{] $SPACE* (@NOTENVNAME) .* $SPACE* [\}])
@EENVNOTDEFD = ([\\] "end" [\{] $SPACE* (@NOTENVNAME) .* $SPACE* [\}])

-- not an environment
@NOTENV = (([^\\] | 
             [\\] ([^be] | 
                   [b] ([^e] | 
                        [e] ([^g] |
                             [g] ([^i] | 
                                  [i] ([^n] |
                                       [n] [^\{]))))  | 
                   [e] ([^n] | 
                        [n] ([^d] | 
                             [d] ([^\{]))
                       ) 
                  )
            )
        )
-- 

@SECTION = ([\\] "SECTION")
@PARENTS = ([\\] "parents")
@ParentsName = (@Id )
-----------------------------
TEX2UNI :-
<0> {
    -- skip [\ \t\n\f\v\r] 
--    $white+ ;
    [\n\f\v\r]                  ;
    ^ $white* $                 ;
    ^ $COMMENT $                ; 
    ^ $COMMENT [^$COMMENT].* $  ; 
}

<0> {
    ^ $SPACE* @BENVZSEC                                                 { begin zsection }
    ^ $SPACE* @BENVNOTDEFD .* $                                         ; 
    ^ $SPACE* @EENVNOTDEFD .* $                                         ; 
    ^ $SPACE* @NOTENV .* $                                              ; 
}

<zsection, section, pre_parents, parents, commas> {
    [\n\f\v\r]                                                          ;
    ^ $white* $                                                         ;
    ^ $COMMENT $                                                        ; 
    ^ $COMMENT [^$COMMENT].* $                                          ; 
    ^ $SPACE* @EENVZSEC                                                 { begin 0 }
}

<zsection> {
    @SPACES                                                             ;
    $SPACE* @SECTION ($SPACE | @SPACES)+                                { begin section } 
}

<section> {
    $SPACE*                                                             ;
    @SPACES                                                             ;
    @Id                                                                 { mkLSec `andBegin` pre_parents}
    @NotId                                                              { errMsg "valid section name" }
    @EmptySecId                                                         { errMsg "non-empty section name" }
}

<pre_parents> {
    $SPACE*                                                             ;
    @SPACES                                                             ;
    @PARENTS                                                            { begin parents }
}

<parents> {
    $SPACE*                                                             ;
    @SPACES                                                             ;
    @Id                                                                 { mkLPar `andBegin` commas }
    @NotId                                                              { errMsg "valid parent name" }
    @EmptyParId                                                         { errMsg "non-empty parent name" }
}

<commas> {
    $SPACE*                                                             ;
    ","                                                                 { begin parents }
}

{
data Tok 
    -- | T_SEC for section name token, such as T_SEC "relation_toolkit"
    = T_SEC !String
    -- | T_PARENT for parent section name token, such as T_PARENT "set_toolkit"
    | T_PARENT !String
 deriving (Eq,Show,Ord)

data Token 
    -- | Normal tokens with position 
    = PT  AlexPosn (Tok) 
    -- | Err with position 
    | Err AlexPosn
    -- | End of File 
    | EOF
  deriving (Eq,Show)

-------------------------------------------------------------------
-- Action 
-------------------------------------------------------------------

-- | make a T_SEC token 
mkLSec :: AlexInput -> Int -> Alex Token
mkLSec (p,_,_,str) len = return (PT p (T_SEC (substUnderLine (take len str))))

-- | make a T_PARENT token 
mkLPar :: AlexInput -> Int -> Alex Token
mkLPar (p,_,_,str) len = return (PT p (T_PARENT (substUnderLine (take len str))))

-- | substitute "\\_" with "_"
substUnderLine :: String -> String
substUnderLine s = rep "\\_" "_" s 

-- | replace a by b for s 
rep :: String -- ^ the original string to be replaced
    -> String -- ^ the string which the original string will be replaced to 
    -> String -- ^ the input string
    -> String -- ^ returned string with a in s being replaced by b 
rep a b s@(x:xs) = if isPrefixOf a s
                     -- then, write 'b' and replace jumping 'a' substring
                     then b++rep a b (drop (length a) s)
                     -- then, write 'x' char and try to replace tail string
                     else x:rep a b xs
rep _ _ [] = []

-------------------------------------------------------------------
-- Alex wrapper code.
-------------------------------------------------------------------
alexEOF :: Alex Token
alexEOF = return EOF

data AlexUserState = AlexUserState {
    -- | current file in processing
    curFile :: String
 }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState [] 

-- | get current file in processing 
getCurFile :: Alex String
getCurFile = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, curFile ust)

-- | set current file in processing 
setCurFile :: (String) -> Alex ()
setCurFile ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){curFile=ss}}, ())

-- | show position information (line:col)
showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':': show col

lexerError :: String -> Alex a
lexerError msg =
    do (p, c, _, inp) <- alexGetInput
       file <- getCurFile
       let inp1 = filter (/= '\r') (takeWhile (/='\n') inp)
       let inp2 = if (length inp1 > 30)
                     then trim (take 30 inp1)
                     else trim inp1
       let disp = if (null inp)
                     then " at end of file"
                     else if (null inp2)
                             then " before end of line"
                             else " on char " ++ show c ++ " before : '" ++ inp2 ++ "'"
       let disp3 = if (null msg)
                      then "Lexer error"
                      else trim msg
       alexError (disp3 ++ " at [" ++ file ++ ":" ++ showPosn p ++ "]" ++ disp)
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

errMsg :: String -> AlexInput -> Int -> Alex a
errMsg s (_,_,_,input) len = lexerError (err2Msg (ErrSecSolve (ESecLexer ("Expect: [" ++ s ++ "]. But it is [" ++ (take len input) ++ "]"))))

-- | scan an input latex file and return a sequence of tokens or error with error message
scanner :: String -> String -> Either String ([Token])
scanner file str = 
  let loop = do
        tok <- alexMonadScan
        if tok == EOF
          then return []
          else do toks <- loop; return (tok:toks)
  in runAlex str (setCurFile file >> loop)

-- | get the standard toolkits path
getStandardToolkitPathFromEnv :: String -- ^ a standard toolkit file name
    -> String -- ^ an environment variable name in which the toolkit file will be searched
    -> String -- ^ a sub-path in the path decided by the environment variable 
    -> IO (Either String String) -- ^ return solved path to the toolkit
getStandardToolkitPathFromEnv file env subpath = do
      r <- try (getEnv env) :: IO (Either SomeException String)
      case r of
        Left e        -> return (Left (err2Msg (ErrSecSolve (ESecNoEnv (env))))) 
        Right evar    -> do
          exists <- doesFileExist (evar ++ [pathSeparator] ++ subpath ++ [pathSeparator] ++ file)
          if not exists 
          then return (Left (err2Msg (ErrSecSolve (ESecNoFile (evar ++ [pathSeparator] ++ subpath ++ [pathSeparator] ++ file)))))
          else return (Right (evar ++ [pathSeparator] ++ subpath ++ [pathSeparator] ++ file))

-- get the standard toolkits path
-- return (Left err) or (Right absolute file path)
-- | get a file's absolute path in the input path or standard search paths (if it is a standard toolkit)
getFilePath :: String -> String -> IO (Either String String)
getFilePath path file = do
--    curpath <- getCurrentDirectory
    exists <- doesFileExist (path ++ [pathSeparator] ++ file)
    if not exists 
    then if file `elem` standardToolkits
         then do 
            r <- getStandardToolkitPathFromEnv file "HOME" (".ZParser" ++ [pathSeparator] ++ "toolkits")
            case r of
                Left err    -> do 
                    r <- getStandardToolkitPathFromEnv file "ZPARSER_DIR" "toolkits"
                    case r of
                        Left err1   -> return (Left (err2Msg (ErrSecSolve (ESecNoFile ((path ++ [pathSeparator] ++ file) ++ "\n" ++ err ++ "\n" ++ err1)))))
                        Right rpath -> return (Right rpath)
                Right rpath -> return (Right rpath)
         else return (Left (err2Msg (ErrSecSolve (ESecNoFile ((path ++ [pathSeparator] ++ file))))))
    else return (Right (path ++ [pathSeparator] ++ file))
  
-- | a list of name for standard toolkits
standardToolkits :: [String]
standardToolkits = [
    "prelude.tex",
    "set_toolkit.tex",
    "relation_toolkit.tex",
    "function_toolkit.tex",
    "number_toolkit.tex",
    "sequence_toolkit.tex",
    "standard_toolkit.tex",
    "fuzz_toolkit.tex",
    "circus_prelude.tex",
    "circus_toolkit.tex"
    ]

-- | load a Latex file and only return a section name and a list of parents
loadLaTexFile :: String -> String -> IO (Either String [Token])
loadLaTexFile path file = do 
  r <- getFilePath path file
  case r of
    Left err    -> return (Left err)
    Right rpath -> do 
        h <- openFile rpath ReadMode
        content <- hGetContents h
        let result = scanner rpath content 
        case result of
            Left err1 ->  return (Left err1)
            Right toks -> return (Right (appendPath (takeDirectory rpath) toks)) 

-- | append path in the prefix of all section names
appendPath :: String -> [Token] -> [Token]
appendPath path []                          = []
appendPath path s@((PT p (T_SEC sec)):xs)   = (PT p (T_SEC (path ++ [pathSeparator] ++ sec ++ ".tex"))):(appendPath path xs)
appendPath path s@(x:xs)                    = x:(appendPath path xs)

-- | recursively read and scan a file with a list of visited sections to determine a list of paths to its all parent sections. 
readAndScanner1 :: String               -- ^ only path, such as "/home/xxx/.zparser/toolkits/"
        -> String                       -- ^ only filename, such as "relation_toolkit.tex"
        -> [(String, String)]           -- ^ a list of solved parents (a pair from its section name to file name), such as [("set_toolkit", "set_toolkit.tex"), ("relation_toolkit", "relation_toolkit.tex")]
        -> IO (Either String [(String, String)])  -- ^ either (Left Err) or (Right [a list of solved parents])
readAndScanner1 path file parents = 
    if isVisitedFile file parents
    then return (Right parents)
    else do 
        ret <- loadLaTexFile path file
        case ret of
            Left err -> return (Left err) 
            Right toks -> case toks of  
                []                  -> return (Right (parents)) 
                -- where s is a full path to "section name.tex", such as "/path/to/sec.tex", but it is not the final latex file which the section is in
                -- (sec, "/path/to/secfile.tex")
                [PT _ (T_SEC s)]    -> return (Right (parents ++ [(takeBaseName s, ((takeDirectory s) ++ [pathSeparator] ++ file))])) 
                (PT _ (T_SEC s)):xs -> do 
                            r <- loop xs (parents ++ [(takeBaseName s, ((takeDirectory s) ++ [pathSeparator] ++ file))])
                            case r of 
                                 Left err -> return (Left err)
                                 Right ps -> return (Right (ps ++ [(takeBaseName s, ((takeDirectory s) ++ [pathSeparator] ++ file))]))
  -- iterate: load of each parent
  where loop [] parents1                         = return (Right parents1)
        loop s@((PT _ (T_PARENT p)):xs) parents1 = if isVisitedSec p parents1
                                                     then loop xs parents1
                                                     else do ret <- readAndScanner1 path (p ++ ".tex") parents1
                                                             case ret of 
                                                                Left err -> return ret
                                                                Right parents2 -> loop xs parents2
        loop s@((PT _ (T_SEC p)):xs) parents1 = loop xs (parents1 ++ [(takeBaseName p, ((takeDirectory p) ++ [pathSeparator] ++ file))])
        --loop s@((PT _ (T_SEC p)):xs) parents1 = error ("\n[Limitations] multiple sections in a file are not supported, \n" ++
        --    "\tplease separate them into individual files and \n" ++
        --    "\tmake sure the file name matches section name!!!")

-- | check if a file has been visited before or not 
isVisitedFile :: String -> [(String, String)] -> Bool
isVisitedFile file []       = False
isVisitedFile file ((s,f):xs)   = if (takeFileName f) == file
                            then True 
                            else isVisitedFile file xs

-- | check if a section has been visited before or not 
isVisitedSec :: String -> [(String, String)] -> Bool
isVisitedSec sec []       = False
isVisitedSec sec ((s,f):xs)   = if sec == s
                            then True 
                            else isVisitedSec sec xs

pairToList :: [(String, String)] -> [String]
pairToList [] = []
pairToList ((_,s):xs) = if s `elem` p
                        then p  
                        else s:p
    where p = (pairToList xs)

{- | get a list of an input latex file's all parent sections with absolute paths. 
For example, (secSolve "set_toolkit.tex") => ["\/path\/to\/prelude.tex","\/path\/to\/set_toolkit.tex"] 
-}
secSolve :: String -- ^ path to input latex file
    -> IO (Either String [String])  -- ^ a list of pathes to its parent sections 
secSolve file = do
    -- curpath <- getCurrentDirectory
    if (takeFileName file) == "prelude.tex" 
    then do
        r <- readAndScanner1 (takeDirectory file) (takeFileName file) []
        case r of
            Left err -> return (Left err)
            Right lp -> return (Right (pairToList lp))
    else do
        prel <- readAndScanner1 (takeDirectory file) "prelude.tex" []
        case prel of
            Left err    -> return (Left err)
            Right [(s,f)]   -> do 
                    r <- readAndScanner1 (takeDirectory file) (takeFileName file) [(s,f)]
                    case r of
                        Left err            -> return (Left err)
                        -- the specification doesn't have a section environtment
                        Right [(_,onlyone)]     -> return (Right [f, file]) 
                        Right morethanone   -> return (Right (pairToList morethanone))
}
