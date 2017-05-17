-- -*- haskell -*-
{
{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE GADTs #-}

{- |
Module      : Language.ISOZ.Tex2Unicode 
Description : 
Copyright   : (C) Kangfeng Ye, 2017
License     : BSD3
Maintainer  : ye.randall@gmail.com 
Stability   : experimental

Tex2Unicode is a module having a lexer to convert an input latex Z specification into corresponding unicode file according to Appendix A.2 of ISO Z Standard 2002. 

-}
module Language.ISOZ.Tex2Unicode (tex2Unicode, printResult, outputUnicode2File, tokensToUnicodeString, tokensToUnicodeStringSpan, locToSourceSpan) where

import Language.ISOZ.ZChar 
import Language.Circus.CircusChar 
import Language.ISOZ.Common.Error
import Control.Monad
import qualified Data.Bits
import Data.Word (Word8)
import Data.List -- (sort, drop)
import Data.Char (chr, ord, isSpace, isAlpha, isDigit, readLitChar)
import System.IO
import System.Environment (getArgs)
import Data.Map (Map)
import qualified Data.Map as Map
import Language.ISOZ.SecSolver (secSolve) 
import Language.Haskell.Exts.SrcLoc
}

%wrapper "monadUserState"

$SPACE   = [\x20 \x09]    -- space ' ' and \t
$COMMENT = \x0025  -- %
$LBRACE  = [\{]
$RBRACE  = [\}]
-----------------------------------------
-- Hard space A.2.2
-----------------------------------------
@HARDSPACES = ( 
    [\~] | 
    "\," | 
    "\:" | 
    "\;" | 
    "\ " | 
    "\t1" | 
    "\t2" | 
    "\t3" | 
    "\t4" | 
    "\t5" | 
    "\t6" | 
    "\t7" | 
    "\t8" | 
    "\t9"
)

@NLCHAR = ( 
    "\\" | 
    "\also" | 
    "\znewpage"
)

$SLASH = \x005C -- '\'
-- @CMD = ($SLASH [_\{\}\#a-zA-Z0-9]+)
@CMD = ($SLASH [_] | $SLASH [\{] | $SLASH [\}] | $SLASH [\#] | $SLASH [a-zA-Z] [a-zA-Z0-9]*)
@UNICODE = ("U+" [0-9a-fA-F]{4} | "U-" [0-9a-fA-F]{8})

-- schema name in schema environment or generic schema environment
-- @SCHNAME = ([^\}]*)
@Id = ([A-Za-z][A-Za-z0-9\\_]*)
@SCHNAME = @Id
@LISTARG = ($SPACE* @Id $SPACE* ("," $SPACE* @Id $SPACE*)*)
@GENSCHNAME = ($SPACE* @SCHNAME $SPACE* [\}] $SPACE* [\[] @LISTARG [\]])

-- environment
@BENVZSEC = ([\\] "begin" [\{] $SPACE* "zsection" $SPACE* [\}])
@EENVZSEC = ([\\] "end" [\{] $SPACE* "zsection" $SPACE* [\}])
@EEENVZSEC = ($SPACE* "end" [\{] $SPACE* "zsection" $SPACE* [\}])
@BENVAXDEF = ([\\] "begin" [\{] $SPACE* "axdef" $SPACE* [\}])
@EENVAXDEF = ([\\] "end" [\{] $SPACE* "axdef" $SPACE* [\}])
@BENVZED = ([\\] "begin" [\{] $SPACE* "zed" $SPACE* [\}])
@EENVZED = ([\\] "end" [\{] $SPACE* "zed" $SPACE* [\}])
@BENVSCH = ([\\] "begin" [\{] $SPACE* "schema" $SPACE* [\}] $SPACE* [\{] $SPACE* )
@EENVSCH = ([\\] "end" [\{] $SPACE* "schema" $SPACE* [\}] $SPACE*)
@BENVGDEF = ([\\] "begin" [\{] $SPACE* "gendef" $SPACE* [\}])
@EENVGDEF = ([\\] "end" [\{] $SPACE* "gendef" $SPACE* [\}])
@BENVTHE = ([\\] "begin" [\{] $SPACE* "theorem" $SPACE* [\}] $SPACE* [\{] $SPACE* )
@EENVTHE = ([\\] "end" [\{] $SPACE* "theorem" $SPACE* [\}] $SPACE*)
@BENVCIRC = ([\\] "begin" [\{] $SPACE* "circus" $SPACE* [\}] $SPACE* [\{] $SPACE* )
@EENVCIRC = ([\\] "end" [\{] $SPACE* "circus" $SPACE* [\}] $SPACE*)

-- | not one of the environment name from [zsection axdef zed schema gendef theorem circus]
@NOTENVNAME = ([^agsztc$SPACE] | 
                [z] ([^se] | 
                     [s] ([^e] |  -- zsection
                          [e] ([^c] | 
                               [c] ([^t] | 
                                    [t] ([^i] | 
                                         [i] ([^o] | 
                                              [o] ([^n] | 
                                                   [n] ($SPACE* [^$SPACE\}]))))))) |
                     [e] ([^d] | [d] ($SPACE* [^$SPACE\}])) -- zed
                    ) |
                [a] ([^x] |     -- axdef
                     [x] ([^d] |
                          [d] ([^e] |
                               [e] ([^f] |
                                    [f] ($SPACE* [^$SPACE\}]))))) |
                [s] ([^c] |     -- schema
                     [c] ([^h] |
                          [h] ([^e] |
                               [e] ([^m] |
                                    [m] ([^a] |
                                         [a] ($SPACE* [^$SPACE\}])))))) |
                [g] ([^e] |     -- gendef 
                     [e] ([^n] |
                          [n] ([^d] |
                               [d] ([^e] |
                                    [e] ([^f] |
                                         [f] ($SPACE* [^$SPACE\}])))))) |
                [t] ([^h] |     -- theorem 
                     [h] ([^e] |
                          [e] ([^o] |
                               [o] ([^r] |
                                    [r] ([^e] |
                                         [e] ([^m] |
                                              [m] ($SPACE* [^$SPACE\}]))))))) |
                [c] ([^i] |     -- circus 
                     [i] ([^r] |
                          [r] ([^c] |
                               [c] ([^u] |
                                    [u] ([^s] |
                                         [s] ($SPACE* [^$SPACE\}]))))))
    )

-- not zsection environment name
@NOTZSECTENVNAME = ([^z$SPACE] | 
                    [z] ([^s] | 
                         [s] ([^e] |  -- zsection
                              [e] ([^c] | 
                                   [c] ([^t] | 
                                        [t] ([^i] | 
                                             [i] ([^o] | 
                                                  [o] ([^n] | 
                                                       [n] ($SPACE* [^$SPACE\}])))))))))
-- not zed environment name
@NOTZEDENVNAME = ([^z$SPACE] | 
                    [z] ([^e] | 
                         [e] ([^d] | 
                              [d] ($SPACE* [^$SPACE\}])))) -- zed

-- not axdef environment name
@NOTAXDEFENVNAME = ([^a$SPACE] | 
                    [a] ([^x] |     -- axdef
                         [x] ([^d] |
                              [d] ([^e] |
                                   [e] ([^f] |
                                        [f] ($SPACE* [^$SPACE\}]))))))

-- not schema environment name
@NOTSCHEMAENVNAME = ([^s$SPACE] | 
                     [s] ([^c] |     -- schema
                          [c] ([^h] |
                               [h] ([^e] |
                                    [e] ([^m] |
                                         [m] ([^a] |
                                              [a] ($SPACE* [^$SPACE\}])))))))
     
-- not gendef environment name
@NOTGENDEFENVNAME = ([^g$SPACE] | 
                     [g] ([^e] |     -- gendef 
                          [e] ([^n] |
                               [n] ([^d] |
                                    [d] ([^e] |
                                         [e] ([^f] |
                                              [f] ($SPACE* [^$SPACE\}])))))))
     
-- not theorem environment name
@NOTTHEOREMENVNAME = ([^t$SPACE] | 
                      [t] ([^h] |     -- theorem 
                           [h] ([^e] |
                                [e] ([^o] |
                                     [o] ([^r] |
                                          [r] ([^e] |
                                               [e] ([^m] |
                                                    [m] ($SPACE* [^$SPACE\}]))))))))

-- not circus environment name
@NOTCIRCUSENVNAME = ([^c$SPACE] | 
                     [c] ([^i] |     -- circus 
                          [i] ([^r] |
                               [r] ([^c] |
                                    [c] ([^u] |
                                         [u] ([^s] |
                                              [s] ($SPACE* [^$SPACE\}])))))))


-- | not a valid zsection environment end 
@NOTVALIDZSECTENDENV = 
    ([^\\] | 
     [\\] ([^e] | 
           [e] ([^n] | 
                [n] ([^d] | 
                     [d] ([^\{] |
                          [\{] @NOTZSECTENVNAME |
                          [\{] $SPACE+ @NOTZSECTENVNAME
                         )
                    )
               ) 
          )
     )

-- | not a valid zed environment end 
@NOTVALIDZEDENDENV = 
    ([^\\] | 
     [\\] ([^e] | 
           [e] ([^n] | 
                [n] ([^d] | 
                     [d] ([^\{] |
                          [\{] @NOTZEDENVNAME |
                          [\{] $SPACE+ @NOTZEDENVNAME
                         )
                    )
               ) 
          )
     )

-- | not a valid axdef environment end 
@NOTVALIDAXDEFENDENV = 
    ([^\\] | 
     [\\] ([^e] | 
           [e] ([^n] | 
                [n] ([^d] | 
                     [d] ([^\{] |
                          [\{] @NOTAXDEFENVNAME |
                          [\{] $SPACE+ @NOTAXDEFENVNAME
                         )
                    )
               ) 
          )
     )

-- | not a valid schema environment end 
@NOTVALIDSCHEMAENDENV = 
    ([^\\] | 
     [\\] ([^e] | 
           [e] ([^n] | 
                [n] ([^d] | 
                     [d] ([^\{] |
                          [\{] @NOTSCHEMAENVNAME |
                          [\{] $SPACE+ @NOTSCHEMAENVNAME
                         )
                    )
               ) 
          )
     )

-- | not a valid gendef environment end 
@NOTVALIDGENDEFENDENV = 
    ([^\\] | 
     [\\] ([^e] | 
           [e] ([^n] | 
                [n] ([^d] | 
                     [d] ([^\{] |
                          [\{] @NOTGENDEFENVNAME |
                          [\{] $SPACE+ @NOTGENDEFENVNAME
                         )
                    )
               ) 
          )
     )

-- | not a valid theorem environment end 
@NOTVALIDTHEOREMENDENV = 
    ([^\\] | 
     [\\] ([^e] | 
           [e] ([^n] | 
                [n] ([^d] | 
                     [d] ([^\{] |
                          [\{] @NOTTHEOREMENVNAME |
                          [\{] $SPACE+ @NOTTHEOREMENVNAME
                         )
                    )
               ) 
          )
     )

-- | not a valid circus environment end 
@NOTVALIDCIRCUSENDENV = 
    ([^\\] | 
     [\\] ([^e] | 
           [e] ([^n] | 
                [n] ([^d] | 
                     [d] ([^\{] |
                          [\{] @NOTCIRCUSENVNAME |
                          [\{] $SPACE+ @NOTCIRCUSENVNAME
                         )
                    )
               ) 
          )
     )

-- | not a valid environment end 
@NOTVALIDENDENV = 
    ([^\\] | 
     [\\] ([^e] | 
           [e] ([^n] | 
                [n] ([^d] | 
                     [d] ([^\{] |
                          [\{] @NOTENVNAME |
                          [\{] $SPACE+ @NOTENVNAME  
                         )
                    )
               ) 
          )
     )
-----------------------------
TEX2UNI :-
-- everywhere: skip ' ' and \t 
--    ^ $SPACE+                     ;

<0, zsection, axdef, zed, schema, gendef, circus> {
    -- skip [\ \t\n\f\v\r] 
    -- whitespace are ignored 
--    $white+ ;
    [\n\f\v\r]                  ;
    ^ $white* $                 ;
    ^ $COMMENT $                ; 
    ^ $COMMENT [^$COMMENT].* $  ; 
    ^ $SPACE+ $COMMENT [^$COMMENT].* $  ; 
}

<0> {
-- comments
    @HARDSPACES                                                         { mkL T_SPACE }
    @NLCHAR                                                             { mkL T_NLCHAR }
    ^ $COMMENT $COMMENT "Zchar" $white+ @CMD $white+ @UNICODE           { mkDirZChar }
    ^ $COMMENT $COMMENT "Zprechar" $white+ @CMD $white+ @UNICODE        { mkDirZChar }
    ^ $COMMENT $COMMENT "Zinchar" $white+ @CMD $white+ @UNICODE         { mkDirZChar }
    ^ $COMMENT $COMMENT "Zpostchar" $white+ @CMD $white+ @UNICODE       { mkDirZChar }
    ^ $COMMENT $COMMENT "Zword" $white+ @CMD $white+ .* $               { mkDirZWord }
    ^ $COMMENT $COMMENT "Zpreword" $white+ @CMD $white+ .* $            { mkDirZWord }
    ^ $COMMENT $COMMENT "Zinword" $white+ @CMD $white+ .* $             { mkDirZWord }
    ^ $COMMENT $COMMENT "Zpostword" $white+ @CMD $white+ .* $           { mkDirZWord }
    ^ $SPACE* / { ifNotBeginEnv }                                       { begin noenv } 
    ^ $SPACE* / { ifBeginEnv }                                          { begin env } -- { begin2 0 env } -- push 0 but begin env
}

<noenv> {
     .* $                                                              { (\p l -> mkL (T_ANN (extractInputString p l)) p l) `andBegin` 0 } 
}

<env> {
    [\n\f\v\r]                                                          ;
    $SPACE*                                                             ;
    \\ begin \{                                                         { mkL (T_UNICODE "") } -- ^ just leave a token to record location information
    zsection $SPACE* \}                                                 { mkL (T_UNICODE zChar_zedchar) `andBegin` zsection } 
    axdef $SPACE* \}                                                    { mkL (T_UNICODE zChar_axchar) `andBegin` axdef } 
    zed $SPACE* \}                                                      { mkL (T_UNICODE zChar_zedchar) `andBegin` zed } 
    gendef $SPACE* \}                                                   { mkL (T_UNICODE (zChar_axchar ++ zChar_genchar)) `andBegin` gendef } 
    schema $SPACE* \} $SPACE* \{ / @GENSCHNAME                          { mkL (T_UNICODE (zChar_schchar ++ zChar_genchar)) `andBegin` schemaname }
    schema $SPACE* \} $SPACE* \{                                        { mkL (T_UNICODE zChar_schchar) `andBegin` schemaname }
    theorem $SPACE* \} $SPACE* \{ / @GENSCHNAME                         { mkL (T_UNICODE (zChar_conjchar ++ zChar_genchar)) `andBegin` theoremname }
    theorem $SPACE* \} $SPACE* \{                                       { mkL (T_UNICODE zChar_conjchar) `andBegin` theoremname }
    circus $SPACE* \}                                                   { mkL (T_UNICODE cChar_circchar) `andBegin` circus} 
}

<zsection, axdef, zed, schema, theorem, gendef, circus> {
    [$SPACE\n\f\v\r]                                                    ;
    -- for not a valid end environment
--    (@NOTVALIDENDENV)+                                                  { mkUnicode }
}

-- at \begin{schema}{ *SchemaName } 
<schemaname> {
    [$SPACE\n\f\v\r]                                                    ;
    -- surround space before and after name
    @SCHNAME/$SPACE* $RBRACE $SPACE*                                    { \p l -> mkL (T_UNICODE (" " ++ (replaceLatexUnderline (extractInputString p l)) ++ " ")) p l }
    $RBRACE                                                             { begin schema }
}

-- at \begin{theorem}{ *TheoremName } 
<theoremname> {
    [$SPACE\n\f\v\r]                                                    ;
    -- surround space before and after name
    @SCHNAME/$SPACE* $RBRACE $SPACE*                                    { \p l -> mkL (T_UNICODE (" " ++ (replaceLatexUnderline (extractInputString p l)) ++ " ")) p l }
    $RBRACE                                                             { begin theorem }
}

<zsection> {
    (@NOTVALIDZSECTENDENV)+                                             { mkUnicode }
    -- if there is a \end{zsection} environment in the input stream
    (@NOTVALIDZSECTENDENV)* @EENVZSEC                                   { mkUnicodeAndEndchar `andBegin` 0 }
}

<axdef> {
    (@NOTVALIDAXDEFENDENV)+                                             { mkUnicode }
    -- if there is a \end{axdef} environment in the input stream
    (@NOTVALIDAXDEFENDENV)* @EENVAXDEF                                  { mkUnicodeAndEndchar `andBegin` 0 }
}

<zed> {
    (@NOTVALIDZEDENDENV)+                                             { mkUnicode }
    -- if there is a \end{zed} environment in the input stream
    (@NOTVALIDZEDENDENV)* @EENVZED                                    { mkUnicodeAndEndchar `andBegin` 0 }
}

<schema> {
    (@NOTVALIDSCHEMAENDENV)+                                          { mkUnicode }
    -- if there is a \end{schema} environment in the input stream
    (@NOTVALIDSCHEMAENDENV)* @EENVSCH                                 { mkUnicodeAndEndchar `andBegin` 0 }
}

<theorem> {
    (@NOTVALIDTHEOREMENDENV)+                                          { mkUnicode }
    -- if there is a \end{theorem} environment in the input stream
    (@NOTVALIDTHEOREMENDENV)* @EENVTHE                                 { mkUnicodeAndEndchar `andBegin` 0 }
}

<gendef> {
    (@NOTVALIDGENDEFENDENV)+                                           { mkUnicode }
    -- if there is a \end{gendef} environment in the input stream
    (@NOTVALIDGENDEFENDENV)* @EENVGDEF                                 { mkUnicodeAndEndchar `andBegin` 0 }
}

<circus> {
    (@NOTVALIDCIRCUSENDENV)+                                           { mkUnicode }
    -- if there is a \end{circus} environment in the input stream
    (@NOTVALIDCIRCUSENDENV)* @EENVCIRC                                 { mkUnicodeAndEndchar `andBegin` 0 }
}

{
data Tok a =
      T_SPACE
    | T_NLCHAR 
    | T_COMMAND !a
-- T_DIRECT_CHAR Zchar  \command U+nnnn
-- T_DIRECT_CHAR Zchar  \command U-nnnnnnnn
-- T_DIRECT_CHAR Zinchar  \command U-nnnnnnnn
-- T_DIRECT_CHAR Zprechar  \command U-nnnnnnnn
    | T_DIRECT_CHAR !a !a !a
-- T_DIRECT_WORD Zword \command1 a b c
-- T_DIRECT_WORD Zpreword \command2 a \command1 c
-- T_DIRECT_WORD Zinword \command 
-- T_DIRECT_WORD Zpostword \command  
    | T_DIRECT_WORD !a !a !a
-- converted from LaTeX
    | T_UNICODE !a 
-- original texts from LaTeX 
    | T_ORIG !a 
-- annotation in LaTeX (out of environment) and won't be translated to Unicode
    | T_ANN  !a 
 deriving (Eq,Show,Ord)

-- | tokens 
data Token =
    -- | PT Posn (Tok str) (Just "original latex string") (converted Unicode string) partitions of unicode string with location
    -- such as: PT AlexPosn (T_UNICODE "acmdb") (Just "a \\cmd b") ("acmdb") 
    -- [("a", SrcSpanInfo (SrcSpan "test.tex" 2 1 2 1) []), 
    --  ("cmd", SrcSpanInfo (SrcSpan "test.tex" 2 3 2 6) [(SrcSpan "test.tex" 1 1 1 10)])
    --  ("b", SrcSpanInfo (SrcSpan "test.tex" 2 7 2 7) [])
    -- ]
   PT AlexPosn (Tok String) (Maybe String) (String) [(String, (SrcSpanInfo))]
   -- | PT1 SourceFile Posn (Tok str) (Just "original zstring") (converted Unicode string)
 | PT1 (String) AlexPosn (Tok String) (Maybe String) (String) [(String, (SrcSpanInfo))]
 | Err AlexPosn
 | Err1 (String) AlexPosn
 | EOF
  deriving (Eq,Show)

tokenPos :: [Token] -> String
tokenPos (PT (AlexPn _ l _) _ _ _ _:_) = "[" ++ "default" ++ "] line " ++ show l
tokenPos (PT1 f (AlexPn _ l _) _ _ _ _:_) = "[" ++ f ++ "] line " ++ show l
tokenPos (Err (AlexPn _ l _) :_) = "[" ++ "default" ++ "] line " ++ show l
tokenPos (Err1 f (AlexPn _ l _) :_) = "[" ++ f ++ "] line " ++ show l
tokenPos _ = "end of file"

tokenPosn :: Token -> AlexPosn
tokenPosn (PT p _ _ _ _) = p
tokenPosn (PT1 f p _ _ _ _) = p
tokenPosn (Err p) = p
tokenPosn (Err1 f p) = p

tokenLineCol :: Token -> (Int, Int)
tokenLineCol = posLineCol . tokenPosn

posLineCol :: AlexPosn -> (Int, Int)
posLineCol (AlexPn _ l c) = (l,c)

mkPosToken :: Token -> ((Int, Int), String)
mkPosToken t@(PT p _ _ _ _) = (posLineCol p, prToken t)
mkPosToken t@(PT1 f p _ _ _ _) = (posLineCol p, prToken t)

prToken :: Token -> String
prToken t = case t of
  PT _ _ (Just s) _ _   -> s
  PT1 f _ _ (Just s) _ _ -> s
  Err _                 -> "Error"
  Err1 f _              -> "Error"
  EOF                   -> "EOF"

-------------------------------------------------------------------
-- predicate used in right context such as "^ ab / { pred }     { action }"
-- 
-- AlexInput (
--      AlexPosn,   -- (offset, line, column)
--      Char,       -- previous char
--      [Byte],     -- rest of the bytes for the current char
--      String      -- current input string
--  )
-------------------------------------------------------------------
-- | Z Environments
envList :: [String]
envList = ["zsection", "zed", "axdef", "schema", "gendef", "theorem", "circus"]

-- | Unicode of Z Environments
envListUni :: [String]
envListUni = [zChar_zedchar, zChar_axchar, (zChar_axchar ++ zChar_genchar), (zChar_schchar ++ zChar_genchar), zChar_schchar, (zChar_conjchar ++ zChar_genchar), zChar_conjchar, cChar_circchar]

-- | check if the input string is at the begin or end of an valid environment
ifAtEnv :: String   -- ^ input string 
    -> String       -- ^ "begin" or "end" 
    -> Bool         -- True - at the specified environment
ifAtEnv ins c =  
    case isAnEnv ["\\" ++ c ++ "{"] (trimLeadAndEndSpace ins) of
         (True, left)   -> case isAnEnv envList (trimLeadAndEndSpace left) of
                                (True, left')   -> if ['}'] `isPrefixOf` (trimLeadAndEndSpace left')
                                                   then True
                                                   else False
                                (False, _)     -> False
         (False, _)     -> False
 where   isAnEnv [] ins = (False, "")
         isAnEnv (x:xs) ins = if x `isPrefixOf` ins 
                                then case stripPrefix x ins of
                                        Just left   -> (True, left)
                                        Nothing     -> (False, "")
                                else isAnEnv xs ins

-- | check if it isn't at the beginning of an valid environment
ifNotBeginEnv :: user    -- predicate state 
    -> AlexInput    -- input stream before the token 
    -> Int          -- length of the token 
    -> AlexInput    -- input stream after the token
    -> Bool         -- True <=> accept the token
ifNotBeginEnv _ _ _ (_, _, _, input) = not (ifAtEnv input "begin") 

-- | check if it is at the beginning of an valid environment
ifBeginEnv :: user       -- predicate state 
    -> AlexInput    -- input stream before the token 
    -> Int          -- length of the token 
    -> AlexInput    -- input stream after the token
    -> Bool         -- True <=> accept the token
ifBeginEnv _ _ _ (_, _, _, input) = (ifAtEnv input "begin")

-- | check if it isn't at the end of an valid environment
ifNotEndEnv :: user -- predicate state 
    -> AlexInput    -- input stream before the token 
    -> Int          -- length of the token 
    -> AlexInput    -- input stream after the token
    -> Bool         -- True <=> accept the token
ifNotEndEnv _ _ _ (_, _, _, input) = not (ifAtEnv input "end") 

-- | check if it is at the end of an valid environment
ifEndEnv :: user       -- predicate state 
    -> AlexInput    -- input stream before the token 
    -> Int          -- length of the token 
    -> AlexInput    -- input stream after the token
    -> Bool         -- True <=> accept the token
ifEndEnv _ _ _ (_, _, _, input) = (ifAtEnv input "end")

-- | check if a substring exists and return the index if so.
substring :: String -> String -> Maybe Int
substring _ []  = Nothing
substring sub str = case isPrefixOf sub str of
  False -> fmap (+1) $ substring sub (tail str)
  True  -> Just 0

-- | Check if the input string contains end environment or not
-- 
-- If it is true, the index of end environment is returned as well
containEndEnv :: String -> (Bool, Int)
containEndEnv [] = (False, 0)
containEndEnv ins = 
    case substring "\\end{" ins of
        Nothing     -> (False, 0)
        Just ind    -> case ifAtEnv (drop ind ins) "end" of
            True        -> (True, ind)
            False       -> containEndEnv (drop (ind + 5) ins)

-- | check if the input string doesn't contain a valid end environment
ifNotContainEndEnv :: user       -- predicate state 
    -> AlexInput    -- input stream before the token 
    -> Int          -- length of the token 
    -> AlexInput    -- input stream after the token
    -> Bool         -- True <=> accept the token
ifNotContainEndEnv _ _ _ (_, _, _, input) = case (containEndEnv input) of
    (True, _)   -> False
    (False, _)   -> True

-- | check if the input string does contain a valid end environment
ifContainEndEnv :: user       -- predicate state 
    -> AlexInput    -- input stream before the token 
    -> Int          -- length of the token 
    -> AlexInput    -- input stream after the token
    -> Bool         -- True <=> accept the token
ifContainEndEnv _ _ _ (_, _, _, input) =  case (containEndEnv input) of
    (True, _)   -> True 
    (False, _)   -> False 

-------------------------------------------------------------------
-- Action 
-------------------------------------------------------------------
-- | similar to begin, but additionally push code 
begin1 :: Int -> AlexAction Token
begin1 code _str _len = do 
    pushLexState code
    begin code _str _len

-- | push code1 but begin code2
begin2 :: Int -> Int -> AlexAction Token
begin2 code1 code2 _str _len = do 
    pushLexState code1
    begin code2 _str _len

-- | similar to andBegin, but additionally push code 
andBegin1 :: AlexAction result -> Int -> AlexAction result
(action `andBegin1` code) input len = do 
    pushLexState code
    (action `andBegin` code) input len

-- | pop a code and then begin it 
pop :: AlexAction Token
pop _buf _len = do 
    code <- popLexState
    begin code _buf _len

-- | pop a code, execute the action, and then begin the code
andPop action input len = do
    code <- popLexState
    (action `andBegin` code) input len

-- | pop two codes, execute the action, and then begin the last poped code
andPop2 action input len = do
    _ <- popLexState
    code <- popLexState
    (action `andBegin` code) input len

-- | extract input string 
extractInputString :: AlexInput -> Int -> String
extractInputString (p,_,_,str) len = (take len str) 

-- | make a token from input string
-- 
-- AlexInput (posn, char, [byte], input)
mkL :: Tok String -> AlexInput -> Int -> Alex Token
mkL (T_UNICODE u) (p@(AlexPn _ l c),_,_,str) len = do
    f <- getCurFile 
    if u `elem` envListUni 
    then  return (PT p (T_UNICODE u) (Just (take len str)) u [(u, (noInfoSpan (SrcSpan f l (c) l (c+len-1))))])
    else return (PT p (T_UNICODE u) (Just (take len str)) u [(u, (noInfoSpan (SrcSpan f l c l (c+len-1))))])
mkL c1 (p@(AlexPn _ l c),_,_,str) len = do
    f <- getCurFile 
    return (PT p c1 (Just inp) inp [(inp, (noInfoSpan (SrcSpan f l c l (c+len-1))))])
  where inp = (take len str)

-- | cope with the situation in which the input string contains a valid end environment.
-- 
-- - the string before the environment will be turned to a UNICODE token
-- - the environment will be turned to a END char (unicode token)  
-- - the string after the environment is discarded, so multiple environments in one line are not supported, 
--    such as "abc \\end{zed} \\begin{zed} add" (TODOs: state the limitation)
-- 
-- for example, "abc \\end{schema} de" will return unicode ("abc" ++ endchar)
mkUnicodeAndEndchar :: AlexInput -> Int -> Alex Token
mkUnicodeAndEndchar s@(p@(AlexPn _ l c),_,_,str) len = 
    case containEndEnv inp of 
        (True, ind) -> do
            f <- getCurFile
            if ind == 0
            then do 
                return (PT p (T_UNICODE (zChar_endchar ++ "\n")) (Just (inp)) (zChar_endchar ++ "\n") ([((zChar_endchar ++ "\n"), (noInfoSpan (SrcSpan f l (c) l (c+len-1))))]))
            else do
                ret <- zstring2UniString (take ind inp) l c
                case ret of
                    Left e -> lexerError e
                    Right r' -> do -- check if environment matches (\begin{axdef} matches \end{axdef} but not \end{zed})
                        return (PT p (T_UNICODE ((assembleString r')++(zChar_endchar ++ "\n"))) (Just (inp)) ((assembleString r')++(zChar_endchar ++ "\n")) (r' ++ [((zChar_endchar ++ "\n"), (noInfoSpan (SrcSpan f l (c+ind) l (c+len-1))))]))
        (False, _)  -> lexerError (err2Msg (ErrTex2Uni (ET2UNoEndEnv ("NO an \\end{} environment"))))
  where inp = take len str
 
-- | convert string to Unicode
mkUnicode :: AlexInput -> Int -> Alex Token
mkUnicode s@(p@(AlexPn _ l c),_,_,str) len = do
            ret <- zstring2UniString inp l c 
            case ret of
                Left e -> lexerError e
                Right r' -> return (PT p (T_UNICODE (assembleString r')) (Just (inp)) (assembleString r') r')
    where inp = take len str

-- | extract a word from the beginning of String and return the word and remaining, and also the index of remaining
-- 
-- For example,  
--  extractWord "Zchar \cmd U+0088"   =>  ("Zchar", "\cmd U+0088", index)
extractWord :: String -> (String, String, Int)
extractWord inp = ext (inp) 1 ([], False)
    -- string to process, index of char in processing, processed word and whether a word is done or not
    -- for "Zchar \cmd U+0088", done to True when processing the space after Zchar
    where ext [] idx (w, done) = (w, [], idx)
          ext (x:xs) idx (w, done) = if isSpace x 
                                then if (null w)
                                        then (ext xs (idx + 1) (w, False) )
                                        else if done 
                                                then (ext xs (idx + 1) (w, done) )
                                                else (ext xs (idx + 1) (w, True) )
                                else if done
                                        then (w, (x:xs), idx) 
                                        else (ext xs (idx + 1) (w ++ [x], done) )

-- | convert from unicode "U+1234" or "U-12345678" to corresponding unicode
unicodeToChar :: String -> Char
unicodeToChar inp = thechar
    where thechar = case readLitChar ("\\x" ++ (drop 2 inp)) of 
                        (c, r):xs    -> c

-- | from "%%Zchar \cmd U+0088" to (Zchar, "\cmd", "U+0088", 14)
extractDirectives :: String -> (String, String, String, Int)
extractDirectives inp = case inp of
    '%':'%':xs -> let   (w1, r1, idx1) = extractWord xs
                        (w2, r2, idx2) = extractWord r1
                        (w3, r3, idx3) = extractWord r2
                  in 
                        (w1, w2, r2, (2+idx1+idx2-1))    -- don't need to return w3 since r2 is more accurate
    _          -> ("", "", "", 0)

-- | from "%%Zchar \cmd U+0088" to PT p (T_DIRECT_CHAR (Zchar, "\cmd", "U+0088")) (Just "")
mkDirZChar :: AlexInput -> Int -> Alex Token
mkDirZChar (p@(AlexPn _ l c),_,_,str) len = do
    f <- getCurFile
    addToCmdMap cmd zstring (noInfoSpan (SrcSpan f l c l (c+len-1)))
    return (PT p (T_DIRECT_CHAR zch cmd def) (Just inp) zstring [(zstring, (noInfoSpan (SrcSpan f l idx l (c+len-1))))])
    where inp = take len str
          (zch, cmd, def, idx) = extractDirectives inp
          zstring = case zch of 
                        "Zchar"      -> ("" ++ [(unicodeToChar def)])
                        "Zprechar"   -> ([(unicodeToChar def)] ++ " ")
                        "Zinchar"    -> (" " ++ [(unicodeToChar def)] ++ " ")
                        "Zpostchar"  -> (" " ++ [(unicodeToChar def)])

-- | from "%%Zword \cmd cmd" to PT p (T_DIRECT_WORD (Zchar, "\cmd", "U+0088")) (Just "")
mkDirZWord :: AlexInput -> Int -> Alex Token
mkDirZWord (p@(AlexPn _ l c),_,_,str) len = do
    r <- zstring2UniString def l idx 
    case r of
        Left e -> lexerError e 
        Right r' -> do  
            f <- getCurFile
            addToCmdMap cmd str (noInfoSpan (SrcSpan f l c l (c+len-1)))
            return (PT p (T_DIRECT_WORD zch cmd def) (Just inp) str span)
          where (str, span) = zstring (assembleString r') r'
    where inp = take len str
          (zch, cmd, def, idx) = extractDirectives inp
          -- d - string, lst - a list of string with location [(String, SrcSpanInfo)]
          zstring d lst = case zch of 
                        "Zword"      -> ("" ++ d, lst)
                        "Zpreword"   -> (d ++ " ", lst ++ [(" ", noSrcSpan)])
                        "Zinword"    -> (" " ++ d ++ " ", [(" ", noSrcSpan)] ++ lst ++ [(" ", noSrcSpan)])
                        "Zpostword"  -> (" " ++ d, [(" ", noSrcSpan)] ++ lst)

-- | add a command's definition to cmdMap
--
-- For example,  
--  '%%Zinchar \where U+007C'
--  will call 'addToCmdMap "where" U+007C span'
addToCmdMap :: String       -- ^ command 
            -> String       -- ^ its definition
            -> SrcSpanInfo  -- ^ command's source location information 
            -> Alex ()
addToCmdMap "" zstring _ = lexerError (err2Msg (ErrTex2Uni (ET2UCmdEmpty ("Latex Command can not be empty!"))))
addToCmdMap cmd ""     _ = lexerError (err2Msg (ErrTex2Uni (ET2UCmdEmpty ("The corresponding ZString of [" ++ cmd ++ "] can not be empty!"))))
addToCmdMap cmd zstring span = do
    m <- getCmdMap 
    case Map.lookup cmd m of
        Just r -> lexerError (err2Msg (ErrTex2Uni (ET2UCmdRedef (cmd))))
        Nothing -> setCmdMap (Map.insert cmd (zstring, span) m) 

-- | get a command's definition and source location from CmdMap
getFromCmdMap :: String -> Alex (Either String (String, SrcSpanInfo))
getFromCmdMap cmd = do
    m <- getCmdMap 
    case Map.lookup ("\\" ++ cmd) m of
        Just r -> return (Right r)
        Nothing -> return (Left (err2Msg (ErrTex2Uni (ET2UCmdNotDef ("\\" ++ cmd)))))

data PMode = 
      M_CHAR             -- ^ general mode and a char is regarded as a normal char
    | M_CMDSLASH         -- ^ just after slash (enter by '\' and exit by a char) 
    | M_CMD              -- ^ in command mode (enter from M_CMDSLASH and exit by a software/hard space, _, {, }, etc)
    | M_INBRACE !Int     -- ^ within brace (enter by {, and exit by }, depth level is an argument)
    | M_UNDERSCR         -- ^ subscript (enter by _, and exit by next char or {})
    | M_SUPERSCR         -- ^ supscript (enter by ^, and exit by next char or {})
    | M_NA               -- ^ NA: special
  deriving (Eq,Show)

-- | pop out modes from the head of the list till there is a M_INBRACE mode
-- it is used in } when a complete brace is solved.
-- return (list of modes after { popped, last mode)
popBraceMode :: [PMode] -> ([PMode], PMode)
popBraceMode [] = ([], M_NA) 
popBraceMode (m:ms) = case m of 
    M_INBRACE l     -> case ms of
                        M_UNDERSCR:xs   -> (xs, M_UNDERSCR)
                        M_SUPERSCR:xs   -> (xs, M_SUPERSCR)
                        _               -> (ms, M_INBRACE l)
    _               -> popBraceMode ms 

-- | assemble the string that is represetned in the input list of string with locations
assembleString :: [(String, SrcSpanInfo)] -> String
assembleString [] = [] 
assembleString ((s, _):xs) = s ++ (assembleString xs)

-- | Parse a Latex zstring by skipping soft spaces and replacing commands
-- 
-- Specs:
--  1. soft spaces (skipped): ' ', '\t', '\b', '\n', '\r' etc
--  2. hard spaces (to SPACE): ~ \, \; \: '\ ' \t1 ... \t9
--  3. hard spaces (to NLCHAR): \\ \also \znewpage 
--  4. '\cmd' (to its definition): such as "a \cmd b" to "a c b" if "\cmd" is defined as inword and " c " 
--  5. '\cmd_1' (to its definition + "SE 1 NW"): such as "a \cmd_1 b" to "a c↘1↖  b"
--  6. '\cmd~_1' (to its definition + " SE 1 NW"): such as "a \cmd~_1 b" to "a c ↘1↖ b" (note no space before b)
--  7. '{\cmd}' (to its definition without spaces in two ends): such as "a {\cmd} b" to "acb" (note no space before b)
--  8. '{~\cmd~}' (to its definition and at least one space is kept in each end): such as "a {~\cmd~} b" to "a c b" (note one space around c)
-- 
-- For example,
--  a b                     => a b
--  a \cmd b                => a c b (if \cmd is defined as c)
--
-- Return a translated string with a location map. For example
--  zstring2UniString "a \cmd b" 5 3        
--      => ("acb", [(SrcSpan "" 5 1 5 1, SrcSpan "" 5 3 5 3), 
--                  (SrcSpan "" 5 2 5 2, SrcSpan "" 5 4 5 9), 
--                  (SrcSpan "" 5 3 5 3, SrcSpan "" 5 10 5 10)]
--         )
zstring2UniString :: String -- ^ string to be processed
    -> Int                  -- ^ line number 
    -> Int                  -- ^ column number 
    -> Alex (Either String [(String, SrcSpanInfo)])  -- ^ return a translated string with a location map [(SrcSpan l 3 l 5, SrcSpan l 4 l 15)]
zstring2UniString inp line col = do
    f <- getCurFile
    scan inp [M_CHAR] ' ' [] 0 [] [] f line 0 col

-- scan inp ListOfModes LastCh Cmd IntermediateResult FinalOut 
-- Parameters 
--  inp         : the zstring to be processed 
--  modelist    : a mode stack (head of list as top of the stack) 
--  lastch      : last char processed 
--  cmd         : a list to store the cmd in processing, command can not be embedded
--  depth       : an integer for the depth level of '{' and '}'
--  interlist   : a stack of intermediate results, used to store the temporary representation of zstring in '{' and '}'
--                it is embedded. So "{ab {\cmd} e}" will have ([" cmd ", "ab"] => ["abcmd"] => ["abcmde"])
--  result      : the output final result, only allow to append and no replacement or modification made 
--  colsrc      : the column of char in processing
--  coldst      : the column of translated char 
--  span        : current span list 
scan ::     String  -- ^ string to be processed
        -> [PMode]  -- ^ mode stack
        -> Char     -- ^ last char being processed 
        -> String   -- ^ command in processing 
        -> Int      -- ^ depth level of '{' and '}' 
        -> [[(String, SrcSpanInfo)]] -- ^ list of string and each one match a '{' and '}'. Such as [[("a", {SrcSpan, [SrcSpan]}],[]]
        -> [(String, SrcSpanInfo)]   -- ^ string that has been processed and translated
        -> String   -- ^ file of source input string 
        -> Int      -- ^ line of source input string 
        -> Int      -- ^ start column of command 
        -> Int      -- ^ start column of source input string 
        -> Alex (Either String [(String, SrcSpanInfo)])  -- ^ return a translated string with a location map [(SrcSpan l 3 l 5, SrcSpan l 4 l 15)]
scan [] (m:ms) c cmd d inter r f line colcmd colsrc = case d of
    0       -> case m of
        M_CHAR      ->  return (Right r)
        M_CMDSLASH  ->  return (Left (err2Msg (ErrTex2Uni (ET2ULexer ("The [" ++ [c] ++ "] cannot be last char in ZString!")))))
        M_CMD       ->  do 
                rm <- getFromCmdMap cmd
                case rm of 
                    Left e  -> return (Left e) 
                    Right (r', span) -> case ms of 
                         (M_UNDERSCR:M_CMD:xms)  -> return (Right (concatendIsMathSpan r (concatendSpan (head inter) [(zSubscript r', span')])))
                         (M_UNDERSCR:xms)        -> return (Right (concatendIsMathSpan r [(zSubscript r', span')]))
                         (M_SUPERSCR:M_CMD:xms)  -> return (Right (concatendIsMathSpan r (concatendSpan (head inter) [(zSupscript r', span')])))
                         (M_SUPERSCR:xms)        -> return (Right (concatendIsMathSpan r [(zSupscript r', span')]))
                         _                       -> return (Right (r ++ [(r', span')]))
                      -- the last srcInfoPoints is to keep the location of \cmd in the input string
                      where span' = infoSpan (srcInfoSpan span) ((srcInfoPoints span) ++ [(SrcSpan f line colcmd line colsrc)])
        M_INBRACE l ->  return (Left (err2Msg (ErrTex2Uni (ET2UUnbalStruct ("Unbalanced brace '{' in ZString!")))))
        M_UNDERSCR  ->  return (Left (err2Msg (ErrTex2Uni (ET2UUnbalStruct ("[_] is not complete in ZStirng!")))))
        M_SUPERSCR  ->  return (Left (err2Msg (ErrTex2Uni (ET2UUnbalStruct ("[^] is not complete in ZStirng!")))))
    _       -> return (Left (err2Msg (ErrTex2Uni (ET2UUnbalStruct ("Unbalanced brace '{' and '}' in ZString!")))))
scan (x:xs) mo@(m:ms) c cmd d inter r f line colcmd colsrc = case x of
    '\\'        -> case m of
          M_CHAR      ->  scan xs (M_CMDSLASH:mo) x [] d inter r f line (colsrc) (colsrc+1) -- M_CMDSLASH put in stack to be current mode
          M_CMDSLASH  ->  case d of
                      -- \\ => NLCHAR, the top mode M_CMDSLASH is popped
                  0   -> case ms of
                          (M_UNDERSCR:M_CMD:xms)  -> scan xs (xms) x [] d (tail inter) (r ++ (concatendSpan (head inter) subNL)) f line (0) (colsrc+1) 
                          (M_UNDERSCR:xms)        -> scan xs (xms) x [] d (inter) (r ++ subNL) f line (0) (colsrc+1) 
                          (M_SUPERSCR:M_CMD:xms)  -> scan xs (xms) x [] d (tail inter) (r ++ (concatendSpan (head inter) supNL)) f line (0) (colsrc+1) 
                          (M_SUPERSCR:xms)        -> scan xs (xms) x [] d (inter) (r ++ supNL) f line (0) (colsrc+1) 
                          _                       -> scan xs (ms) x [] d inter (r ++ normNL) f line (0) (colsrc+1) 
                      -- in braces, then NLCHAR is pushed into the top of string in interlist
                  _   -> case ms of
                          (M_UNDERSCR:M_CMD:xms)  -> scan xs (ms) x [] d ([(head (drop 1 inter)) ++ (concatendSpan (head inter) subNL)] ++ (drop 2 inter)) (r) f line (0) (colsrc+1) 
                          (M_UNDERSCR:xms)        -> scan xs (ms) x [] d ([(head inter) ++ subNL] ++ (drop 1 inter)) (r) f line (0) (colsrc+1) 
                          (M_SUPERSCR:M_CMD:xms)  -> scan xs (ms) x [] d ([(head (drop 1 inter)) ++ (concatendSpan (head inter) supNL)] ++ (drop 2 inter)) (r)  f line (0) (colsrc+1) 
                          (M_SUPERSCR:xms)        -> scan xs (ms) x [] d ([(head inter) ++ supNL] ++ (drop 1 inter)) (r) f line (0) (colsrc+1) 
                          _                       -> scan xs (ms) x [] d ([(head inter) ++ normNL] ++ (drop 1 inter)) (r) f line (0) (colsrc+1) 
              where subNL = [(zSubscript [zNLchar], noInfoSpan (SrcSpan f line colcmd line colsrc))]
                    supNL = [(zSupscript [zNLchar], noInfoSpan (SrcSpan f line colcmd line colsrc))]
                    normNL = [([zNLchar], noInfoSpan (SrcSpan f line colcmd line colsrc))]
          M_CMD       ->  do 
               rm <- getFromCmdMap cmd
               case rm of 
                   Left e  -> return (Left e) 
                   -- a '\\' can terminate a command and current mode becomes M_CMDSLASH
                   Right (r', span) -> case d of
                           0  -> scan xs (M_CMDSLASH:ms) x [] d inter (r ++ [(r', span')]) f line (colsrc) (colsrc+1) 
                           _  -> scan xs (M_CMDSLASH:ms) x [] d ([(head inter) ++ [(r', span')]] ++ (drop 1 inter)) (r) f line (colsrc) (colsrc+1) 
                      -- the last srcInfoPoints is to keep the location of \cmd in the input string
                      where span' = infoSpan (srcInfoSpan span) ((srcInfoPoints span) ++ [(SrcSpan f line colcmd line (colsrc-1))])
          M_INBRACE l -> scan xs (M_CMDSLASH:mo) x [] d inter r f line (colsrc) (colsrc+1)
          M_UNDERSCR  -> scan xs (M_CMDSLASH:mo) x [] d inter r f line (colsrc) (colsrc+1)  -- M_CMDSLASH put in stack to be current mode
          M_SUPERSCR  -> scan xs (M_CMDSLASH:mo) x [] d inter r f line (colsrc) (colsrc+1)  -- M_CMDSLASH put in stack to be current mode
    '{'         -> case m of
          -- push M_INBRACE mode in the stack, and push an empty intermediate stirng into interlist
          M_CHAR      -> scan xs ((M_INBRACE d):mo) x [] (d+1) ([[]] ++ inter) r f line 0 (colsrc+1) 
          -- pop out M_CMDSLASH mode from the stack
          M_CMDSLASH  -> case d of
                   0  -> scan xs (ms) x [] d [] (r ++ [(['{'], noInfoSpan (SrcSpan f line colcmd line colsrc))]) f line 0 (colsrc+1)     -- '\{' => '{' 
                   _  -> scan xs (ms) x [] d ([(head inter) ++ [(['{'], noInfoSpan (SrcSpan f line colcmd line colsrc))]] ++ (drop 1 inter)) (r) f line 0 (colsrc+1)
          M_CMD       ->  do 
                              rm <- getFromCmdMap cmd
                              case rm of 
                                  Left e  -> return (Left e) 
                                  Right (r', span) -> case d of
                                          0  -> scan xs ((M_INBRACE d):ms) x [] (d+1) ([[]] ++ inter) (r ++ [(r', span')]) f line 0 (colsrc+1)
                                          _  -> scan xs ((M_INBRACE d):ms) x [] (d+1) ([[]] ++ [(head inter) ++ [(r', span')]] ++ (drop 1 inter)) (r) f line 0 (colsrc+1)
                                    -- the last srcInfoPoints is to keep the location of \cmd in the input string
                                    where span' = infoSpan (srcInfoSpan span) ((srcInfoPoints span) ++ [(SrcSpan f line colcmd line (colsrc-1))])
          -- the depth is incremented by 1 
          M_INBRACE l -> scan xs ((M_INBRACE (d+1)):mo) x [] (d+1) ([[]] ++ inter) r f line 0 (colsrc+1)
          M_UNDERSCR  -> scan xs ((M_INBRACE (d+1)):mo) x [] (d+1) ([[]] ++ inter) r f line 0 (colsrc+1)
          M_SUPERSCR  -> scan xs ((M_INBRACE (d+1)):mo) x [] (d+1) ([[]] ++ inter) r f line 0 (colsrc+1)
    '}'         -> case m of
          -- push M_INBRACE mode in the stack
          M_CHAR      ->  case d of
                   0  -> return (Left ("Unbalanced brace in ZString: no matched open brace '{'!"))
                   -- check if the mode before '{' is _ or ^
                   1  -> case popBraceMode ms of
                          -- the head of inter for {}, the second for the \cmd
                          ((M_CMD:xms), M_UNDERSCR)    -> scan xs (xms) x [] (d-1) []  (r ++ (concatendIsMathSpan (head (drop 1 inter)) (zSubSpan (trimBothSpaceSpan (head inter))))) f line (0) (colsrc+1) 
                          (xms, M_UNDERSCR)    -> scan xs (xms) x [] (d-1) []  (concatendIsMathSpan r (zSubSpan (trimBothSpaceSpan (head inter)))) f line 0 (colsrc+1) 
                          ((M_CMD:xms), M_SUPERSCR)    -> scan xs (xms) x [] (d-1) []  (r ++ (concatendIsMathSpan (head (drop 1 inter)) (zSupSpan (trimBothSpaceSpan (head inter))))) f line (0) (colsrc+1) 
                          (xms, M_SUPERSCR)    -> scan xs (xms) x [] (d-1) []  (concatendIsMathSpan r (zSupSpan (trimBothSpaceSpan (head inter)))) f line (0) (colsrc+1) 
                          (xms, _)             -> scan xs (xms) x [] (d-1) []  (r ++ inter') f line (0) (colsrc+1) 
                                                where inter' = (trimBothSpaceSpan (head inter))
                         -- 1st one appended in the 2nd one. for example, ["cmd","a","c"] => ["acmd", "c"]
                   _  -> case popBraceMode ms of
                          -- the head of inter for {}, the second for the \cmd, the third for the upper level {}
                          ((M_CMD:xms), M_UNDERSCR)    -> scan xs (xms) x [] (d-1) ([(head (drop 2 inter)) ++ (concatendIsMathSpan (head (drop 1 inter)) (zSubSpan (trimBothSpaceSpan (head inter))))] ++ (drop 3 inter)) r  f line (0) (colsrc+1) 
                          (xms, M_UNDERSCR)    -> scan xs (xms) x [] (d-1) ([concatendIsMathSpan (head (drop 1 inter)) (zSubSpan (trimBothSpaceSpan (head inter)))] ++ (drop 2 inter))  (r) f line (0) (colsrc+1) 
                          ((M_CMD:xms), M_SUPERSCR)    -> scan xs (xms) x [] (d-1) ([(head (drop 2 inter)) ++ (concatendIsMathSpan (head (drop 1 inter)) (zSupSpan (trimBothSpaceSpan (head inter))))] ++ (drop 3 inter)) r  f line (0) (colsrc+1) 
                          (xms, M_SUPERSCR)    -> scan xs (xms) x [] (d-1) ([concatendIsMathSpan (head (drop 1 inter)) (zSupSpan (trimBothSpaceSpan (head inter)))] ++ (drop 2 inter))  (r) f line (0) (colsrc+1) 
                          (xms, _)             -> scan xs (xms) x [] (d-1) ([(head (drop 1 inter)) ++ (trimBothSpaceSpan (head inter))] ++ (drop 2 inter))  (r) f line (0) (colsrc+1) 
          -- pop out M_CMDSLASH mode from the stack
          M_CMDSLASH  ->  case d of
                   0  -> scan xs (ms) x [] d [] (r ++ [(['}'], noInfoSpan (SrcSpan f line colcmd line colsrc))]) f line 0 (colsrc+1) -- '\}' => '}' 
                   _  -> scan xs (ms) x [] d ([(head inter) ++ [(['}'], noInfoSpan (SrcSpan f line colcmd line colsrc))]] ++ (drop 1 inter)) (r) f line 0 (colsrc+1) 
          M_CMD       ->  do 
                              rm <- getFromCmdMap cmd
                              case rm of 
                                  Left e  -> return (Left e) 
                                  Right (r', span) -> case d of
                                      0  -> return (Left (err2Msg (ErrTex2Uni (ET2UUnbalStruct ("Unbalanced brace in ZString: no matched open brace '{'!")))))
                                      1  -> case popBraceMode ms of
                                          -- the head of inter for {}, the second for the \cmd
                                             ((M_CMD:xms), M_UNDERSCR)    -> scan xs (xms) x [] (d-1) []  (r ++ (concatendIsMathSpan (head (drop 1 inter)) (zSubSpan (trimBothSpaceSpan ((head inter) ++ [(r', span')]))))) f line 0 (colsrc+1)
                                             (xms, M_UNDERSCR)    -> scan xs (xms) x [] (d-1) []  (concatendIsMathSpan r (zSubSpan (trimBothSpaceSpan ((head inter) ++ [(r', span')])))) f line 0 (colsrc+1)
                                             ((M_CMD:xms), M_SUPERSCR)    -> scan xs (xms) x [] (d-1) []  (r ++ (concatendIsMathSpan (head (drop 1 inter)) (zSupSpan (trimBothSpaceSpan ((head inter) ++ [(r', span')]))))) f line 0 (colsrc+1)
                                             (xms, M_SUPERSCR)    -> scan xs (xms) x [] (d-1) []  (concatendIsMathSpan r (zSupSpan (trimBothSpaceSpan ((head inter) ++ [(r', span')])))) f line 0 (colsrc+1)
                                             (xms, _)             -> scan xs (xms) x [] (d-1) []  (r ++ (trimBothSpaceSpan ((head inter) ++ [(r', span')]))) f line 0 (colsrc+1)
                                            -- 1st one appended in the 2nd one. for example, ["cmd","a","c"] => ["acmd", "c"]
                                      _  -> case popBraceMode ms of
                                             -- the head of inter for {}, the second for the \cmd, the third for the upper level {}
                                             ((M_CMD:xms), M_UNDERSCR)    -> scan xs (xms) x [] (d-1) ([(head (drop 2 inter)) ++ (concatendIsMathSpan (head (drop 1 inter)) (zSubSpan (trimBothSpaceSpan ((head inter) ++ [(r', span')]))))] ++ (drop 3 inter)) r f line 0 (colsrc+1)
                                             (xms, M_UNDERSCR)    -> scan xs (xms) x [] (d-1) ([concatendIsMathSpan (head (drop 1 inter)) (zSubSpan (trimBothSpaceSpan ((head inter) ++ [(r', span')])))] ++ (drop 2 inter))  (r) f line 0 (colsrc+1)
                                             ((M_CMD:xms), M_SUPERSCR)    -> scan xs (xms) x [] (d-1) ([(head (drop 2 inter)) ++ (concatendIsMathSpan (head (drop 1 inter)) (zSupSpan (trimBothSpaceSpan ((head inter) ++ [(r', span')]))))] ++ (drop 3 inter)) r  f line 0 (colsrc+1)
                                             (xms, M_SUPERSCR)    -> scan xs (xms) x [] (d-1) ([concatendIsMathSpan (head (drop 1 inter)) (zSupSpan (trimBothSpaceSpan ((head inter) ++ [(r', span')])))] ++ (drop 2 inter))  (r) f line 0 (colsrc+1)
                                             (xms, _)             -> scan xs (xms) x [] (d-1) ([(head (drop 1 inter)) ++ (trimBothSpaceSpan ((head inter) ++ [(r', span')]))] ++ (drop 2 inter))  (r) f line 0 (colsrc+1)
                                    -- the last srcInfoPoints is to keep the location of \cmd in the input string
                                    where span' = infoSpan (srcInfoSpan span) ((srcInfoPoints span) ++ [(SrcSpan f line colcmd line (colsrc-1))])
          -- the depth is incremented by 1 
          M_INBRACE l -> case d of
                 0  -> return (Left (err2Msg (ErrTex2Uni (ET2UUnbalStruct ("Unbalanced brace '{' and '}' in ZString: depth cannot be zero when it is in the INBRACE mode.")))))
                 1  -> scan xs (ms) x [] (d-1) []  (r ++ (trimBothSpaceSpan (head inter))) f line 0 (colsrc+1)
                        -- 1st one appended in the 2nd one. for example, ["cmd","a","c"] => ["acmd", "c"]
                 _  -> scan xs (ms) x [] (d-1) ([(head (drop 1 inter)) ++ (trimBothSpaceSpan (head inter))] ++ (drop 2 inter))  (r) f line 0 (colsrc+1)
          M_UNDERSCR  -> return (Left (err2Msg (ErrTex2Uni (ET2UResSymb "[_}] in ZStirng!"))))
          M_SUPERSCR  -> return (Left (err2Msg (ErrTex2Uni (ET2UResSymb "[^}] in ZStirng!"))))
    '_'         -> case m of
          M_CHAR      ->  scan xs ((M_UNDERSCR):mo) x [] d inter r f line 0 (colsrc+1)
          M_CMDSLASH  ->  case d of
                   0  -> scan xs (ms) x [] d [] (r ++ [(['_'], noInfoSpan (SrcSpan f line colcmd line colsrc))]) f line 0 (colsrc+1)    -- '\_' => '_' 
                   _  -> scan xs (ms) x [] d ([(head inter) ++ [(['_'], noInfoSpan (SrcSpan f line colcmd line colsrc))]] ++ (drop 1 inter)) (r) f line 0 (colsrc+1)
          M_CMD       ->  do 
                              rm <- getFromCmdMap cmd
                              case rm of 
                                  Left e  -> return (Left e) 
                                  -- \cmd_ is regarded as a part of cmd as well, and so the result of \cmd should not be resolved
                                  -- add an extra entry in the head of inter as for \cmd_, such as \cmd_1
                                  Right (r', span) -> do
                                        scan xs (M_UNDERSCR:mo) x [] (d) ([[(r', span')]] ++ (inter)) (r) f line colcmd (colsrc+1)
                                    -- the last srcInfoPoints is to keep the location of \cmd in the input string
                                    where span' = infoSpan (srcInfoSpan span) ((srcInfoPoints span) ++ [(SrcSpan f line colcmd line (colsrc-1))])
          M_INBRACE l -> scan xs ((M_UNDERSCR):mo) x [] d inter r f line 0 (colsrc+1)
          M_UNDERSCR  -> return (Left (err2Msg (ErrTex2Uni (ET2UResSymb "[__] in ZStirng!"))))
          M_SUPERSCR  -> return (Left (err2Msg (ErrTex2Uni (ET2UResSymb "[^_] in ZStirng!"))))
    '^'         -> case m of
          M_CHAR      -> scan xs ((M_SUPERSCR):mo) x [] d inter r f line 0 (colsrc+1)
          M_CMDSLASH  -> return (Left (err2Msg (ErrTex2Uni (ET2UResSymb "[\\^] in ZStirng!"))))
          M_CMD       -> do 
                              rm <- getFromCmdMap cmd
                              case rm of 
                                  Left e  -> return (Left e) 
                                  -- \cmd_ is regarded as a part of cmd as well, and so the result of \cmd should not be resolved
                                  -- add an extra entry in the head of inter as for \cmd_ 
                                  Right (r', span) -> do
                                        scan xs (M_SUPERSCR:mo) x [] (d) ([[(r',span')]] ++ (inter)) (r) f line colcmd (colsrc+1)
                                    -- the last srcInfoPoints is to keep the location of \cmd in the input string
                                    where span' = infoSpan (srcInfoSpan span) ((srcInfoPoints span) ++ [(SrcSpan f line colcmd line (colsrc-1))])
          M_INBRACE l -> scan xs ((M_SUPERSCR):mo) x [] d inter r f line 0 (colsrc+1)
          M_UNDERSCR  ->  return (Left (err2Msg (ErrTex2Uni (ET2UResSymb "[_^] in ZStirng!"))))
          M_SUPERSCR  ->  return (Left (err2Msg (ErrTex2Uni (ET2UResSymb "[^^] in ZStirng!"))))
    '~'         -> case m of
          M_CHAR      -> case d of
                              0  -> scan xs (mo) x [] (d) inter (r ++ [([zSpecialSpacechar], noInfoSpan (SrcSpan f line colsrc line colsrc))]) f line 0 (colsrc+1)
                              _  -> scan xs (mo) x [] (d) ([(head inter) ++ [([zSpecialSpacechar], noInfoSpan (SrcSpan f line colsrc line colsrc))]] ++ (drop 1 inter)) (r)  f line 0 (colsrc+1)
          M_CMDSLASH  -> return (Left (err2Msg (ErrTex2Uni (ET2UResSymb "[\\~] in ZStirng!"))))
          M_CMD       -> do 
                         rm <- getFromCmdMap cmd
                         case rm of 
                             Left e  -> return (Left e)                  -- the command has not found in the map
                             Right (r', span) -> case d of                       -- the command has found in the map
                                 0  -> scan (x:xs) (ms) x [] (d) inter (r ++ [(r', span')] ++ [([zSpecialSpacechar], noInfoSpan (SrcSpan f line colsrc line colsrc))]) f line 0 (colsrc+1)
                                       -- 1st one appended in the 2nd one. for example, ["cmd","a","c"] => ["acmd", "c"]
                                 _  -> scan (x:xs) (ms) x [] (d) ([(head inter) ++ [(r', span')] ++ [([zSpecialSpacechar], noInfoSpan (SrcSpan f line colsrc line colsrc))]] ++ (drop 1 inter)) (r) f line 0 (colsrc+1)
                               -- the last srcInfoPoints is to keep the location of \cmd in the input string
                               where span' = infoSpan (srcInfoSpan span) ((srcInfoPoints span) ++ [(SrcSpan f line colcmd line (colsrc-1))])
          M_INBRACE l -> case d of
                 0  -> return (Left (err2Msg (ErrTex2Uni (ET2UUnbalStruct ("Unbalanced brace '{' and '}' in ZString: depth cannot be zero when it is in the INBRACE mode."))))) 
                 _  -> scan (x:xs) (M_CHAR:mo) x [] (d) ([(head inter) ++ [([zSpecialSpacechar], noInfoSpan (SrcSpan f line colsrc line colsrc))]] ++ (drop 1 inter)) (r) f line 0 (colsrc+1)
          M_UNDERSCR  -> return (Left ("[_~] is not allowed in ZStirng!"))
          M_SUPERSCR  -> return (Left ("[^~] is not allowed in ZStirng!"))
    '\''         -> case m of
          M_CHAR      -> case d of
                              0  -> scan xs (mo) x [] (d) inter (r ++ [(zChar_next, noInfoSpan (SrcSpan f line colsrc line colsrc))]) f line 0 (colsrc+1)
                              _  -> scan xs (mo) x [] (d) ([(head inter) ++ [(zChar_next, noInfoSpan (SrcSpan f line colsrc line colsrc))]] ++ (drop 1 inter)) (r) f line 0 (colsrc+1)
          M_CMDSLASH  -> return (Left ("[\\\'] is not allowed in ZStirng!"))
          M_CMD       -> do 
                         rm <- getFromCmdMap cmd
                         case rm of 
                             Left e  -> return (Left e)                  -- the command has not found in the map
                             Right (r', span) -> case d of                       -- the command has found in the map
                                 0  -> scan (x:xs) (ms) x [] (d) inter (r ++ [(r', span')])  f line 0 (colsrc+1)
                                       -- 1st one appended in the 2nd one. for example, ["cmd","a","c"] => ["acmd", "c"]
                                 _  -> scan (x:xs) (ms) x [] (d) ([(head inter) ++ [(r', span')]] ++ (drop 1 inter)) (r) f line 0 (colsrc+1)
                               -- the last srcInfoPoints is to keep the location of \cmd in the input string
                               where span' = infoSpan (srcInfoSpan span) ((srcInfoPoints span) ++ [(SrcSpan f line colcmd line (colsrc-1))])
          M_INBRACE l -> case d of
                 0  -> return (Left (err2Msg (ErrTex2Uni (ET2UUnbalStruct ("Unbalanced brace '{' and '}' in ZString: depth cannot be zero when it is in the INBRACE mode."))))) 
                 _  -> scan (x:xs) (M_CHAR:mo) x [] (d) ([(head inter) ++ [(zChar_next, noInfoSpan (SrcSpan f line colsrc line colsrc))]] ++ (drop 1 inter)) (r)  f line 0 (colsrc+1)
          M_UNDERSCR  -> return (Left (err2Msg (ErrTex2Uni (ET2UResSymb "[_\'] in ZStirng!"))))
          M_SUPERSCR  -> return (Left (err2Msg (ErrTex2Uni (ET2UResSymb "[^\'] in ZStirng!"))))
    _           -> if (isSpace x)
                      then case m of
                          M_CHAR      ->  scan xs mo x [] d inter r f line 0 (colsrc+1)       -- ignore space
                          M_CMDSLASH  ->  case x of
                                              ' ' -> scan xs (ms) x [] d inter (r ++ [([zSpecialSpacechar], noInfoSpan (SrcSpan f line colsrc line colsrc))])  f line 0 (colsrc+1)
                                                  -- other space chars except ' ' are now allowed after '\\'
                                              _   -> return (Left (err2Msg (ErrTex2Uni (ET2UResSymb "Other spaces (except ' ') after '\\' in ZStirng!"))))
                          M_CMD       ->  do
                              rm <- getFromCmdMap cmd
                              case rm of 
                                  Left e  -> return (Left e)                  -- the command has not found in the map
                                  Right (r',span) -> case d of                       -- the command has found in the map
                                      0  -> case ms of
                                              (M_UNDERSCR:M_CMD:xms)  -> scan xs (xms) x [] d (tail inter) (concatendIsMathSpan r (concatendSpan (head inter) (zSubSpan [(r', span')]))) f line 0 (colsrc+1)
                                              (M_UNDERSCR:xms)        -> scan xs (xms) x [] d (inter) (concatendIsMathSpan r (zSubSpan [(r',span')])) f line 0 (colsrc+1)
                                              (M_SUPERSCR:M_CMD:xms)  -> scan xs (xms) x [] d (tail inter) (concatendIsMathSpan r (concatendSpan (head inter) (zSupSpan [(r',span')]))) f line 0 (colsrc+1)
                                              (M_SUPERSCR:xms)        -> scan xs (xms) x [] d (inter) (concatendIsMathSpan r (zSupSpan [(r',span')])) f line 0 (colsrc+1)
                                              _                       -> scan xs (ms) x [] d inter (r ++ [(r',span')]) f line 0 (colsrc+1)
                                         -- 1st one appended in the 2nd one. for example, ["cmd","a","c"] => ["acmd", "c"]
                                      _  -> case ms of 
                                              (M_UNDERSCR:M_CMD:xms)  -> scan xs (ms) x [] d ([(head (drop 1 inter)) ++ (concatendIsMathSpan (head inter) (zSubSpan [(r',span')]))] ++ (drop 2 inter)) (r) f line 0 (colsrc+1)
                                              (M_UNDERSCR:xms)        -> scan xs (ms) x [] d ([concatendIsMathSpan (head inter) (zSubSpan [(r',span')])] ++ (drop 1 inter)) (r) f line 0 (colsrc+1)
                                              (M_SUPERSCR:M_CMD:xms)  -> scan xs (ms) x [] d ([(head (drop 1 inter)) ++ (concatendIsMathSpan (head inter) (zSupSpan [(r',span')]))] ++ (drop 2 inter)) (r)  f line 0 (colsrc+1)
                                              (M_SUPERSCR:xms)        -> scan xs (ms) x [] d ([concatendIsMathSpan (head inter) (zSupSpan [(r',span')])] ++ (drop 1 inter)) (r) f line 0 (colsrc+1)
                                              _                       -> scan xs (ms) x [] d ([(head inter) ++ [(r',span')]] ++ (drop 1 inter)) (r) f line 0 (colsrc+1)
                                    -- the last srcInfoPoints is to keep the location of \cmd in the input string
                                    where span' = infoSpan (srcInfoSpan span) ((srcInfoPoints span) ++ [(SrcSpan f line colcmd line (colsrc-1))])
                          M_INBRACE l -> scan xs mo x [] d inter r f line 0 (colsrc+1)            -- just ignore it
                          M_UNDERSCR  -> return (Left (err2Msg (ErrTex2Uni (ET2UResSymb "Space after '_' in ZStirng!"))))   -- space is not allowed after _ 
                          M_SUPERSCR  -> return (Left (err2Msg (ErrTex2Uni (ET2UResSymb "Space after '^' in ZStirng!"))))   -- space is not allowed after ^ 
                      else case m of 
                          M_CHAR      -> case d of
                                      0  -> scan xs (mo) x [] (d) inter (concatendIsMathRelSpan r (x,noInfoSpan (SrcSpan f line colsrc line colsrc)))  f line 0 (colsrc+1)
                                      _  -> scan xs (mo) x [] (d) ([concatendIsMathRelSpan (head inter) (x,noInfoSpan (SrcSpan f line colsrc line colsrc))] ++ (drop 1 inter)) (r) f line 0 (colsrc+1)
                          M_CMDSLASH  ->  scan xs (M_CMD:ms) x ([x]) d inter (r) f line colcmd (colsrc+1) 
                          -- letter and digit are regarded as a part of a command, but others won't
                          M_CMD       -> if (isAlpha x) || (isDigit x)
                                          then scan xs (M_CMD:ms) x (cmd ++ [x]) d inter (r) f line colcmd (colsrc+1) 
                                          else do
                                              rm <- getFromCmdMap cmd
                                              case rm of 
                                                  Left e  -> return (Left e)                  -- the command has not found in the map
                                                  Right (r', span) -> case d of                       -- the command has found in the map
                                                      0  -> scan (x:xs) (ms) x [] (d) inter  (r ++ [(r', span')]) f line 0 (colsrc)
                                                            -- 1st one appended in the 2nd one. for example, ["cmd","a","c"] => ["acmd", "c"]
                                                      _  -> scan (x:xs) (ms) x [] (d) ([(head inter) ++ [(r', span')]] ++ (drop 1 inter)) (r) f line 0 (colsrc)
                                                    -- the last srcInfoPoints is to keep the location of \cmd in the input string
                                                    where span' = infoSpan (srcInfoSpan span) ((srcInfoPoints span) ++ [(SrcSpan f line colcmd line (colsrc-1))])
                          M_INBRACE l -> scan xs ((M_CHAR):mo) x [] d ([concatendIsMathRelSpan (head inter) (x,noInfoSpan (SrcSpan f line colsrc line colsrc))] ++ (drop 1 inter)) r f line 0 (colsrc+1)
                          M_UNDERSCR  -> case ms of
                              (M_CMD:mms)     -> case d of
                                      0  -> scan xs (mms) x [] (d) (tail inter) (r ++ (concatendSpan (head inter) (zSubSpan [([x], noInfoSpan (SrcSpan f line colsrc line colsrc))]))) f line 0 (colsrc+1)
                                          -- inter: head (for \cmd), second (for {}), so add _1's corresponding string to head, then append to the second
                                      _  -> scan xs (mms) x [] (d) ([(head (drop 1 inter)) ++ (concatendSpan (head inter) (zSubSpan [([x], noInfoSpan (SrcSpan f line colsrc line colsrc))]))] ++ (drop 2 inter)) (r) f line 0 (colsrc+1)
                              _               -> case d of
                                      0  -> scan xs (ms) x [] (d) inter (concatendIsMathSpan r (zSubSpan [([x], noInfoSpan (SrcSpan f line colsrc line colsrc))])) f line 0 (colsrc+1)
                                      _  -> scan xs (ms) x [] (d) ([concatendIsMathSpan (head inter) (zSubSpan [([x], noInfoSpan (SrcSpan f line colsrc line colsrc))])] ++ (drop 1 inter)) (r) f line 0 (colsrc+1)
                          M_SUPERSCR  -> case ms of
                              (M_CMD:mms)     -> case d of
                                      0  -> scan xs (mms) x [] (d) (tail inter) (r ++ (concatendSpan (head inter) (zSupSpan [([x], noInfoSpan (SrcSpan f line colsrc line colsrc))]))) f line 0 (colsrc+1)
                                          -- inter: head (for \cmd), second (for {}), so add _1's corresponding string to head, then append to the second
                                      _  -> scan xs (mms) x [] (d) ([(head (drop 1 inter)) ++ (concatendSpan (head inter) (zSupSpan [([x], noInfoSpan (SrcSpan f line colsrc line colsrc))]))] ++ (drop 2 inter)) (r) f line 0 (colsrc+1)
                              _               -> case d of
                                      0  -> scan xs (ms) x [] (d) inter (concatendIsMathSpan r (zSupSpan [([x], noInfoSpan (SrcSpan f line colsrc line colsrc))])) f line 0 (colsrc+1)
                                      _  -> scan xs (ms) x [] (d) ([concatendIsMathSpan (head inter) (zSupSpan [([x], noInfoSpan (SrcSpan f line colsrc line colsrc))])] ++ (drop 1 inter)) (r) f line 0 (colsrc+1)
-- 
scan s [] c cmd d inter r f line colcmd colsrc = return (Left (err2Msg (ErrTex2Uni (ET2UInternal "Mode List shall not be empty!"))))
scan s mod c cmd d inter r f line colcmd colsrc = return (Left (err2Msg (ErrTex2Uni (ET2UInternal "Undefined scan!"))))

-- concatend: concat two strings (s1 ++ s2) but if the last char of s1 is space, 
-- then the space is moved to the end of ((init s1) ++ s2 ++ ' ')
concatend :: String -> String -> String
concatend s1 s2 = case last s1 of
                    ' '     -> ((init s1) ++ s2 ++ [' '])
                    _       -> (s1 ++ s2)

-- concatend: concat two strings (s1 ++ s2) but if the last char of s1 is space, 
-- then the space is moved to the end of ((init s1) ++ s2 ++ ' ')
concatendSpan :: [(String, SrcSpanInfo)] -> [(String, SrcSpanInfo)] -> [(String, SrcSpanInfo)]
concatendSpan [] s2 = s2 
concatendSpan s1 s2 = case s1r of
    [' ']   -> ((init s1) ++ s2 ++ [(s1r, span)])
    _       -> case last s1r of
         (' ')     -> ((init s1) ++ [(init s1r, span)] ++ s2 ++ [([' '], noSrcSpan)])
         _         -> (s1 ++ s2)
  where (s1r, span) = last s1

-- like concatend but the last space moved to then end only if the last 2nd char is a math 
-- function, relation or punctuation given in cIsMath.
concatendIsMath :: String -> String -> String
concatendIsMath [] s2 = s2
concatendIsMath [c] s2 = [c] ++ s2
concatendIsMath s1 s2 = case last s1 of 
    ' '     -> if cIsMath (last (init s1)) 
                    then ((init s1) ++ s2 ++ [' '])
                    else (s1 ++ s2)
    _       -> (s1 ++ s2) 

-- like concatend but the last space moved to then end only if the last 2nd char is a math 
-- function, relation or punctuation given in cIsMath.
concatendIsMathSpan :: [(String, SrcSpanInfo)] 
                    -> [(String, SrcSpanInfo)] 
                    -> [(String, SrcSpanInfo)]
concatendIsMathSpan [] s2 = s2 
concatendIsMathSpan [([],span)] s2 = s2 
concatendIsMathSpan [([c],span)] s2 = [([c],span)] ++ s2 
concatendIsMathSpan s1 s2 = case length l1s1 of
    -- the last is only one char and so need to check the last char of the last second
    1       -> case last l1s1 of 
        ' '     -> if cIsMath (last l2s1) 
                    then ((init s1) ++ s2 ++ [(l1s1, span1s1)])
                    else (s1 ++ s2) 
        _       -> (s1 ++ s2)  
    _       -> case last l1s1 of 
        ' '     -> if cIsMath (last (init l1s1)) 
                    then ((init s1) ++ [(init l1s1, span1s1)] ++ s2 ++ [([' '], noInfoSpan (lastSrcLoc span1s1))])
                    else (s1 ++ s2) 
        _       -> (s1 ++ s2)  
  where (l1s1, span1s1) = last s1
        (l2s1, span2s1) = last (init s1)

-- like concatend but the last space moved to then end only if the last 2nd char is a math 
-- function, relation or punctuation given in cIsMath.
concatendIsMathRel :: String -> Char -> String
concatendIsMathRel [] c2 = c2u c2
concatendIsMathRel [c] c2 = [c] ++ (c2u c2)
concatendIsMathRel s1 '=' = case last s1 of 
    ' '     -> case (last (init s1)) of
                '='     -> ((init s1) ++ ['='] ++ [' '])
                ':'     -> ((init s1) ++ ['='] ++ [' '])
                _     -> (s1 ++ (c2u '=')) 
    _       -> (s1 ++ (c2u '='))  
concatendIsMathRel s1 ':' = case last s1 of 
    ' '     -> case (last (init s1)) of
                ':'     -> ((init s1) ++ [':'] ++ [' '])
                _     -> (s1 ++ (c2u ':')) 
    _       -> (s1 ++ (c2u ':'))  
concatendIsMathRel s1 c2 = (s1 ++ (c2u c2))

-- like concatend but the last space moved to then end only if the last 2nd char is a math 
-- function, relation or punctuation given in cIsMath.
concatendIsMathRelSpan :: [(String, SrcSpanInfo)] -> (Char, SrcSpanInfo) -> [(String, SrcSpanInfo)]
concatendIsMathRelSpan [] (c2, span2) = [(c2u c2, span2)]
concatendIsMathRelSpan [([], span1)] (c2, span2) = [(c2u c2, span2)]
concatendIsMathRelSpan [([c], span1)] (c2, span2) = [([c],span1)] ++ [(c2u c2, span2)]
concatendIsMathRelSpan s1 ('=', span2) = case length l1s1 of
    -- the last is only one char and so need to check the last char of the last second
    1       -> case last l1s1 of 
        ' '     -> case (last l2s1) of
                    '='     -> ((init s1) ++ [(['='], span2)] ++ [(l1s1, span1s1)])
                    ':'     -> ((init s1) ++ [(['='], span2)] ++ [(l1s1, span1s1)])
                    _     -> (s1 ++ [(c2u '=', span2)]) 
        _       -> (s1 ++ [(c2u '=', span2)])  
    _       -> case last l1s1 of 
        ' '     -> case (last (init l1s1)) of
                    '='     -> ((init s1) ++ [(init l1s1, span1s1)] ++ [(['='], span2)] ++ [([' '], noInfoSpan (lastSrcLoc span1s1))])
                    ':'     -> ((init s1) ++ [(init l1s1, span1s1)] ++ [(['='], span2)] ++ [([' '], noInfoSpan (lastSrcLoc span1s1))])
                    _       -> (s1 ++ [(c2u '=', span2)]) 
        _       -> (s1 ++ [(c2u '=', span2)])  
  where (l1s1, span1s1) = last s1
        (l2s1, span2s1) = last (init s1)

concatendIsMathRelSpan s1 (':', span2) = case length l1s1 of
    -- the last is only one char and so need to check the last char of the last second
    1       -> case last l1s1 of 
        ' '     -> case (last l2s1) of
                    ':'     -> ((init s1) ++ [([':'], span2)] ++ [(l1s1, span1s1)])
                    _     -> (s1 ++ [(c2u ':', span2)]) 
        _       -> (s1 ++ [(c2u ':', span2)])  
    _       -> case last l1s1 of 
        ' '     -> case (last (init l1s1)) of
                    ':'     -> ((init s1) ++ [(init l1s1, span1s1)] ++ [([':'], span2)] ++ [([' '], noInfoSpan (lastSrcLoc span1s1))])
                    _       -> (s1 ++ [(c2u ':', span2)]) 
        _       -> (s1 ++ [(c2u ':', span2)])  
  where (l1s1, span1s1) = last s1
        (l2s1, span2s1) = last (init s1)
concatendIsMathRelSpan s1 (c2,span2) = (s1 ++ [(c2u c2, span2)])

-- | Get the last position of a SrcSpanInfo and assemble into a new SrcSpan with only one last position
lastSrcLoc :: SrcSpanInfo -> SrcSpan
lastSrcLoc noSrcSpan = srcInfoSpan noSrcSpan 
lastSrcLoc s = SrcSpan f line endcol line endcol 
  where span = srcInfoSpan s 
        f = srcSpanFilename span 
        line = srcSpanStartLine span 
        endcol = srcSpanEndColumn span 

-- | replace '\_' by '_'
replaceLatexUnderline :: String -> String
replaceLatexUnderline [] = []
replaceLatexUnderline [x] = [x]
replaceLatexUnderline ('\\':'_':xs) = '_' : (replaceLatexUnderline xs)  
replaceLatexUnderline (x:xs) = x : (replaceLatexUnderline xs)  
 
zNLchar :: Char
zNLchar = '\x2028'
 
zSpacechar :: Char
zSpacechar = ' '

-- A special narrow no-break space to represent translation from a hard space
-- to distinguish from soft space, finally this space should be replaced by a soft space ' '
-- so it is introduced for internal use only
zSpecialSpacechar :: Char
zSpecialSpacechar = '\x2602'    -- ☂

zSubscript :: String -> String
zSubscript str = (['\x2198'] ++ str ++ ['\x2196'])

zSubSpan :: [(String, SrcSpanInfo)] -> [(String, SrcSpanInfo)]
zSubSpan [] = [((['\x2198'] ++ "" ++ ['\x2196']), noSrcSpan)]
zSubSpan [(str, span)] = [((['\x2198'] ++ str ++ ['\x2196']), span)]
zSubSpan ((str, span):xs) = [(['\x2198'] ++ str, span)] ++ (init xs) ++ [(lstr ++ ['\x2196'], lspan)]
  where (lstr, lspan) = last xs

zSupscript :: String -> String
zSupscript str = (['\x2197'] ++ str ++ ['\x2199'])

zSupSpan :: [(String, SrcSpanInfo)] -> [(String, SrcSpanInfo)]
zSupSpan [] = [((['\x2197'] ++ "" ++ ['\x2199']), noSrcSpan)]
zSupSpan [(str, span)] = [((['\x2197'] ++ str ++ ['\x2199']), span)]
zSupSpan ((str, span):xs) = [(['\x2197'] ++ str, span)] ++ (init xs) ++ [(lstr ++ ['\x2199'], lspan)]
  where (lstr, lspan) = last xs

-- initial commands and zstring map 
initCmdsMap :: [(String, (String, SrcSpanInfo))]
initCmdsMap = [
    ("~" ,          ([zSpecialSpacechar], noSrcSpan)), 
    ("\\," ,        ([zSpecialSpacechar], noSrcSpan)),
    ("\\:" ,        ([zSpecialSpacechar], noSrcSpan)),
    ("\\;" ,        ([zSpecialSpacechar], noSrcSpan)),
    ("\\ " ,        ([zSpecialSpacechar], noSrcSpan)),
    ("\\t1",        ([zSpecialSpacechar], noSrcSpan)),
    ("\\t2",        ([zSpecialSpacechar], noSrcSpan)),
    ("\\t3",        ([zSpecialSpacechar], noSrcSpan)),
    ("\\t4",        ([zSpecialSpacechar], noSrcSpan)),
    ("\\t5",        ([zSpecialSpacechar], noSrcSpan)),
    ("\\t6",        ([zSpecialSpacechar], noSrcSpan)),
    ("\\t7",        ([zSpecialSpacechar], noSrcSpan)),
    ("\\t8",        ([zSpecialSpacechar], noSrcSpan)),
    ("\\t9",        ([zSpecialSpacechar], noSrcSpan)),
    ("\\\\",        ([zNLchar],           noSrcSpan)),
    ("\\also" ,     ([zNLchar],           noSrcSpan)),
    ("\\znewpage" , ([zNLchar],           noSrcSpan)), 
    ("\\SECTION" ,  ("section ",          noSrcSpan)), 
    ("\\parents" ,  (" parents ",         noSrcSpan)),
    (zChar_apostrophe , (zChar_next,      noSrcSpan)) 
    ]

-- convert a char from Latex to Unicode 
c2u :: Char -> String
c2u '@' = [' '] ++ zChar_spot ++ [' ']
c2u '+' = " + " 
--c2u '-' = " - " 
-- U+2212 for the minus operator (A.2.5.4)
c2u '-' = " " ++ zChar_minussign ++ " "
c2u '*' = " * " 
c2u '|' = " | " 
c2u ';' = "; " 
c2u ',' = ", " 
c2u '=' = " = " 
c2u ':' = " : " 
c2u '<' = " < " 
c2u '>' = " > " 
c2u x   = [x]

-- for a char that acts as a math function, punctuation, or relation
cIsMath :: Char -> Bool
cIsMath '@' = True  
cIsMath '+' = True  
cIsMath '-' = True  
cIsMath '*' = True  
cIsMath '|' = True  
cIsMath ';' = True 
cIsMath ',' = True 
cIsMath '=' = True  
cIsMath ':' = True  
cIsMath '<' = True  
cIsMath '>' = True  
cIsMath _   = False

-- The initial command map
--  we keep the translation of Latex commands to Unicode string into this map
cmdMapInit :: Map String (String, SrcSpanInfo)
cmdMapInit = Map.fromList initCmdsMap

-- remove one space from both leading and ending 
trimOneLeadAndEndSpace :: String -> String
trimOneLeadAndEndSpace = reverse . trimOneSpace . reverse . trimOneSpace

-- | remove one space from both leading and ending 
trimOneLeadAndEndSpaceSpan :: [(String, SrcSpanInfo)] 
                           -> [(String, SrcSpanInfo)] 
trimOneLeadAndEndSpaceSpan [] = [] 
trimOneLeadAndEndSpaceSpan [(str, span)] = [(trimOneLeadAndEndSpace str, span)] 
trimOneLeadAndEndSpaceSpan ((str, span):xs) = [(trimOneSpace str, span)] ++ (init xs) ++ [(trimTail lstr, lspan)]
    where (lstr, lspan) = last xs

trimBothSpaceSpan = trimOneLeadAndEndSpaceSpan

-- | trim one leading space
trimOneSpace :: String -> String
trimOneSpace [] = [] 
trimOneSpace (x:xs) 
    | x == zSpacechar = xs
--    | isSpace x       = xs
    | otherwise       = x:xs

-- | remove leading and ending spaces
trimLeadAndEndSpace :: String -> String
trimLeadAndEndSpace = reverse . dropWhile (isSpace) . reverse . dropWhile (isSpace)

trimBothSpace = trimLeadAndEndSpace

-- trim ending spaces
trimTail :: String -> String
trimTail = reverse . dropWhile (isSpace) . reverse

-------------------------------------------------------------------
-- Alex wrapper code.
-------------------------------------------------------------------
alexEOF :: Alex Token
alexEOF = return EOF

data AlexUserState = AlexUserState
                   {
                     -- used by the lexer phase
                       lexerCommentDepth  :: Int,
                       lexerZedDepth :: Int,
                     -- lex states, used for management of "begin code"
                       lex_state  :: [Int],
                     -- latex command map from a command to its replaced char or Zstring  
                     -- for example, ("\Delta", \x0394), ("\sqsubseteq", " \x2291 ")
                       cmdMap :: Map String (String, SrcSpanInfo),
                     -- current file in processing
                       curFile :: String
                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0 0 [] cmdMapInit [] 

-- | get depth of Zed environment 
getLexerZedDepth :: Alex Int
getLexerZedDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerZedDepth ust)

-- | set depth of Zed environment 
setLexerZedDepth :: Int -> Alex ()
setLexerZedDepth ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerZedDepth=ss}}, ())

-- | enter a new Zed environment 
enterNewZed :: Tok String -> AlexInput -> Int -> Alex Token 
enterNewZed c input len =
    do setLexerZedDepth 1
       mkL c input len 

-- | enter an embedded Zed environment, and depth increased by 1
embedZed :: Tok String -> AlexInput -> Int -> Alex Token 
embedZed c input len =
    do cd <- getLexerZedDepth
       setLexerZedDepth (cd + 1)
       mkL c input len 

-- | exit an embedded Zed environment, and depth decreased by 1
unembedZed :: Tok String -> AlexInput -> Int -> Alex Token 
unembedZed c input len =
    do cd <- getLexerZedDepth
       setLexerZedDepth (cd - 1)
       when (cd == 1) (alexSetStartCode state_initial)
       mkL c input len 

-- CommonDepth 
-- @ is as-pattern
-- AlexState{alex_ust=ust}:  
-- lambda function 
getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentDepth ust)

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentDepth=ss}}, ())

enterNewComment input len =
    do setLexerCommentDepth 1
       skip input len

embedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd + 1)
       skip input len

unembedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd - 1)
       when (cd == 1) (alexSetStartCode state_initial)
       skip input len

-- | push a code into lex_state
pushLexState :: Int -> Alex ()
pushLexState ls = Alex $ \s@AlexState{alex_ust=ust} -> Right (s{alex_ust=(alex_ust s){lex_state=ls:(lex_state ust)}}, ())

-- | pop a code from lex_state
popLexState :: Alex Int
popLexState = Alex $ \s@AlexState{alex_ust=(AlexUserState {lex_state = ls:l})} -> Right (s{alex_ust=(alex_ust s){lex_state=l}}, ls)

-- | get a last code from lex_state
getLexState :: Alex Int
getLexState = Alex $ \s@AlexState{alex_ust=(AlexUserState {lex_state = ls:l})} -> Right(s, ls)

-- cmdMap
getCmdMap :: Alex (Map String (String, SrcSpanInfo))
getCmdMap = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, cmdMap ust)

setCmdMap :: (Map String (String, SrcSpanInfo)) -> Alex ()
setCmdMap ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){cmdMap=ss}}, ())

state_initial :: Int
state_initial = 0

-- curFile
getCurFile :: Alex String
getCurFile = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, curFile ust)

setCurFile :: (String) -> Alex ()
setCurFile ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){curFile=ss}}, ())

-- common
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
errMsg s (_,_,_,input) len = lexerError ("Expect: [" ++ s ++ "]. But it is [" ++ (take len input) ++ "].")

-- postscan: a final step to cope with some special treatments temporary
--  1. use zSpecialSpacechar for hard spaces and now need to convert to hard space
postscan :: String -> String
postscan [] = [] 
postscan (x : xs) = if x == zSpecialSpacechar 
                    then (zSpacechar : (postscan xs))
                    else (x : (postscan xs))

postscanSpan :: [(String, (SrcSpanInfo))] -> [(String, (SrcSpanInfo))]
postscanSpan [] = []
postscanSpan ((str,span):xs) = (postscan str, span) : (postscanSpan xs)

postset :: Token -> Token
postset tok = case tok of  
                  PT1 file pn (T_UNICODE u) (Just o) (c) (loc)  ->  (PT1 file pn (T_UNICODE u) (Just o) (postscan c) (postscanSpan loc))
                  PT pn (T_UNICODE u) (Just o) (c) (loc)        ->  (PT pn (T_UNICODE u) (Just o) (postscan c) (postscanSpan loc))
                  _                                             ->  tok

scanner :: String                                   -- filename to be scanned
    -> Map String (String, SrcSpanInfo)             -- an initial map between commands to their definitions
    -> String                                       -- the contents of the file to be scanned 
    -> Either String ([Token], Map String (String, SrcSpanInfo))   -- either error or a pair of scanned tokens and the new map
scanner file map str = 
  let loop = do
        tok <- alexMonadScan
        if tok == EOF
          then return []
          else do 
                toks <- loop
                return ((postset tok):toks)
      preset = do
        setCurFile file 
        setCmdMap map 
      afterLoop = do
        preset
        toks <- loop
        m <- getCmdMap
        return (toks, m)
  in runAlex str (afterLoop)
--  in runAlex str (setCurFile file >> loop)

addFileNameTokens :: String -> [Token] -> [Token]
addFileNameTokens f [] = []
addFileNameTokens f (x:xs) = case x of
   PT pos tok m s l -> (PT1 f pos tok m s l) : (addFileNameTokens f xs)
   Err pos         -> (Err1 f pos) : (addFileNameTokens f xs)
   _               -> []  

-- load a latex file 
loadLaTexFile :: String                 -- latex file name
        -> Map String (String, SrcSpanInfo) -- an initial map between commands to their definitions
        -> IO (Either String ([Token], Map String (String, SrcSpanInfo)))
loadLaTexFile file map =  do 
            h <- openFile file ReadMode
            content <- hGetContents h
            let result = scanner file map (content)
            case result of
                Left err -> return result 
                Right (toks, m) -> return (Right (addFileNameTokens file toks, m)) 

-- | print the lexed tokens
printResult :: Either String [Token] -> IO ()
printResult (Left str) = putStrLn ("Error: " ++ str)
printResult (Right toks) = do
    putStrLn "-------------------------------- Start ----------------------------------" 
    pprt toks 
    where pprt [] = putStrLn "-------------------------------- End ----------------------------------" 
          pprt (x:xs) = do 
                         print x
                         pprt xs

{- | 
    a lexer to convert a latex specification into a list of tokens 
-}
tex2Unicode :: String -- ^ path to input latex file
    -> IO (Either String [Token]) -- ^ a list of tokens 
tex2Unicode file = do
    ret <- secSolve file 
    case ret of
        Left err -> return (Left err)
        Right secs -> loop secs cmdMapInit [] 
  where loop [] map toks = return (Right toks)
        loop (x:xs) map toks = do
                ret <- loadLaTexFile x map 
                case ret of
                    Left err -> return (Left err) 
                    Right (pre_toks, map1) -> loop xs map1 (toks ++ pre_toks)
                
outputUnicode2Handle :: [Token] -> Handle -> IO ()
outputUnicode2Handle [] outh = do
    hClose outh
outputUnicode2Handle s@(x:xs) outh = case x of
    PT1 file pn (T_UNICODE u) (Just o) (c) _ -> do
                    hPutStr outh c 
                    outputUnicode2Handle xs outh
    _                                       -> do   
                    outputUnicode2Handle xs outh

-- | output a list of unicode tokens into a file
outputUnicode2File :: [Token]   -- ^ a list of tokens
    -> String                   -- ^ output file name 
    -> IO ()
outputUnicode2File toks outfile = do
    outh <- openFile outfile WriteMode
    outputUnicode2Handle toks outh

-- | extract Unicode strings from tokens and combine them into a line break separated string
tokensToUnicodeString :: [Token] -> String
tokensToUnicodeString [] = [] 
tokensToUnicodeString s@(x:xs) = case x of
    PT1 file pn (T_UNICODE u) (Just o) (c) _ -> do
                    (c ++ (tokensToUnicodeString xs))
    _                                       -> do   
                    (tokensToUnicodeString xs)

-- | extract Unicode strings from tokens and combine them into a line break separated string
-- with location information mapping from target unicode string to source latex file
tokensToUnicodeStringSpan :: [Token] -- ^ lexed tokens
    -> Int  -- ^ start line. In the beginning, it should start from line=1, column=1
    -> Int  -- ^ start column 
    -> (String, [(SrcSpan, SrcSpanInfo)])   -- ^ returned pair from output unicode string to location information (map from output unicode string to original latex file)
tokensToUnicodeStringSpan [] line col = ([], [])
tokensToUnicodeStringSpan s@(x:xs) line col = case x of
    PT1 file pn (T_UNICODE u) (Just o) (c) span -> 
        if (null c) 
        then (tokensToUnicodeStringSpan xs line col)
        else case last c of
            '\n'    -> let (xsstr, xsspan) = tokensToUnicodeStringSpan xs (line+1) 1 
                           (xstr, xspan) = updateLoc span line col
                       in (xstr ++ xsstr, xspan ++ xsspan)
            _       -> let (xsstr, xsspan) = tokensToUnicodeStringSpan xs (line) (col + (length c))
                           (xstr, xspan) = updateLoc span line col
                       in (xstr ++ xsstr, xspan ++ xsspan)
    _                                       -> do   
                    (tokensToUnicodeStringSpan xs line col)

-- | convert a list of map from unicode string to source latex location, 
-- to a pair from final unicode string and mapping from target string location to source latex string location
updateLoc :: [(String, SrcSpanInfo)] -- ^ location information of PT tokens
    -> Int  -- ^ start line 
    -> Int  -- ^ start column 
    -> (String, [(SrcSpan, SrcSpanInfo)])   -- ^  returned pair from output unicode string to location information (map from output unicode string to original latex file)
updateLoc [] line col = ("", [])
updateLoc ((x, span):xs) line col = (x ++ xstr, [((SrcSpan "" line col line (col+(length x) - 1)), span)] ++ xspan)
  where (xstr, xspan) = updateLoc xs line (col + (length x))

-- | search the location (line, column) from input location information, and return found source location of latex file
locToSourceSpan :: [(SrcSpan, SrcSpanInfo)] -- ^ location map from output unicode string to source latex file 
    -> Int  -- ^ line number in unicode string 
    -> Int  -- ^ column number in unicode string 
    -> Either String SrcSpanInfo -- ^ return source location information
locToSourceSpan [] line col = Left ("Either input location information is empty, or the line and column (" ++ (show line) ++ ", " ++ (show col) ++ ") given are out of range.")
locToSourceSpan ((SrcSpan _ stline stcol endline endcol, infospan):xs) line col
    | stline == line && col >= stcol && col <= endcol   = Right infospan  
    | otherwise                                         = locToSourceSpan xs line col 
}
