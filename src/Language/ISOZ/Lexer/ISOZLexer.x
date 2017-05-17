-- -*- haskell -*-
{
{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE GADTs #-}

{- |
Module      : Language.ISOZ.Lexer.ISOZLexer
Description : 
Copyright   : (C) Kangfeng Ye, 2017
License     : BSD3
Maintainer  : ye.randall@gmail.com 
Stability   : experimental

Lexis (clause 6 and clause 7).

-}

module Language.ISOZ.Lexer.ISOZLexer  where

import Language.ISOZ.ZChar 
import Language.Circus.CircusChar 
import Language.ISOZ.Common.Error
import Control.Monad
import qualified Data.Bits
import Data.Word (Word8)
import Data.List (sort)
import Data.Char (chr, ord)
import System.IO
import System.Environment (getArgs)
import Data.Map (Map)
import qualified Data.Map as Map
}

%wrapper "monadUserState"

$DECIMAL    = [0-9]         -- decimal 
$DIGIT      = [$DECIMAL]    -- digit
$LATIN      = [a-zA-Z]      -- latin
-----------------------------------------
$DELTA      = \x0394        -- delta
$XI         = \x039E        -- xi 
$THETA      = \x03B8        -- theta
$LAMBDA     = \x03BB        -- lambda 
$MU         = \x03BC        -- mu 
-- GREEK 
$GREEK      = [$DELTA $XI $THETA $LAMBDA $MU]   -- greek 
-----------------------------------------
$ARITHMOS   = \x01D538      -- arithmos
$NAT        = \x2115        -- nat 
$POWER      = \x2119        -- power 
$OTHERLETTER = [$ARITHMOS $NAT $POWER]   -- otherletter
-----------------------------------------
-- set_toolkit
$LEFTRIGHTARROW     = \x2194    -- <->
$RIGHTWARDSARROW    = \x2192    -- ->
$NOTEQUALTO         = \x2260    -- not equal to
$NOTELEMENT         = \x2209    -- not an element of
$EMPTYSET           = \x2205    -- empty set 
$SUBSETEQ           = \x2286    -- subset of or equal to 
$SUBSET             = \x2282    -- subset of 
$UNION              = \x222A    -- union 
$INTER              = \x2229    -- intersection 
$REVSOLIDUS         = \x005C    -- reverse solidus 
-- alternative char for \setminus from CZT
$SETMINUS           = \x2216    -- setminus 
$CIRCMINUS          = \x2296    -- circled minus 
$NARYUNION          = \x22C3    -- N-ary union 
$NARYINTER          = \x22C2    -- N-ary intersection 
$DOUBLEF            = \x1D53D   -- mathematical double-struck capital F 
--
$SETTOOLKIT = [ $LEFTRIGHTARROW $RIGHTWARDSARROW $NOTEQUALTO     
    $NOTELEMENT     $EMPTYSET       $SUBSETEQ       $SUBSET         
    $UNION          $INTER          $REVSOLIDUS     $CIRCMINUS      
    $NARYUNION      $NARYINTER      $DOUBLEF        $SETMINUS]
-----------------------------------------
-- relation_toolkit
$RIGHTWARDSARROWBAR = \x21A6    -- rightwards arrow from bar 
$RELCOMPOSE         = \x2A3E    -- relational composition 
$RINGOP             = \x2218    -- ring operator 
$LEFTTRIANGLE       = \x25C1    -- white left-pointing triangle 
$RIGHTTRIANGLE      = \x25B7    -- white right-pointing triangle 
$DOMANTIREST        = \x2A64    -- domain anti-restriction 
$RANANTIREST        = \x2A65    -- range anti-restriction 
$TILDEOP            = \x223C    -- tilde operator 
$LEFTIMAGE          = \x2987    -- left image bracket 
$RIGHTIMAGE         = \x2988    -- right image bracket 
$CIRCPLUS           = \x2295    -- circled plus 
--
$RELTOOLKIT = [ $RIGHTWARDSARROWBAR $RELCOMPOSE $RINGOP            
    $LEFTTRIANGLE      $RIGHTTRIANGLE     $DOMANTIREST       $RANANTIREST       
    $TILDEOP           $LEFTIMAGE         $RIGHTIMAGE        $CIRCPLUS          
    ]
-----------------------------------------
-- function_toolkit
$RARROWVER          = \x21F8    -- rightwards arrow with vertical stroke
$RARROWTAILVER      = \x2914    -- rightwards arrow with tail with vertical stroke
$RARROWTAIL         = \x21A3    -- rightwards arrow with tail
$RTWOARROWVER       = \x2900    -- rightwards two-headed arrow with vertical stroke 
$RTWOARROW          = \x21A0    -- rightwards two-headed arrow 
$RTWOARROWTAIL      = \x2916    -- rightwards two-headed arrow with tail
$RARROWDVER         = \x21FB    -- rightwards arrow with double vertical stroke 
$RARROWTAILDVER     = \x2915    -- rightwards arrow with tail with double vertical stroke 
-- 
$FUNCTOOLKIT = [ $RARROWVER     $RARROWTAILVER $RARROWTAIL    
    $RTWOARROWVER  $RTWOARROW     $RTWOARROWTAIL 
    $RARROWDVER    $RARROWTAILDVER
    ]

-----------------------------------------
-- number_toolkit
$DOUBLEZ            = \x2124    -- double-struck capital Z
$HYPMINUS           = \x002D    -- hyphen-minus 
$MINUSSIGN          = \x2212    -- minus sign
$LESSEQ             = \x2264    -- less than or equal to 
$LESS               = \x003C    -- less than 
$GRTEQ              = \x2265    -- greater than or equal to 
$GREAT              = \x003E    -- greater than 
$START              = \x002A    -- asterisk 
--
$NUMBERTOOLKIT  = [ $DOUBLEZ  $HYPMINUS 
    $MINUSSIGN      $LESSEQ         $LESS     
    $GRTEQ    $GREAT    $START    
    ]
-----------------------------------------
-- sequence_toolkit
$NUMSIGN            = \x0023    -- number sign
$LEFTANGLE          = \x27E8    -- left angle bracket 
$RIGHTANGLE         = \x27E9    -- right angle bracket 
$CHARTIE            = \x2040    -- character tie 
$UPBARBLEFT         = \x21BF    -- upwards harpoon with barb leftwards 
$UPBARBRIGHT        = \x21BE    -- upwards harpoon with barb rightwards 
--
$SEQTOOLKIT         = [ $NUMSIGN    $LEFTANGLE  
    $RIGHTANGLE $CHARTIE    $UPBARBLEFT $UPBARBRIGHT
    ]

-----------------------------------------
$MATHLETTER = [$SETTOOLKIT $RELTOOLKIT $FUNCTOOLKIT
    $NUMBERTOOLKIT $SEQTOOLKIT]
-----------------------------------------
--$LETTER     = [$LATIN $GREEK $OTHERLETTER $MATHLETTER] -- LETTER
$LETTER     = [$LATIN $GREEK $OTHERLETTER] -- LETTER

-----------------------------------------
$NEXT       = \x2032        -- next ʹ
$IN         = [\?]          -- in 
$OUT        = [!]           -- out
$STROKECHAR = [$NEXT $IN $OUT] -- stroke 
-----------------------------------------
$WORDGLUENE = \x2197        -- north east 
$WORDGLUESW = \x2199        -- south west 
$WORDGLUESE = \x2198        -- south east 
$WORDGLUENW = \x2196        -- north west 
$LOWLINE    = [_]           -- low line
$WORDGLUE   = [$WORDGLUENE $WORDGLUESW $WORDGLUESE $WORDGLUENW $LOWLINE] -- wordglue
-----------------------------------------
$LPARENT    = [\(]          -- (
$RPARENT    = [\)]          -- )
$LBRACKET   = [\[]
$RBRACKET   = [\]]
$LBRACE     = [\{]
$RBRACE     = [\}]
$LBLOT      = \x2989
$RBLOT      = \x298A
$LDATA      = \x27EA
$RDATA      = \x27EB
$LCHANSET   = \x2983
$RCHANSET   = \x2984
$LCIRCINDEX = \x230A
$RCIRCINDEX = \x230B
$LCIRCGUARD = \x3014
$RCIRCGUARD = \x3015
$LCIRCRENAME = \x2768
$RCIRCRENAME = \x2769
$LPAR       = \x27E6
$RPAR       = \x27E7
$LINTER     = \x301A
$RINTER     = \x301B
-- $BRACKET    = [\(\)\[\]\{\}\x2989 \x298A \x300A \x300B] 
$BRACKET    = [$LPARENT $RPARENT $LBRACKET $RBRACKET $LBRACE 
    $RBRACE $LBLOT $RBLOT $LDATA $RDATA
    $LCHANSET $RCHANSET $LCIRCINDEX $RCIRCINDEX
    $LCIRCGUARD $RCIRCGUARD $LCIRCRENAME $RCIRCRENAME
    $LPAR $RPAR $LINTER $RINTER 
    ]
-----------------------------------------
$ZEDCHAR    = \x2500        -- zedchar
$AXCHAR     = \x2577        -- axchar 
$SCHCHAR    = \x250C        -- schchar 
$CONJCHAR   = \x250D        -- conjchar 
$GENCHAR    = \x2550        -- genchar 
$ENDCHAR    = \x2029        -- enchar 
$BOXCHAR    = [$ZEDCHAR $AXCHAR $SCHCHAR $CONJCHAR $GENCHAR $ENDCHAR] -- boxchar
-----------------------------------------
$NLCHAR     = [\x2028]      -- newline
$SPACE      = [\x20]        -- space
$SEMICOLON  = [\;]
-----------------------------------------
$SPECIAL    = [$STROKECHAR $WORDGLUE $BRACKET $BOXCHAR $NLCHAR $SPACE] -- special
-----------------------------------------
-- Reference from CZT (ContextFreeScanner.xml)
$PUNCT      = [ \x002C -- comma 
                \x002E -- full stop 
                \x003A -- colon 
                \x003B -- semicolon 
            ]
$EQUALSIGN = [\x003D]
$CORESYMBOL = [
                \|
                & 
                \x22A2      -- Right track (vdash)
                \x2227      -- land
                \x2228      -- lor
                \x21D2      -- implies 
                \x21D4      -- iff 
                \x00AC      -- lnot
                \x2200      -- forall 
                \x2203      -- exists 
                \x00D7      -- cross 
                \x002F      -- solidus 
                \x003D      -- equal
                \x2208      -- in 
--                \x003A      -- colon :
--                \x003B      -- semicolon ;
--                \x002C      -- comma ,
--                \x002E      -- full stop .
                \x2981      -- spot @
                \x29F9      -- hide
                \x2A21      -- schema project
                \x2A1F      -- schema composition
                \x2A20      -- schema piping
                \x002B      -- +  
            ]   -- special
-- $SYMBOL = [$SYMBOL1 $MATHLETTER]
-- SYMBOL is the character that is not a NOT_SYMBOL 
$NOT_SYMBOL = [$DIGIT $LETTER $SPECIAL $PUNCT]
-- $SYMBOL = [^$NOT_SYMBOL]
$SYMBOL = [$CORESYMBOL $MATHLETTER]
-----------------------------------------
$ZCHAR    = [$DIGIT $LETTER $SPECIAL $SYMBOL ] -- zchar 

-- regular expression macros
@STROKE = $STROKECHAR | $WORDGLUESE $DECIMAL $WORDGLUENW
@NUMERAL = $DECIMAL+ 
@ALPHASTR = ($LETTER | $DIGIT)*
@SYMBOLSTR = ($SYMBOL)*
@WORDPART = $WORDGLUE (@ALPHASTR | @SYMBOLSTR)
@DECORWORD = (@WORDPART+ | 
    (($LETTER | ($DIGIT # $DECIMAL)) @ALPHASTR @WORDPART*) |
    ($SYMBOL @SYMBOLSTR @WORDPART*) |
    ($PUNCT+ $EQUALSIGN?)
    ) @STROKE*
@ZED = $ZEDCHAR
@END = $ENDCHAR
@SCH = $SCHCHAR
@CONJ = $CONJCHAR
@AX = $AXCHAR
@GENAX = $AXCHAR $GENCHAR
@GENSCH = $SCHCHAR $GENCHAR
@GENCONJ = $CONJCHAR $GENCHAR
@NL = $NLCHAR

@KEYWORDS = ( 
    "else" | "false" | "function" | "generic" | "if" | "leftassoc" | 
    "let" | [chr 0x2119] | "parents" | "pre" | "relation" | "rightassoc" | 
    "section" | "then" | "true" | ":" | "==" | "," | "::=" | "|" | "&" | 
    ['\x29F9'] | ['\x002F'] | 
    "." | ";" | "_" | ",," | "=" |  -- symbolic keywords
    ['\x22A2'] '?' | 
    ['\x2200'] | ['\x2981'] | ['\x2203'] | 
    ['\x2203'] ['\x2198'] "1" ['\x2196'] |
    ['\x21D4'] | ['\x21D2'] | ['\x2227'] | 
    ['\x2228'] | ['\x00AC'] | ['\x2208'] | 
    ['\x2A21'] | ['\x00D7'] | ['\x03BB'] | 
    ['\x03BC'] | ['\x03B8'] | ['\x2A1F'] | 
    ['\x2A20']
    )

@relation = "relation"
@function = "function"
@generic = "generic"
@Prec = $DECIMAL+
@Assoc = ("leftassoc" | "rightassoc")
@ES = ($LOWLINE $SPACE* @DECORWORD)
@SS = (",," $SPACE* @DECORWORD)
@ERE = ($LOWLINE $SPACE* @DECORWORD)
@SRE = (",," $SPACE* @DECORWORD)
@ESS_ESRE = ($SPACE* ($LOWLINE | ",,") $SPACE+ @DECORWORD $SPACE*)
@PrefixName = (@DECORWORD @ESS_ESRE* $SPACE* $LOWLINE)
@PostfixName = ($LOWLINE $SPACE* @DECORWORD @ESS_ESRE*)
@InfixName = ($LOWLINE $SPACE* @DECORWORD @ESS_ESRE* $SPACE* $LOWLINE)
@NofixName = (@DECORWORD $SPACE+ @ESS_ESRE*)
--@PrefixTemplate = $LPARENT (@PrefixName | $POWER $LOWLINE) $RPARENT
--@PostfixTemplate = $LPARENT @PostfixName $RPARENT
--@InfixTemplate = $LPARENT @InfixName $RPARENT
--@NofixTemplate = $LPARENT @NofixName $RPARENT
@PrefixTemplate = $SPACE* (@PrefixName | $POWER $SPACE* $LOWLINE) $SPACE* $RPARENT $SPACE* 
@PostfixTemplate = $SPACE* @PostfixName $SPACE* $RPARENT $SPACE* 
@InfixTemplate = $SPACE* @InfixName $SPACE* $RPARENT $SPACE* 
@NofixTemplate = $SPACE* @NofixName $SPACE* $RPARENT $SPACE* 
@NLS = ($SPACE* @NL $SPACE*)+ 
@PrecAssocInfix = $SPACE* @Prec $SPACE* @Assoc $SPACE* $LPARENT $SPACE* @InfixName $SPACE* $RPARENT $SPACE*

-- match DeclName (DeclName)+ :
--  DeclName
--      1. NAME
--          such as: x,y,z:T
--      2. OpName 
--          2.1 PrefixName 
--          2.2 PostfixName 
--          2.3 InfixName 
--          such as: _ div _, _ mod _:T
--          2.4 NofixName 

-- simple one: only NAME
-- @LISTNAMEDECL = ($SPACE* @DECORWORD $SPACE*) ($SPACE* "," $SPACE* @DECORWORD $SPACE*)+ ":" 
-- [_] NAME [_]
@DeclName0 = (($LOWLINE $SPACE*)? @DECORWORD ($SPACE* $LOWLINE)?)
-- [_] NAME [(_ | ,,) NAME]* [_]
-- there are at least one space around "_" or ",,", otherwise they may be regarded as a part of a DECORWORD
@DeclName1 = (($LOWLINE $SPACE*)? @DECORWORD ($SPACE+ ($LOWLINE | ",,") $SPACE+ @DECORWORD)* ($SPACE+ $LOWLINE)?)
-- A list of declartion like "x,y:"
@LISTNAMEDECL = ($SPACE* @DeclName1 $SPACE*) ($SPACE* "," $SPACE* @DeclName1 $SPACE*)+ ":" 
-- A list of rename pair like "x0/x1 , y0/y1"
@LISTRENAMEPAIR = ($SPACE* @DECORWORD $SPACE* [\/] $SPACE* @DECORWORD $SPACE*) ("," $SPACE* @DECORWORD $SPACE* [\/] $SPACE* @DECORWORD $SPACE*)*
--@LISTNAMEDECL = ($SPACE* @DeclName0 $SPACE*) ($SPACE* "," $SPACE* @DeclName0 $SPACE*)+ ":" 

-- match ZED section
@SECTION = ($SPACE* "section" $SPACE*)
--@CONJECTURE = (([^\x22A2][^\?])* \x22A2 \?)
@CONJECTURE = (([^\x22A2\?])* \x22A2 \?)

ISOZ :-
-- skip [\ \t\n\f\v\r] 
-- $white+ ;
[\n]+   ;
$SPACE+ ;
<0> {
   @ZED/@SECTION        { (enterNewZed T_ZED_SEC) `andBegin` body }
   @ZED/@CONJECTURE     { \_ _ -> lexerError (err2Msg (ErrUniLexer (EUniLexer ("Please use theorem environment instead ZED for conjecture")))) }
   @ZED                 { (enterNewZed T_ZED) `andBegin` body }
   @AX/@LISTNAMEDECL    { (enterNewZed T_AX2) `andBegin` body }
   @AX                  { (enterNewZed T_AX) `andBegin` body }
   @SCH                 { (enterNewZed T_SCH) `andBegin` body }
   @CONJ                { (enterNewZed T_CONJ) `andBegin` body }
   @GENAX               { (enterNewZed T_GENAX) `andBegin` body }
   @GENSCH              { (enterNewZed T_GENSCH) `andBegin` body }
   @GENCONJ             { (enterNewZed T_GENCONJ) `andBegin` body }
   @END                 { \_ _ -> lexerError (err2Msg (ErrUniLexer (EUniLexer ("ENDCHAR occurs only after box characters [ZEDCHAR, AXCHAR, SCHCHAR, GENCHAR, CONJCHAR].")))) }
   @NLS                 ; 
-- comments
   ^ $SPACE* "--" .* $  ; 
}

<body> {
   $LPARENT             { mkL T_LPARENT }
   $RPARENT             { mkL T_RPARENT }
   $LBRACKET/@LISTNAMEDECL { mkL T_LBRACKET2 }
   $LBRACKET/@LISTRENAMEPAIR { mkL T_LBRACKET3 }
   $LBRACKET            { mkL T_LBRACKET }
   $RBRACKET/@LISTNAMEDECL  { mkL T_RBRACKET2 }
   $RBRACKET            { mkL T_RBRACKET }
   $LBRACE/@LISTNAMEDECL  { mkL T_LBRACE2 }
   $LBRACE              { mkL T_LBRACE }
   $RBRACE              { mkL T_RBRACE }
   $LBLOT               { mkL T_LBLOT }
   $RBLOT               { mkL T_RBLOT }
   $LDATA               { mkL T_LDATA }
   $RDATA               { mkL T_RDATA }
   $LCHANSET            { mkL T_LCHANSET } 
   $RCHANSET            { mkL T_RCHANSET }
   $LCIRCINDEX          { mkL T_LCIRCINDEX }
   $RCIRCINDEX          { mkL T_RCIRCINDEX }
   $LCIRCGUARD          { mkL T_LCIRCGUARD }
   $RCIRCGUARD          { mkL T_RCIRCGUARD }
   $LCIRCRENAME         { mkL T_LCIRCRENAME }
   $RCIRCRENAME         { mkL T_RCIRCRENAME }
   $LPAR                { mkL T_LPAR }
   $RPAR                { mkL T_RPAR }
   $LINTER              { mkL T_LINTER }
   $RINTER              { mkL T_RINTER }
   $DECIMAL+            { mkL T_NUMERAL }
   @STROKE              { mkL T_STROKE }
   @ZED                 { embedZed T_ZED }
   @AX                  { embedZed T_AX }
   @SCH                 { embedZed T_SCH }
   @GENAX               { embedZed T_GENAX }
   @GENSCH              { embedZed T_GENSCH }
   @END                 { unembedZed T_END }
   $NLCHAR /@LISTNAMEDECL    { mkL T_NL2 }
   $SEMICOLON /@LISTNAMEDECL { mkKeyword2 }
--   $NLCHAR            { mkL T_NL }
   @NLS                 { mkL T_NL } 
--
   @relation            { enterOpRelationMode `andBegin` optemp }
   @function/@PrecAssocInfix  { enterOpFunctionMode `andBegin` optempprec }
   @function            { enterOpFunctionMode `andBegin` optemp }
   @generic/@PrecAssocInfix   { enterOpGenericMode `andBegin` optempprec }
   @generic             { enterOpGenericMode `andBegin` optemp }
-- 
   @DECORWORD/@LISTNAMEDECL  { mkKeyword2 }
   @DECORWORD           { mkKeyword }
}

<optempprec> {
    @Prec                       { mkL T_NUMERAL `andBegin` optempassoc }
}

<optempassoc> {
    @Assoc                      { mkKeyword `andBegin` optemp }
}

<optemp> {
    $LPARENT/@PrefixTemplate    { mkL T_LPARENT `andBegin` relationPre }
    $LPARENT/@PostfixTemplate   { mkL T_LPARENT `andBegin` relationPost }
    $LPARENT/@InfixTemplate     { mkL T_LPARENT `andBegin` relationIn }
    $LPARENT/@NofixTemplate     { mkL T_LPARENT `andBegin` relationNo }
    [^$LPARENT]*                { errMsg "(" }
}

<relationPre> {
   @DECORWORD/$SPACE* $LOWLINE $SPACE* $RPARENT { enterPrefixName2 }
   -- L,{_,ES|,,SS},(_,ERE|,,SRE),_ 
   @DECORWORD/$SPACE* ($SPACE* (@ES | @SS) $SPACE*)* $SPACE* (@ERE | @SRE) $SPACE* $LOWLINE $SPACE* $RPARENT  { enterPrefixName }
   $LOWLINE                             { mkKeyword }
   ",,"                                 { mkKeyword }
   $RPARENT                             { mkL T_RPARENT `andBegin` body }
}

<relationPost> {
   @DECORWORD/$SPACE* $RPARENT          { enterPostfixName2 }
   -- L,{_,ES|,,SS},(_,ERE|,,SRE),_ 
   @DECORWORD/$SPACE* ($SPACE* (@ES | @SS) $SPACE*)* $SPACE* (@ERE | @SRE) $SPACE* $RPARENT  { enterPostfixName }
   $LOWLINE                             { mkKeyword }
   ",,"                                 { mkKeyword }
   $RPARENT                             { mkL T_RPARENT `andBegin` body }
}

<relationIn> {
   @DECORWORD/$SPACE* $LOWLINE $SPACE* $RPARENT { enterInfixName2 }
   -- EL,{_,ES|,,SS},(_,ERE|,,SRE),_ 
   @DECORWORD/$SPACE* ($SPACE* (@ES | @SS) $SPACE*)* $SPACE* (@ERE | @SRE) $SPACE* $LOWLINE $SPACE* $RPARENT  { enterInfixName }
   $LOWLINE                             { mkKeyword }
   ",,"                                 { mkKeyword }
   $RPARENT                             { mkL T_RPARENT `andBegin` body }
}

<relationNo> {
   @DECORWORD/$SPACE* $RPARENT { enterNofixName2 }
   -- L,{_,ES|,,SS},(_,ER|,,SR) 
   @DECORWORD/$SPACE* ($SPACE* (@ES | @SS) $SPACE*)* $SPACE* (@ERE | @SRE) $SPACE* $RPARENT  { enterNofixName }
   $LOWLINE                             { mkKeyword }
   ",,"                                 { mkKeyword }
   $RPARENT                             { mkL T_RPARENT `andBegin` body }
}

{
-- | token types
data Tok a =
   T_NIL        -- ^ Nil
 | T_LPARENT    -- ^ (
 | T_RPARENT    -- ^ ) 
 | T_LBRACKET   -- ^ [ 
 | T_LBRACKET2  -- ^ [ before a list of declaration, such as "[x : T, y: Ty"
 | T_LBRACKET3  -- ^ [ before a list of rename pair, such as "[x/x1, y/y1"
 | T_RBRACKET   -- ^ ]
 | T_RBRACKET2  -- ^ ] before a list of declaration, such as "]x : T, y: Ty"
 | T_LBRACE     -- ^ '{' 
 | T_LBRACE2    -- ^ '{' before a list of declaration, such as "{x : T, y: Ty"
 | T_RBRACE     -- ^ '}' 
 | T_LBLOT      -- ^ ⟨|  
 | T_RBLOT      -- ^ |⟩  
 | T_LDATA      -- ^ ⟨⟨  
 | T_RDATA      -- ^ ⟩⟩  
 | T_LCHANSET
 | T_RCHANSET
 | T_LCIRCINDEX
 | T_RCIRCINDEX
 | T_LCIRCGUARD
 | T_RCIRCGUARD
 | T_LCIRCRENAME
 | T_RCIRCRENAME
 | T_LPAR
 | T_RPAR
 | T_LINTER
 | T_RINTER
 | T_NUMERAL    -- ^  
 | T_STROKE     -- ^ Strokes: ’′’| ’!’| ’?’ | _{n}
 | T_ZED        -- ^  
 | T_ZED_SEC    -- ^ specific for ZED before "sections"
 | T_AX 
 | T_AX2
 | T_SCH 
 | T_CONJ
 | T_GENAX 
 | T_GENSCH 
 | T_GENCONJ
 | T_END 
 | T_NL 
 | T_NL2 
 | T_WORD 
 | T_DECORWORD 
 | T_KEYWORD !a !Int
 | T_KEYWORD2 !a !Int   -- ^ add T_START_SCHEMATEXT before it
-- operator and user defined operator
 | T_OPERATOR
 | T_PREP 
 | T_PRE 
 | T_POSTP 
 | T_POST 
 | T_IP 
 | T_I 
 | T_LP1    -- ^ LP for Prefix 
 | T_L1     -- ^ L for Prefix 
 | T_LP4    -- ^ LP for Nofix
 | T_L4     -- ^ L for Nofix 
 | T_ELP2   -- ^ ELP for Postfix
 | T_EL2    -- ^ EL for Postfix
 | T_ELP3   -- ^ ELP for Infix 
 | T_EL3    -- ^ EL for Infix 
 | T_ERP2   -- ^ ERP for Postfix 
 | T_ER2    -- ^ ER for Postfix 
 | T_ERP4   -- ^ ERP for Nofix 
 | T_ER4    -- ^ ER for Nofix 
 | T_SRP2   -- ^ SRP for Postfix 
 | T_SR2    -- ^ SR for Postfix 
 | T_SRP4   -- ^ SRP for Nofix 
 | T_SR4    -- ^ SR for Nofix 
 | T_EREP1  -- ^ EREP for Prefix 
 | T_ERE1   -- ^ ERE for Prefix  
 | T_EREP3  -- ^ EREP for Infix 
 | T_ERE3   -- ^ ERE for Infix 
 | T_SREP1  -- ^ SREP for Prefix
 | T_SRE1   -- ^ SRE for Prefix 
 | T_SREP3  -- ^ SREP for Infix 
 | T_SRE3   -- ^ SRE for Infix 
 | T_ES 
 | T_SS 
 | T_NAME 
 | T_NAME2 
 | T_START_SCHEMATEXT -- ^ special for placemark
-- NLCHAR (7.5) newlines
 deriving (Eq,Show,Ord)

-- | Token 
data Token =
   PT  AlexPosn (Tok String) (Maybe String)
 | Err AlexPosn
 | EOF
  deriving (Eq,Show)

-- | Token to line number  
tokenPos :: [Token] -> String
tokenPos (PT (AlexPn _ l c) _ _ :_) = "at (" ++ (show l) ++ ":" ++ (show c) ++ ")"
tokenPos (Err (AlexPn _ l c) :_) = "at (" ++ (show l) ++ ":" ++ (show c) ++ ")"
tokenPos _ = "end of file"

-- | Token to Posn 
tokenPosn :: Token -> AlexPosn
tokenPosn (PT p _ _) = p
tokenPosn (Err p) = p

tokenLineCol :: Token -> (Int, Int)
tokenLineCol = posLineCol . tokenPosn

-- | get a pair of line number and column number
posLineCol :: AlexPosn -> (Int, Int)
posLineCol (AlexPn _ l c) = (l,c)

mkPosToken :: Token -> ((Int, Int), String)
mkPosToken t@(PT p _ _) = (posLineCol p, prToken t)

-- | get a Token's string 
prToken :: Token -> String
prToken t = case t of
  PT _ _ (Just s) -> s

-------------------------------------------------------------------
-- Action 
-------------------------------------------------------------------
-- AlexInput (posn, char, [byte], input)

-- | get a list of operator tokens
getListOfOpTok :: [Tok String]
-- getListOfOpTok = [T_PRE, T_PREP, T_PREP, T_PRE ,T_POSTP ,T_POST ,T_IP ,T_I ,T_LP ,T_L ,T_ELP ,T_EL ,T_ERP ,T_ER ,T_SRP ,T_SR ,T_EREP ,T_ERE ,T_SREP ,T_SRE ,T_ES ,T_SS]
getListOfOpTok = [T_PRE, T_PREP, T_PREP, T_PRE ,T_POSTP ,T_POST ,T_IP ,T_I , T_LP1, T_L1, T_LP4, T_L4, T_ELP2, T_EL2, T_ELP3, T_EL3, T_ERP2, T_ER2, T_ERP4, T_ER4, T_SRP2, T_SR2, T_SRP4, T_SR4, T_EREP1, T_ERE1, T_EREP3, T_ERE3, T_SREP1, T_SRE1, T_SREP3, T_SRE3, T_ES ,T_SS]

-- | make a Token. If it is an operator token, add it to nameTokenMap and then we can use it determine if a Token is an operator or not
mkL :: Tok String -> AlexInput -> Int -> Alex Token
-- mkL c (p,_,_,str) len = return (PT p c (Just (take len str)))
mkL c (p,_,_,str) len = do
    m <- getNameTokenMap 
    update m
    return (PT p c (Just input))
-- only if the Tok is Operator, then add it to the map
 where update x | c `elem` getListOfOpTok = setNameTokenMap (Map.insert input c x)
                | otherwise               = setNameTokenMap (x)
       input = take len str

-- | check if a string is a subscribe "_", unicode "↘  ↖"
isSubscript :: String -> Int 
isSubscript s = subIndex s 0 0

-- | determine if a string is a subscript (such as "abc_1", "a_{12}") or not. If it is, return the index of SE
-- 
-- If index = 0, it means it is not a subscript. 
-- For example,
-- 
-- - 1. "a_1"    =>  1
-- - 2. "_1"     =>  0
-- - 3. "a_12"   =>  0
subIndex :: String  -- ^ string to be processed 
    -> Int          -- ^ index of char in processing  
    -> Int          -- ^ index of SE  
    -> Int          -- ^ return the index of SE
subIndex []      ic index   = index
subIndex (x:xs)  ic index   = case x of
            '\x2198'    -> if index == 0  -- zChar_wordgluese
                                    then case ic of
                                            0   -> 0    -- SE should not be the first in the string
                                            _   -> subIndex xs (ic+1) ic
                                    else 0  --  if there is a SE before (index > 0), then it is not allowed, just return 0 to mean it is not a subscript
            '\x2196'    -> if index == 0 -- zChar_wordgluenw
                                    then 0  --  if there isn't a SE before (index = 0), then it is not allowed, just return 0 to mean it is not a subscript
                                    else case xs of -- NW should be the last char in the string
                                        []      -> index        -- this is the index we want
                                        _       -> 0            -- , otherwise just return 0
            _                   -> subIndex xs (ic+1) index
            

-- | if it is not a keyword and a T_NAME instead, check nameTokenMap map if this name has been
-- linked to a Token or not. If so, just use this linked Token for this NAME. Otherwise, it
-- is a T_NAME token
mkKeyword :: AlexInput -> Int -> Alex Token
mkKeyword (p,_,_,str) len = case c of
        T_NAME  -> case (isSubscript input) of  -- if it is (name + subscript), then we use the token for name as token for input. 
                0   -> do 
                    m <- getNameTokenMap 
                    case Map.lookup input m of
                        Just tok -> return (PT p tok (Just input)) 
                        Nothing -> return (PT p c (Just input))
                i   -> do
                    m <- getNameTokenMap 
                    case Map.lookup (take i input) m of     -- (take input i) to get the name part and remove the subscript part
                        Just tok -> return (PT p tok (Just input)) 
                        Nothing -> return (PT p c (Just input))
        _       -> return (PT p c (Just input))
    where input = take len str
          c = reservedDecorwords T_NAME input

-- | if it is not a keyword and a T_NAME instead, check nameTokenMap map if this name has been
-- linked to a Token or not. If so, just use this linked Token for this NAME. Otherwise, it
-- is a T_NAME token
mkKeyword2 :: AlexInput -> Int -> Alex Token
mkKeyword2 (p,_,_,str) len = case c of
        T_NAME2  -> case (isSubscript input) of  -- if it is (name + subscript), then we use the token for name as token for input. 
                0   -> do 
                    m <- getNameTokenMap 
                    case Map.lookup input m of
                        Just tok -> return (PT p tok (Just input)) 
                        Nothing -> return (PT p c (Just input))
                i   -> do
                    m <- getNameTokenMap 
                    case Map.lookup (take i input) m of     -- (take input i) to get the name part and remove the subscript part
                        Just tok -> return (PT p tok (Just input)) 
                        Nothing -> return (PT p c (Just input))
        T_KEYWORD ['\x2200'] no -> return (PT p (T_KEYWORD2 ['\x2200'] no) (Just input)) 
        T_KEYWORD ['\x2203'] no -> return (PT p (T_KEYWORD2 ['\x2203'] no) (Just input)) 
        T_KEYWORD "\x2203\x2198\&1\x2196" no -> return (PT p (T_KEYWORD2 "\x2203\x2198\&1\x2196" no) (Just input)) 
        T_KEYWORD ['\x03BB'] no -> return (PT p (T_KEYWORD2 ['\x03BB'] no) (Just input)) 
        T_KEYWORD ['\x03BC'] no -> return (PT p (T_KEYWORD2 ['\x03BC'] no) (Just input)) 
        T_KEYWORD ";" no        -> return (PT p (T_KEYWORD2 [';'] no) (Just input)) 
        _       -> return (PT p c (Just input))
    where input = take len str
          c = reservedDecorwords T_NAME2 input

-- | enter relation mode
enterOpRelationMode :: AlexInput -> Int -> Alex Token
enterOpRelationMode input len = do
    setOpMode OpRelation
    mkKeyword input len 

-- | enter function mode
enterOpFunctionMode :: AlexInput -> Int -> Alex Token
enterOpFunctionMode input len = do
    setOpMode OpFunction
    mkKeyword input len 

-- | enter generic mode
enterOpGenericMode :: AlexInput -> Int -> Alex Token
enterOpGenericMode input len = do
    setOpMode OpGeneric 
    mkKeyword input len 

-- | make operator keyword according to current op mode (relation/function/generic)
mkOpKeyword :: (Tok String, Tok String) -> AlexInput -> Int -> Alex Token 
mkOpKeyword (func, rel) input@(p,_,_,_) len = do
    mod <- getOpMode
    case mod of 
        OpRelation      -> mkL rel input len
        OpFunction      -> mkL func input len 
        OpGeneric       -> mkL func input len 
        OpOther         -> return (Err p) 
    

-- | just before "_)"
enterPrefixName2 :: AlexInput -> Int -> Alex Token
enterPrefixName2 input@(p,_,_,_) len = do
    tok <- getLastToken
    case tok of
        T_LPARENT           -> mkOpKeyword (T_PRE, T_PREP) input len 
        T_KEYWORD "_" _     -> mkOpKeyword (T_ERE1, T_EREP1) input len
        T_KEYWORD ",," _    -> mkOpKeyword (T_SRE1, T_SREP1) input len
        _                   -> return (Err p)

-- | (_ ES | ,, SS)* (_, ERE | ,, SRE) _)
enterPrefixName:: AlexInput -> Int -> Alex Token
enterPrefixName input@(p,_,_,_) len = do
    tok <- getLastToken
    case tok of
        T_LPARENT           -> mkOpKeyword (T_L1, T_LP1) input len
        T_KEYWORD "_" _     -> mkL T_ES input len
        T_KEYWORD ",," _    -> mkL T_SS input len
        _                   -> return (Err p)

-- | just before ")"
enterPostfixName2 :: AlexInput -> Int -> Alex Token
enterPostfixName2 input@(p,_,_,_) len = do
    tok <- getLastToken
    tok1 <- getLast1Token
    check tok1 tok
    where check t1 t | t1 `elem` [T_EL2, T_ELP2, T_ES, T_SS] = case t of 
                                                                T_KEYWORD "_" _   -> mkOpKeyword (T_ER2, T_ERP2) input len
                                                                T_KEYWORD ",," _  -> mkOpKeyword (T_SR2, T_SRP2) input len
                                                                _                 -> return (Err p)
                     | t1 == T_LPARENT              = case t of 
                                                        T_KEYWORD "_" _ -> mkOpKeyword (T_POST, T_POSTP) input len
                                                        _               -> return (Err p)
                     | otherwise                    = return (Err p)

-- | EL , {_ , ES| ,, SS } , (_, ER| ,, SR )
enterPostfixName:: AlexInput -> Int -> Alex Token
enterPostfixName input@(p,_,_,_) len = do
    tok <- getLastToken
    tok1 <- getLast1Token
    check tok1 tok
    where check t1 t | t1 `elem` [T_EL2, T_ELP2, T_ES, T_SS] = case t of 
                                                                T_KEYWORD "_" _   -> mkL T_ES input len
                                                                T_KEYWORD ",," _  -> mkL T_SS input len
                                                                _                 -> return (Err p)
                     | t1 == T_LPARENT              = case t of 
                                                        T_KEYWORD "_" _ -> mkOpKeyword (T_EL2, T_ELP2) input len
                                                        _               -> return (Err p)
                     | otherwise                    = return (Err p)

-- | just before "_)"
enterInfixName2 :: AlexInput -> Int -> Alex Token
enterInfixName2 input@(p,_,_,_) len = do
    tok <- getLastToken
    tok1 <- getLast1Token
    check tok1 tok
    where check t1 t | t1 `elem` [T_EL3, T_ELP3, T_ES, T_SS] = case t of 
                                                                T_KEYWORD "_" _   -> mkOpKeyword (T_ERE3, T_EREP3) input len
                                                                T_KEYWORD ",," _  -> mkOpKeyword (T_SRE3, T_SREP3) input len
                                                                _                 -> return (Err p)
                     | t1 == T_LPARENT              = case t of 
                                                        T_KEYWORD "_" _ -> mkOpKeyword (T_I, T_IP) input len
                                                        _               -> return (Err p)
                     | otherwise                    = return (Err p)

-- | EL , {_ , ES| ,, SS } , (_, ERE| ,, SRE ) _
enterInfixName:: AlexInput -> Int -> Alex Token
enterInfixName input@(p,_,_,_) len = do
    tok <- getLastToken
    tok1 <- getLast1Token
    check tok1 tok
    where check t1 t | t1 `elem` [T_EL3, T_ELP3, T_ES, T_SS] = case t of 
                                                                T_KEYWORD "_" _   -> mkL T_ES input len
                                                                T_KEYWORD ",," _  -> mkL T_SS input len
                                                                _                 -> return (Err p)
                     | t1 == T_LPARENT              = case t of 
                                                        T_KEYWORD "_" _ -> mkOpKeyword (T_EL3, T_ELP3) input len
                                                        _               -> return (Err p)
                     | otherwise                    = return (Err p)


-- | just before ")"
enterNofixName2 :: AlexInput -> Int -> Alex Token
enterNofixName2 input@(p,_,_,_) len = do
    tok <- getLastToken
    case tok of
        T_KEYWORD "_" _   -> mkOpKeyword (T_ER4, T_ERP4) input len
        T_KEYWORD ",," _  -> mkOpKeyword (T_SR4, T_SRP4) input len
        _                 -> return (Err p)

-- | L , {_ , ES| ,, SS } , (_, ER| ,, SR )
enterNofixName:: AlexInput -> Int -> Alex Token
enterNofixName input@(p,_,_,_) len = do
    tok <- getLastToken
    case tok of
        T_KEYWORD "_" _   -> mkL T_ES input len
        T_KEYWORD ",," _  -> mkL T_SS input len
        T_LPARENT         -> mkOpKeyword (T_L4, T_LP4) input len
        _                 -> return (Err p)

-------------------------------------------------------------------
-- Keyword search binary tree 
-------------------------------------------------------------------
-- A binary tree: 
--  B - value constructor
--  String - Label  
--  Tok - Tok constructor
--  Left - left BTree
--  Right - right BTree
-- data (Ord a) => BTree a = N | B a (Tok a) (BTree a) (BTree a) deriving (Show)

-- GADT style syntax will eliminate warning
-- "-XDatatypeContexts is deprecated: It was widely considered a misfeature, 
-- "and has been removed from the Haskell language."
-- | Keyword search binary tree
data BTree a where
    N :: Ord a => BTree a
    B :: Ord a => a -> (Tok a) -> (BTree a) -> (BTree a) -> (BTree a)

-- deriving instance Show (BTree a)

-- | search reserved keywords to check if it is a reserved word or not 
reservedDecorwords :: (Tok String) -> String -> (Tok String)
reservedDecorwords tv s = treeFind resWords
  where
  treeFind N = tv
  treeFind (B a t left right) | s < a  = treeFind left
                              | s > a  = treeFind right
                              | s == a = t

-- | return a BTree for reserved words
resWords :: BTree String 
-- resWords = buildBST (appendNumbering (sort (concat [getISOZKeywords, getCircusKeywords])) 1 )
resWords = buildBST (appendNumbering (sort (getISOZKeywords)) 1 )

unescapeInitTail :: String -> String
unescapeInitTail = id . unesc . tail . id where
  unesc s = case s of
    '\\':c:cs | elem c ['\"', '\\', '\''] -> c : unesc cs
    '\\':'n':cs  -> '\n' : unesc cs
    '\\':'t':cs  -> '\t' : unesc cs
    '"':[]    -> []
    c:cs      -> c : unesc cs
    _         -> []

-- | build the balanced binary search tree, and input list should be sorted in advance, 
--  such as buildBST [("a", 1), ("b", 2), ...]
buildBST :: (Ord a, Eq a) => [(a, Int)] -> BTree a
buildBST [] = N 
buildBST xs = B root (T_KEYWORD root n) left right
  where ((root, n),len) = middle xs
        left       = buildBST (take len xs)
        right      = buildBST (drop (len+1) xs)
        
         
-- | a helper function that gives the element in the middle and the index of it
middle :: [a] -> (a,Int)
middle [] = error (err2Msg (ErrUniLexer (EUniLexer ("Empty list"))))
middle xs = (xs !! half, half)
  where half = (length xs) `div` 2

-- | append number to each String in the list to get a pair of (String, Int)
-- 
-- For example, 'appendNumbering ["a", "b"] 1' will get [("a",1),("b",2)] 
appendNumbering :: [String] -> Int -> [(String, Int)]
appendNumbering [] n = []
appendNumbering [s] n = [(s, n)]
appendNumbering (h:t) n = [(h, n)] ++ appendNumbering t (n+1)

-------------------------------------------------------------------
-- Predicate
-------------------------------------------------------------------
-- ifPrefixName :: user -> AlexInput -> Int -> AlexInput -> Bool

-------------------------------------------------------------------
-- Alex wrapper code.
-------------------------------------------------------------------
alexEOF :: Alex Token
alexEOF = return EOF

-- | user defined operator template mode (relation, function and generic template) or not
data OpMode = OpRelation | OpFunction | OpGeneric | OpOther deriving (Eq, Show)

-- | User State
data AlexUserState = AlexUserState
                   {
                     -- used by the lexer phase
                       lexerCommentDepth  :: Int,   -- ^ 
                       lexerZedDepth :: Int,        -- ^ ZED depths
                       opTemplate :: OpMode,        -- ^ operator template mode
                     -- | last two tokens (last-1, last), for user defined operators
                       lastToken :: (Tok String, Tok String),
                     -- | a (key, value) hash map from a T_NAME to Tok. It is used to store the visited name and its Tok. So in the later stage, if a same name is encountered, we use this Tok directly. This is expecially for operator template
                       nameTokenMap :: Map String (Tok String),
                     -- | a (schemaName, bool) hash map from a name to a boolean to indicate if it is schema or not
                       schemaExprMap :: Map String Bool,
                     -- | An operator table to store all operators defined in templates. A pair from the name of operator to its related infomation including (relation|function|generic : prec : assoc). 
                     -- 
                     -- For example: 
                     -- -    ("+", "function:30:leftassoc")
                     -- -    ("≠", "relation")
                     -- -    ("ℙ", "generic::")
                       opTable :: Map String String,
                     -- | Depth of expressions in LPARENT and RPARENT, and used in parsing stage to
                     -- cope with Catersian product like 
                     --     (X x Y x Z)     ==== should be ====> ProdExpr [X, Y, Z]              where the depthes of both 'x' are equal
                     --     ((X x Y) x Z)   ==== should be ====> ProdExpr [(ProdExpr [X, Y]), Z] where the depth of the first x is equal to that of the second + 1
                     --     (X x Y op U x Z)   ==== should be ====> ProdExpr [X, (ApplExpr InfixApp Y op U), Z] where the depth of the first x is equal to that of the second and 'op' binds more tightly than 'x'
                       parserParDepth :: Int
                   }

-- | initialisation of user states 
-- 
-- Notes: the Catersian product has the precedence 8 according to p42 of ISO Standard Z
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0 0 OpOther (T_NL, T_NL) Map.empty Map.empty (Map.fromList [(zChar_cross, "generic:8:leftassoc")]) 0

-- | get ZedDepth
getLexerZedDepth :: Alex Int
getLexerZedDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerZedDepth ust)

-- | set ZedDepth
setLexerZedDepth :: Int -> Alex ()
setLexerZedDepth ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerZedDepth=ss}}, ())

-- | enter a new ZED environment (ZedDepth = 1)
enterNewZed :: Tok String -> AlexInput -> Int -> Alex Token 
enterNewZed c input len =
    do setLexerZedDepth 1
       mkL c input len 

-- | enter an embedded ZED environment (ZedDepth = ZedDepth + 1)
embedZed :: Tok String -> AlexInput -> Int -> Alex Token 
embedZed c input len =
    do cd <- getLexerZedDepth
       setLexerZedDepth (cd + 1)
       mkL c input len 

-- | exit an embedded ZED environment (ZedDepth = ZedDepth - 1)
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

state_initial :: Int
state_initial = 0

-- | get operator mode 
getOpMode :: Alex OpMode
getOpMode = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, opTemplate ust)

-- | set operator mode 
setOpMode :: OpMode -> Alex ()
setOpMode ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){opTemplate=ss}}, ())

-- | get last two Tokens
getLast2Token :: Alex (Tok String, Tok String)
getLast2Token = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lastToken ust)

-- | get last Token
getLastToken :: Alex (Tok String)
getLastToken = do 
    l <- getLast2Token
    return (snd l)

-- | get the second last Token
getLast1Token :: Alex (Tok String)
getLast1Token = do 
    l <- getLast2Token
    return (fst l) 

-- | set last two tokens
setLast2Token :: (Tok String, Tok String) -> Alex ()
setLast2Token ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lastToken=ss}}, ())

-- | set last token
setLastToken :: Token -> Alex ()
setLastToken ss@(PT _ t _) = do tok <- getLastToken; setLast2Token (tok, t)
setLastToken ss@(Err p) = lexerError (err2Msg (ErrUniLexer (EUniLexer ("The Err token is not expected."))))
setLastToken EOF = lexerError (err2Msg (ErrUniLexer (EUniLexer ("The EOF token is not expected."))))

-- | get nameTokenMap
getNameTokenMap :: Alex (Map String (Tok String))
getNameTokenMap = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, nameTokenMap ust)

-- | set nameTokenMap
setNameTokenMap :: (Map String (Tok String)) -> Alex ()
setNameTokenMap ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){nameTokenMap=ss}}, ())

-- | get schemaExprMap 
getSchemaExprMap :: Alex (Map String Bool)
getSchemaExprMap = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, schemaExprMap ust)

-- | set schemaExprMap 
setSchemaExprMap :: (Map String Bool) -> Alex ()
setSchemaExprMap ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){schemaExprMap=ss}}, ())

-- | get OpTable 
getOpTable :: Alex (Map String String)
getOpTable = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, opTable ust)

-- | set OpTable
setOpTable :: (Map String String) -> Alex ()
setOpTable ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){opTable=ss}}, ())

-- | get parserParDepth 
getParserParDepth :: Alex Int
getParserParDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, parserParDepth ust)

-- | set parserParDepth 
setParserParDepth :: Int -> Alex ()
setParserParDepth ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){parserParDepth=ss}}, ())

showPosn :: AlexPosn -> String
showPosn (AlexPn _ l c) = "at (" ++ (show l) ++ ": " ++ (show c) ++ ")"

lexerError :: String -> Alex a
lexerError msg =
    do (p, c, _, inp) <- alexGetInput
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
       alexError (disp3 ++ " at " ++ showPosn p ++ disp)
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

errMsg :: String -> AlexInput -> Int -> Alex a
errMsg s (_,_,_,input) len = lexerError (err2Msg (ErrUniLexer (EUniLexer ("Expect: [" ++ s ++ "]. But it is [" ++ (take len input) ++ "]."))))

-- get the Tok from a Token
isNL :: Token -> Bool
isNL t@(PT p T_NL _) = True
isNL _ = False 

-- | check if it is a soft NL or hard NL.
-- 
-- a NLCHAR is soft only if it is checked to be soft from both the token before and the token after
isHardNL :: Token -> Token -> Bool 
isHardNL last next = not ((isAfterSoft last) || (isBeforeSoft next))
    where isAfterSoft t@(PT _ (T_KEYWORD "else" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "function" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "generic" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "relation" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "leftassoc" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "parents" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "rightassoc" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "section" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "then" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "::=" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "|" _) _) = True
          isAfterSoft t@(PT _ T_LDATA _) = True
          -- Since >> has completed, NL after that should be regarded as hard space 
          isAfterSoft t@(PT _ T_RDATA _) = False
          isAfterSoft t@(PT _ (T_KEYWORD "&" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\x22A2?" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD ",," _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\8744" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\8743" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\8658" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\8660" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\215" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\x2F" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "=" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\8712" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "==" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD ":" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD ";" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "," _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "." _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\10625" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\10745" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\10785" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\10783" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\10784" _) _) = True
          isAfterSoft t@(PT _ T_I _) = True
          isAfterSoft t@(PT _ T_IP _) = True
          isAfterSoft t@(PT _ T_EL2 _) = True
          isAfterSoft t@(PT _ T_EL3 _) = True
          isAfterSoft t@(PT _ T_ELP2 _) = True
          isAfterSoft t@(PT _ T_ELP3 _) = True
          isAfterSoft t@(PT _ T_ERE1 _) = True
          isAfterSoft t@(PT _ T_ERE3 _) = True
          isAfterSoft t@(PT _ T_EREP1 _) = True
          isAfterSoft t@(PT _ T_EREP3 _) = True
          isAfterSoft t@(PT _ T_ES _) = True
          isAfterSoft t@(PT _ T_SS _) = True
          isAfterSoft t@(PT _ T_SRE1 _) = True
          isAfterSoft t@(PT _ T_SRE3 _) = True
          isAfterSoft t@(PT _ T_SREP1 _) = True
          isAfterSoft t@(PT _ T_SREP3 _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "if" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "let" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "pre" _) _) = True
          isAfterSoft t@(PT _ T_LBRACKET _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "_" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\172" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\8704" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\8707" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\x2203\x2198\&1\x2196" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\x2119" _) _) = True
          isAfterSoft t@(PT _ T_LPARENT _) = True
          isAfterSoft t@(PT _ T_LBRACE _) = True
          isAfterSoft t@(PT _ T_LBLOT _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\955" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\956" _) _) = True
          isAfterSoft t@(PT _ (T_KEYWORD "\952" _) _) = True
          isAfterSoft t@(PT _ T_PRE _) = True
          isAfterSoft t@(PT _ T_PREP _) = True
          isAfterSoft t@(PT _ T_L1 _) = True
          isAfterSoft t@(PT _ T_L4 _) = True
          isAfterSoft t@(PT _ T_LP1 _) = True
          isAfterSoft t@(PT _ T_LP4 _) = True
          -- Tokens that cannot appear in these contexts are in category BOTH. This includes the box tokens.
          isAfterSoft t@(PT _ T_ZED _) = True
          isAfterSoft t@(PT _ T_ZED_SEC _) = True
          isAfterSoft t@(PT _ T_END _) = True
          isAfterSoft t@(PT _ T_AX _) = True
          isAfterSoft t@(PT _ T_AX2 _) = True
          isAfterSoft t@(PT _ T_SCH _) = True
          isAfterSoft t@(PT _ T_CONJ _) = True
          isAfterSoft t@(PT _ T_GENAX _) = True
          isAfterSoft t@(PT _ T_GENSCH _) = True
          isAfterSoft t@(PT _ T_GENCONJ _) = True
          isAfterSoft t@(PT _ T_NAME _) = False
          isAfterSoft t@(PT _ T_NUMERAL _) = False
          isAfterSoft t@(PT _ T_STROKE _) = False
          isAfterSoft t@(PT _ (T_KEYWORD "false" _) _) = False
          isAfterSoft t@(PT _ (T_KEYWORD "true" _) _) = False
          isAfterSoft _ = False
          isBeforeSoft t@(PT _ (T_KEYWORD "else" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "function" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "generic" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "relation" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "leftassoc" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "parents" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "rightassoc" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "section" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "then" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "::=" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "|" _) _) = True
          isBeforeSoft t@(PT _ T_LDATA _) = True
          isBeforeSoft t@(PT _ T_RDATA _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "&" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "\x22A2?" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD ",," _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "\8744" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "\8743" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "\8658" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "\8660" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "\215" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "\x2F" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "=" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "\8712" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "==" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD ":" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD ";" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "," _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "." _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "\10625" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "\10745" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "\10785" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "\10783" _) _) = True
          isBeforeSoft t@(PT _ (T_KEYWORD "\10784" _) _) = True
          isBeforeSoft t@(PT _ T_I _) = True
          isBeforeSoft t@(PT _ T_IP _) = True
          isBeforeSoft t@(PT _ T_EL2 _) = True
          isBeforeSoft t@(PT _ T_EL3 _) = True
          isBeforeSoft t@(PT _ T_ELP2 _) = True
          isBeforeSoft t@(PT _ T_ELP3 _) = True
          isBeforeSoft t@(PT _ T_ERE1 _) = True
          isBeforeSoft t@(PT _ T_ERE3 _) = True
          isBeforeSoft t@(PT _ T_EREP1 _) = True
          isBeforeSoft t@(PT _ T_EREP3 _) = True
          isBeforeSoft t@(PT _ T_ES _) = True
          isBeforeSoft t@(PT _ T_SS _) = True
          isBeforeSoft t@(PT _ T_SRE1 _) = True
          isBeforeSoft t@(PT _ T_SRE3 _) = True
          isBeforeSoft t@(PT _ T_SREP1 _) = True
          isBeforeSoft t@(PT _ T_SREP3 _) = True
          isBeforeSoft t@(PT _  T_RBRACKET  _) = True
          isBeforeSoft t@(PT _  T_RPARENT  _) = True
          isBeforeSoft t@(PT _  T_RBRACE _) = True
          isBeforeSoft t@(PT _  T_RBLOT _) = True
          isBeforeSoft t@(PT _  T_POST _) = True
          isBeforeSoft t@(PT _  T_POSTP _) = True
          isBeforeSoft t@(PT _  T_ER2 _) = True
          isBeforeSoft t@(PT _  T_ER4 _) = True
          isBeforeSoft t@(PT _  T_ERP2 _) = True
          isBeforeSoft t@(PT _  T_ERP4 _) = True
          isBeforeSoft t@(PT _  T_SR2 _) = True
          isBeforeSoft t@(PT _  T_SR4 _) = True
          isBeforeSoft t@(PT _  T_SRP2 _) = True
          isBeforeSoft t@(PT _  T_SRP4 _) = True
          -- Tokens that cannot appear in these contexts are in category BOTH. This includes the box tokens.
          isBeforeSoft t@(PT _ T_ZED _) = True
          isBeforeSoft t@(PT _ T_ZED_SEC _) = True
          isBeforeSoft t@(PT _ T_END _) = True
          isBeforeSoft t@(PT _ T_AX _) = True
          isBeforeSoft t@(PT _ T_AX2 _) = True
          isBeforeSoft t@(PT _ T_SCH _) = True
          isBeforeSoft t@(PT _ T_CONJ _) = True
          isBeforeSoft t@(PT _ T_GENAX _) = True
          isBeforeSoft t@(PT _ T_GENSCH _) = True
          isBeforeSoft t@(PT _ T_GENCONJ _) = True
          isBeforeSoft t@(PT _ T_NAME _) = False
          isBeforeSoft t@(PT _ T_NUMERAL _) = False
          isBeforeSoft t@(PT _ T_STROKE _) = False
          isBeforeSoft t@(PT _ (T_KEYWORD "false" _) _) = False
          isBeforeSoft t@(PT _ (T_KEYWORD "true" _) _) = False
          isBeforeSoft _ = False 

scanner :: String -> Either String [Token]
scanner str = 
  let loop = do
        tok <- alexMonadScan
        if tok == EOF
        then return []
        else do 
            if isNL tok 
            then do 
                 toklast <- getLastToken 
                 toks <- loop 
                 if not (null toks) && isHardNL (PT (AlexPn 0 0 0) toklast (Just "")) (head toks) 
                 then return (tok:toks)
                 else return toks
            else do 
                case tok of
                    PT pn T_AX2 s -> do setLastToken n_tok; toks <- loop; return (n_tok:(tmp_tok:toks))
                                    where n_tok = PT pn T_AX s
                                          tmp_tok = PT pn T_START_SCHEMATEXT (Just "")
                    PT pn T_NAME2 s -> do setLastToken n_tok; toks <- loop; return (n_tok:(tmp_tok:toks))
                                    where n_tok = PT pn T_NAME s
                                          tmp_tok = PT pn T_START_SCHEMATEXT (Just "")
                    PT pn T_LBRACE2 s -> do setLastToken n_tok; toks <- loop; return (n_tok:(tmp_tok:toks))
                                    where n_tok = PT pn T_LBRACE s
                                          tmp_tok = PT pn T_START_SCHEMATEXT (Just "")
                    PT pn T_LBRACKET2 s -> do setLastToken n_tok; toks <- loop; return (n_tok:(tmp_tok:toks))
                                    where n_tok = PT pn T_LBRACKET s
                                          tmp_tok = PT pn T_START_SCHEMATEXT (Just "")
                    PT pn T_RBRACKET2 s -> do setLastToken n_tok; toks <- loop; return (n_tok:(tmp_tok:toks))
                                    where n_tok = PT pn T_RBRACKET s
                                          tmp_tok = PT pn T_START_SCHEMATEXT (Just "")
                    PT pn (T_KEYWORD2 k n) s -> do setLastToken n_tok; toks <- loop; return (n_tok:(tmp_tok:toks))
                                    where n_tok = PT pn (T_KEYWORD k n) s
                                          tmp_tok = PT pn T_START_SCHEMATEXT (Just "")
                    PT pn T_NL2 s -> do setLastToken n_tok; toks <- loop; return (n_tok:(tmp_tok:toks))
                                    where n_tok = PT pn T_NL s
                                          tmp_tok = PT pn T_START_SCHEMATEXT (Just "")
                    _           -> do setLastToken tok; toks <- loop; return (tok:toks)
  in runAlex str loop

-------------------------------------------------------------------
-- main 
-------------------------------------------------------------------
--main = do
--    args <- getArgs
--    case args of 
--        [input] -> readAndScanner input
--        _       -> putStrLn "Error: expect one argument only"
--    where readAndScanner filename = do 
--            h <- openFile filename ReadMode
--            content <- hGetContents h
--            let result = scanner content
--            print result
--}
