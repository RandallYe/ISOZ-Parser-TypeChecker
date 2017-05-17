{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}

{- |
Module      : Language.ISOZ.Parser.ISOZParser 
Description : 
Copyright   : (C) Kangfeng Ye, 2017
License     : BSD3
Maintainer  : ye.randall@gmail.com 
Stability   : experimental

ISOZParser is a parser for ISO Standard Z. 

-}
module Language.ISOZ.Parser.ISOZParser where

import Language.ISOZ.ZChar 
import Language.Circus.CircusChar 
import Language.ISOZ.Common.Error
import Language.ISOZ.Common.AbsIsoz as AbsIsoz
import Language.ISOZ.Lexer.ISOZLexer 
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Language.ISOZ.Utils.PrintIsoz (printTree)
import Language.ISOZ.Parser.SynTransformerOne (getAnnTerm, addAnnTerm, getAnnTermAndRemoveExprInPars, getAnnTermAndRemoveExprInPars1, showPos)
import Language.ISOZ.Common.Ann
}

%name pSpecification Specification
-- no lexer declaration
-- %monad { Err } { thenM } { returnM }
%monad { Alex } { thenA } { returnA }
%tokentype {Token}

%token
L_LPARENT { PT _ T_LPARENT (Just _) }
L_RPARENT { PT _ T_RPARENT (Just _) }
L_LBRACKET { PT _ T_LBRACKET (Just _) }
L_LBRACKET3 { PT _ T_LBRACKET3 (Just _) }
L_RBRACKET { PT _ T_RBRACKET (Just _) }
L_LBRACE { PT _ T_LBRACE (Just _) }
L_RBRACE { PT _ T_RBRACE (Just _) }
L_LBLOT { PT _ T_LBLOT (Just _) }
L_RBLOT { PT _ T_RBLOT (Just _) }
L_LDATA { PT _ T_LDATA (Just _) }
L_RDATA { PT _ T_RDATA (Just _) }
L_LCHANSET { PT _ T_LCHANSET (Just _) }
L_RCHANSET { PT _ T_RCHANSET (Just _) }
L_LCIRCINDEX { PT _ T_LCIRCINDEX (Just _) }
L_RCIRCINDEX { PT _ T_RCIRCINDEX (Just _) }
L_LCIRCGUARD { PT _ T_LCIRCGUARD (Just _) }
L_RCIRCGUARD { PT _ T_RCIRCGUARD (Just _) }
L_LCIRCRENAME { PT _ T_LCIRCRENAME (Just _) }
L_RCIRCRENAME { PT _ T_RCIRCRENAME (Just _) }
L_LPAR { PT _ T_LPAR (Just _) }
L_RPAR { PT _ T_RPAR (Just _) }
L_LINTER { PT _ T_LINTER (Just _) }
L_RINTER { PT _ T_RINTER (Just _) }
L_NUMERAL { PT _ T_NUMERAL (Just _) }
L_STROKE { PT _ T_STROKE (Just _) }
L_ZED { PT _ T_ZED (Just _) }
L_ZED_SEC { PT _ T_ZED_SEC (Just _) }
L_AX { PT _ T_AX (Just _) }
L_SCH { PT _ T_SCH (Just _) }
L_CONJ { PT _ T_CONJ (Just _) }
L_GENAX { PT _ T_GENAX (Just _) }
L_GENSCH { PT _ T_GENSCH (Just _) }
L_GENCONJ { PT _ T_GENCONJ (Just _) }
L_END { PT _ T_END (Just _) }
L_NL { PT _ T_NL (Just _) }
--L_WORD { PT _ T_WORD (Just _) }
--L_DECORWORD { PT _ T_DECORWORD (Just _) }
--L_OPERATOR { PT _ T_OPERATOR (Just _) }
L_PREP  { PT _  T_PREP  (Just _)}
L_PRE   { PT _  T_PRE   (Just _)}
L_POSTP { PT _  T_POSTP (Just _)}
L_POST  { PT _  T_POST  (Just _)}
L_IP    { PT _  T_IP    (Just _)}
L_I     { PT _  T_I     (Just _)}
L_LP1    { PT _  T_LP1    (Just _)}
L_LP4    { PT _  T_LP4    (Just _)}
L_L1     { PT _  T_L1     (Just _)}
L_L4     { PT _  T_L4     (Just _)}
L_ELP2   { PT _  T_ELP2   (Just _)}
L_ELP3   { PT _  T_ELP3   (Just _)}
L_EL2    { PT _  T_EL2    (Just _)}
L_EL3    { PT _  T_EL3    (Just _)}
L_ERP2   { PT _  T_ERP2   (Just _)}
L_ERP4   { PT _  T_ERP4   (Just _)}
L_ER2    { PT _  T_ER2    (Just _)}
L_ER4    { PT _  T_ER4    (Just _)}
L_SRP2   { PT _  T_SRP2   (Just _)}
L_SRP4   { PT _  T_SRP4   (Just _)}
L_SR2    { PT _  T_SR2    (Just _)}
L_SR4    { PT _  T_SR4    (Just _)}
L_EREP1  { PT _  T_EREP1  (Just _)}
L_EREP3  { PT _  T_EREP3  (Just _)}
L_ERE1   { PT _  T_ERE1   (Just _)}
L_ERE3   { PT _  T_ERE3   (Just _)}
L_SREP1  { PT _  T_SREP1  (Just _)}
L_SREP3  { PT _  T_SREP3  (Just _)}
L_SRE1   { PT _  T_SRE1   (Just _)}
L_SRE3   { PT _  T_SRE3   (Just _)}
L_ES    { PT _  T_ES    (Just _)}
L_SS    { PT _  T_SS    (Just _)}
L_NAME { PT _ T_NAME (Just _) }
--L_KEYWORD { PT _ (T_KEYWORD _ 1) _ }
L_START { PT _ T_START_SCHEMATEXT (Just _)}

'then' { PT _ (T_KEYWORD "then" _) _ }
'else' { PT _ (T_KEYWORD "else" _) _ }
':' { PT _ (T_KEYWORD ":" _) _ }
',,' { PT _ (T_KEYWORD ",," _) _ }
',' { PT _ (T_KEYWORD "," _) _ }
'&' { PT _ (T_KEYWORD "&" _) _ }
'/' { PT _ (T_KEYWORD "/" _) _ } {- \x2F -}
'.' { PT _ (T_KEYWORD "." _) _ }
'=' { PT _ (T_KEYWORD "=" _) _ }
';' { PT _ (T_KEYWORD ";" _) _ }
'::=' { PT _ (T_KEYWORD "::=" _) _ }
'_' { PT _ (T_KEYWORD "_" _) _ }
'==' { PT _ (T_KEYWORD "==" _) _ }
'let' { PT _ (T_KEYWORD "let" _) _ }
'generic' { PT _ (T_KEYWORD "generic" _) _ }
'function' { PT _ (T_KEYWORD "function" _) _ }
'false' { PT _ (T_KEYWORD "false" _) _ }
'leftassoc' { PT _ (T_KEYWORD "leftassoc" _) _ }
'if' { PT _ (T_KEYWORD "if" _) _ }
'relation' { PT _ (T_KEYWORD "relation" _) _ }
'pre' { PT _ (T_KEYWORD "pre" _) _ }
'parents' { PT _ (T_KEYWORD "parents" _) _ }
'section' { PT _ (T_KEYWORD "section" _) _ }
'rightassoc' { PT _ (T_KEYWORD "rightassoc" _) _ }
'exists' { PT _ (T_KEYWORD "\8707" _) _ } {- exists -}
'lambda' { PT _ (T_KEYWORD "\955" _) _ } {- lambda -}
'lnot' { PT _ (T_KEYWORD "\172" _) _ } {- lnot -}
'|' { PT _ (T_KEYWORD "|" _) _ }
'true' { PT _ (T_KEYWORD "true" _) _ }
'theta' { PT _ (T_KEYWORD "\952" _) _ } {- theta -}
'cross' { PT _ (T_KEYWORD "\215" _) _ } {- cross -}
'implies' { PT _ (T_KEYWORD "\8658" _) _ } {- => implies -}
'powerset' { PT _ (T_KEYWORD "\8473" _) _ }
'mu' { PT _ (T_KEYWORD "\956" _) _ } {- \mu -}
'forall' { PT _ (T_KEYWORD "\8704" _) _ } {- forall -}
'equiv' { PT _ (T_KEYWORD "\8660" _) _ } {- <=> equivalent -}
'spot' { PT _ (T_KEYWORD "\10625" _) _ } {- . spot -}
'land' { PT _ (T_KEYWORD "\8743" _) _ } {- land -}
'in' { PT _ (T_KEYWORD "\8712" _) _ } {- \in -}
'exists1' { PT _ (T_KEYWORD "\8707\8600\&1\8598" _) _ } {- exists_1 -}
'conjecture' { PT _ (T_KEYWORD "\8866?" _) _ } {- conjecture -}
'lor' { PT _ (T_KEYWORD "\8744" _) _ } {- lor -}
'pipe' { PT _ (T_KEYWORD "\10784" _) _ } {- >> pipe -}
'compose' { PT _ (T_KEYWORD "\10783" _) _ } {- ; compose -}
'hide' { PT _ (T_KEYWORD "\10745" _) _ } {- \ hide -}
'project' { PT _ (T_KEYWORD "\10785" _) _ } {- project -}
{-
 - settoolkit
 -}
'union'          { PT _  T_I     (Just zChar_union)}
'inter'          { PT _  T_I     (Just zChar_inter)}
'diff'           { PT _  T_I     (Just zChar_diff)}
{-
 - circus 
 -}
'circrefines' { PT _ (T_KEYWORD ['\x2291'] _) _ }       -- circrefines    
'circsimulates' { PT _ (T_KEYWORD ['\x227C'] _) _ }     --circsimulates
'circassertref' { PT _ (T_KEYWORD "assert" _) _ }       -- circassertref
'prefixcolon' { PT _ (T_KEYWORD ['\x2236'] _) _ }       --prefixcolon
'circdef' { PT _ (T_KEYWORD ['\x2259'] _) _ }           --circdef
'circspot' { PT _ (T_KEYWORD ['\x2219'] _) _ }          --circspot
'circindex' { PT _ (T_KEYWORD ['\x2299'] _) _ }         --circindex
'circmu' { PT _ (T_KEYWORD ['\x00B5'] _) _ }            --circmu
'circthen' { PT _ (T_KEYWORD ['\x27FC'] _) _ }          --circthen
'circelse' { PT _ (T_KEYWORD ['\x25AF'] _) _ }          --circelse
'\then' { PT _ (T_KEYWORD ['\x27F6'] _) _ }              --then
'circguard' { PT _ (T_KEYWORD ['\x0026'] _) _ }         --circguard
'circseq' { PT _ (T_KEYWORD ['\x037E'] _) _ }          --circseq 
'circinterrupt' { PT _ (T_KEYWORD ['\x25B3'] _) _ }     --circinterrupt
'Semi' { PT _ (T_KEYWORD ['\x2A1F'] _) _ }              --Semi
'interleave' { PT _ (T_KEYWORD ['\x2980'] _) _ }        --interleave
'Parallel' { PT _ (T_KEYWORD ['\x2225'] _) _ }         --Parallel 
'Interleave' { PT _ (T_KEYWORD ['\x2AFC'] _) _ }        --Interleave
'circhide' { PT _ (T_KEYWORD ['\x2AF5'] _) _ }         --circhide 
'extchoice' { PT _ (T_KEYWORD ['\x25FB'] _) _ }         --extchoice
'Extchoice' { PT _ (T_KEYWORD ['\x25A1'] _) _ }         --Extchoice
'intchoice' { PT _ (T_KEYWORD ['\x2293'] _) _ }         --intchoice
'Intchoice' { PT _ (T_KEYWORD ['\x25A1'] _) _ }         --Intchoice
-- brackets
--'lchanset' { PT _ (T_KEYWORD ['\x2983'] _) _ }          --lchanset
--'rchanset' { PT _ (T_KEYWORD ['\x2984'] _) _ }          --rchanset
--'lcircindex' { PT _ (T_KEYWORD ['\x230A'] _) _ }       --lcircindex 
--'rcircindex' { PT _ (T_KEYWORD ['\x230B'] _) _ }       --rcircindex 
--'lcircguard' { PT _ (T_KEYWORD ['\x3014'] _) _ }        --lcircguard
--'rcircguard' { PT _ (T_KEYWORD ['\x3015'] _) _ }       --rcircguard 
--'lcircrename' { PT _ (T_KEYWORD ['\x2768'] _) _ }      --lcircrename 
--'rcircrename' { PT _ (T_KEYWORD ['\x2769'] _) _ }      --rcircrename 
--'lpar' { PT _ (T_KEYWORD ['\x27E6'] _) _ }             --lpar 
--'rpar' { PT _ (T_KEYWORD ['\x27E7'] _) _ }             --rpar 
--'linter' { PT _ (T_KEYWORD ['\x301A'] _) _ }           --linter 
--'rinter' { PT _ (T_KEYWORD ['\x301B'] _) _ }           --rinter 
'circif' { PT _ (T_KEYWORD "if" _) _ }                  --circif
'circfi' { PT _ (T_KEYWORD "fi" _) _ }                  --circfi
'circdo' { PT _ (T_KEYWORD "do" _) _ }                  --circdo
'circod' { PT _ (T_KEYWORD "od" _) _ }                  --circod
'circcon' { PT _ (T_KEYWORD "con" _) _ }                --circcon
'circvar' { PT _ (T_KEYWORD "var" _) _ }                --circvar
'circval' { PT _ (T_KEYWORD "val" _) _ }                --circval
'circres' { PT _ (T_KEYWORD "res" _) _ }                --circres
'circvres' { PT _ (T_KEYWORD "vres" _) _ }              --circvres
'circchannel' { PT _ (T_KEYWORD "channel" _) _ }        --circchannel
'circchannelfrom' { PT _ (T_KEYWORD "channelfrom" _) _ }--circchannelfrom
'circchannelset' { PT _ (T_KEYWORD "channelset" _) _ }  --circchannelset
'circnameset' { PT _ (T_KEYWORD "nameset" _) _ }       --circnameset 
'circprocess' { PT _ (T_KEYWORD "process" _) _ }       --circprocess 
'circbegin' { PT _ (T_KEYWORD "begin" _) _ }           --circbegin 
'circend' { PT _ (T_KEYWORD "end" _) _ }               --circend 
'circstate' { PT _ (T_KEYWORD "circstate" _) _ }        --circstate
'Skip' { PT _ (T_KEYWORD "Skip" _) _ }                 --Skip 
'Stop' { PT _ (T_KEYWORD "Stop" _) _ }                 --Stop 
'Chaos' { PT _ (T_KEYWORD "Chaos" _) _ }               --Chaos 

-- precedences and associativities
-- Order: from low to high precedences (bind more strongly)
-- all prefixs are right associative
-- %right L_PRE L_PREP 
-- all postfixs are left associative
-- %left L_POST L_POSTP

%left ';' L_NL
%right 'forall' 'exists' 'exists_1' 
%left 'equiv'
%right 'implies'
%left 'lor'
%left 'land'
%right 'lnot'
%right L_PREP L_LP1 L_LP4 
%left L_POSTP L_ELP2 L_ELP3 L_IP 
%right 'lambda'
%right 'mu'
%right 'let'
%right 'if' 'then' 'else'
%left 'compose'
%left 'pipe'
%left 'hide'
%left 'project'
%right 'pre'
%left 'cross' L_I L_EL3
%right 'powerset' L_PRE L_L1
%left L_POST L_EL2 
%left L_EL4 -- nofix and it is not defined in the Table 31
-- Application
%left L_STROKE 
%left '/' 
%nonassoc '.' 
%left 'theta'


%%


-- ListNAME:: { [NAME] }
ListNAME : L_NAME { [(prToken $1)] } 
         | ListNAME ',' L_NAME { flip (:) $1 (prToken $3) }

ListSection :: { [Section] }
ListSection : Section { [$1] }
            | ListSection Section { flip (:) $1 $2 }

ListParagraph0 :: { [Paragraph] }
ListParagraph0 : {- empty -} { [] }
              | ListParagraph0 Paragraph { flip (:) $1 $2 }

ListParagraph :: { [Paragraph] }
ListParagraph : Paragraph { [$1] }
              | ListParagraph Paragraph { flip (:) $1 $2 }

Specification :: { Specification }
Specification : ListSection 
                    -- For a specification, its annotation contains a schema map and an operation table
                    {% do {m <- getOpTable; m1 <- getSchemaExprMap; returnA (AbsIsoz.SpecSect ((SchNameMapAnn m1) : ((OpMapAnn m): (getAnnTermAndRemoveExprInPars1 $1))) (reverse $1))} 
                    } 
        
              | ListParagraph 
                    -- For a specification, its annotation contains a schema map and an operation table
                    {% do {m <- getOpTable; m1 <- getSchemaExprMap; returnA (AbsIsoz.SpecAnony ((SchNameMapAnn m1) : ((OpMapAnn m): (getAnnTermAndRemoveExprInPars1 $1))) (reverse $1))} 
                    }
              | {- empty -} { AbsIsoz.SpecEmpty [] }
                -- ListSection { AbsIsoz.SpecSect (getAnnTermAndRemoveExprInPars1 $1) (reverse $1) }
              -- | ListParagraph { AbsIsoz.SpecAnony (getAnnTermAndRemoveExprInPars1 $1) (reverse $1) }

Section :: { Section }
Section : L_ZED_SEC 'section' L_NAME SectionParents L_END ListParagraph0 { AbsIsoz.Section (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (prToken $3) $4 (prToken $5) (reverse $6) }

SectionParents :: { SectionParents }
SectionParents : {- empty -} { AbsIsoz.BaseSection2 [] }
               | 'parents' ListNAME { AbsIsoz.InheritingSection2 (getAnnTermAndRemoveExprInPars1 $1) $2}

Paragraph :: { Paragraph }
Paragraph : L_ZED ListParagraphBody L_END { AbsIsoz.AxPara (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) $2 (prToken $3) }
          | L_GENCONJ L_NAME L_LBRACKET Formals L_RBRACKET 'conjecture' ExprPredicate_P L_END { AbsIsoz.EPGenConjecturePara (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) $4 $7 }
          | L_CONJ L_NAME 'conjecture' ExprPredicate_P L_END  { AbsIsoz.EPConjecturePara (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) $4 }
          | L_AX SchemaText L_END { AbsIsoz.AxdefPara (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) $2 (prToken $3) }
          | L_SCH L_NAME SchemaText L_END {% do {setToSchemaExpr (prToken $2) ; returnA (AbsIsoz.SchdefPara (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (prToken $2) $3 (prToken $4)) } }
          | L_GENAX L_LBRACKET Formals L_RBRACKET SchemaText L_END { AbsIsoz.GenAxdefPara (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) $3 $5 (prToken $6) }
          | L_GENSCH L_NAME L_LBRACKET Formals L_RBRACKET SchemaText L_END { AbsIsoz.GenSchdefPara (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) $4 (prToken $2) $6 (prToken $7) }

ListParagraphBody :: {[ParagraphBody]}
ListParagraphBody : ListParagraphBodysInOneZed { (reverse $1) }

ListParagraphBodysInOneZed :: {[ParagraphBody]}
ListParagraphBodysInOneZed : {- empty -}     { [] }
                           | ParagraphBodysInOneZed { [$1] }
                           | ListParagraphBodysInOneZed L_NL ParagraphBodysInOneZed { flip (:) $1 $3 }

-- | Paragraph bodys that can occur in a ZED environment
ParagraphBodysInOneZed :: { ParagraphBody } 
ParagraphBodysInOneZed : L_LBRACKET ListNAME L_RBRACKET { AbsIsoz.GivenPara2 (getAnnTermAndRemoveExprInPars1 $1) $2 }
              | DeclName '==' ExprPredicate 
                                {% do r <- isSchemaExpr $3; case r of
                                    False -> do {returnA (AbsIsoz.EPHDefPara2 (getAnnTermAndRemoveExprInPars1 $1) $1 $3) }
                                    True -> case $1 of 
                                        DName _ n   -> do {setToSchemaExpr n; returnA (AbsIsoz.EPHDefPara2 (getAnnTermAndRemoveExprInPars1 $1) $1 $3) }
                                        _           -> do {returnA (AbsIsoz.EPHDefPara2 (getAnnTermAndRemoveExprInPars1 $1) $1 $3) }
                                }
-- In ISO Z, it is NAME. However, it will not make "< ,, >" be parsed correctly
-- So DeclName would be right here
              | DeclName L_LBRACKET Formals L_RBRACKET '==' ExprPredicate  
                                {% do r <- isSchemaExpr $6; case r of
                                    False -> do {returnA (AbsIsoz.EPGenHDefPara2 (getAnnTermAndRemoveExprInPars1 $1) ($1) $3 $6)}
                                    True -> case $1 of 
                                        DName _ n -> do {setToSchemaExpr n; returnA (AbsIsoz.EPGenHDefPara2 (getAnnTermAndRemoveExprInPars1 $1) ($1) $3 $6) }
                                        _         -> do {returnA (AbsIsoz.EPGenHDefPara2 (getAnnTermAndRemoveExprInPars1 $1) ($1) $3 $6)}
                                }
              | GenName '==' ExprPredicate { AbsIsoz.EPGenOpDefPara2 (getAnnTermAndRemoveExprInPars1 $1) $1 $3 }
              | ListFreetype { AbsIsoz.FreetypePara2 (getAnnTermAndRemoveExprInPars1 $1) (reverse $1) }
              | OperatorTemplate { AbsIsoz.OperatorPara2 (getAnnTermAndRemoveExprInPars1 $1) $1 }

ListFreetype :: { [Freetype] }
ListFreetype : Freetype { [$1] }
             | ListFreetype '&' Freetype { flip (:) $1 $3 }
        
Freetype :: { Freetype }
Freetype : L_NAME '::=' ListBranch { AbsIsoz.Freetype (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (reverse $3) }

ListBranch :: { [Branch] }
ListBranch : Branch { [$1] }
           | ListBranch '|' Branch { flip (:) $1 $3 }

Branch :: { Branch }
Branch : DeclName { AbsIsoz.ConstantBranch (getAnnTermAndRemoveExprInPars1 $1) $1 }  
       | DeclName L_LDATA ExprPredicate L_RDATA { AbsIsoz.EPConstructorBranch (getAnnTermAndRemoveExprInPars1 $1) $1 $3 }  

Formals :: { Formals }
Formals : ListNAME { AbsIsoz.Formals [] (reverse $1) } 
--Formals : ListNAME { AbsIsoz.Formals (getAnnTermAndRemoveExprInPars1 $1) (reverse $1) } 

----------------------------------
-- For Predicate context
ExprPredicate_P :: { ExprPredicate } 
ExprPredicate_P : ExprPredicate_P L_NL ExprPredicate_P { AbsIsoz.EPAnd (getAnnTermAndRemoveExprInPars1 $1) $1 $3 }
              | ExprPredicate_P ';' ExprPredicate_P { AbsIsoz.EPAnd (getAnnTermAndRemoveExprInPars1 $1) $1 $3 }
              | ExprPredicate { $1 }

-- For Expression context
ExprPredicate :: { ExprPredicate }
ExprPredicate : 'forall' SchemaText 'spot' ExprPredicate { AbsIsoz.EPForall (getAnnTermAndRemoveExprInPars1 $1) $2 $4 }
           | 'exists' SchemaText 'spot' ExprPredicate { AbsIsoz.EPExists (getAnnTermAndRemoveExprInPars1 $1) $2 $4 }
           | 'exists1' SchemaText 'spot' ExprPredicate { AbsIsoz.EPExists1 (getAnnTermAndRemoveExprInPars1 $1) $2 $4 }
           | ExprPred2 { $1 }
           
ExprPred2 :: { ExprPredicate }
ExprPred2 : ExprPred2 'equiv' ExprPred3 { AbsIsoz.EPEquiv (getAnnTermAndRemoveExprInPars1 $1) $1 $3 }
          | ExprPred3 { $1 }

ExprPred3 :: { ExprPredicate }
ExprPred3 : ExprPred4 'implies' ExprPred3 { AbsIsoz.EPImpl (getAnnTermAndRemoveExprInPars1 $1) $1 $3 }
          | ExprPred4 { $1 }

ExprPred4 :: { ExprPredicate }
ExprPred4 : ExprPred4 'lor' ExprPred5 { AbsIsoz.EPOr (getAnnTermAndRemoveExprInPars1 $1) $1 $3 }
      | ExprPred5 { $1 }

ExprPred5 :: { ExprPredicate }
ExprPred5 : ExprPred5 'land' ExprPred6 { AbsIsoz.EPAnd (getAnnTermAndRemoveExprInPars1 $1) $1 $3 }
          | ExprPred6 { $1 }

ExprPred6 :: { ExprPredicate }
ExprPred6 : 'lnot' ExprPred6 { AbsIsoz.EPNeg (getAnnTermAndRemoveExprInPars1 $1) $2 }
          | ExprPred6_7_0 { $1 }

ExprPred6_7_0 :: { ExprPredicate }
ExprPred6_7_0 : L_PREP ExprPred6_7 { AbsIsoz.EPRel (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.PrefixRel (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.EPPrePPrefixRel (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) $2)) }
      | L_LP1 ExpSep EREPSREP_E1 ExprPred6_7 { AbsIsoz.EPRel (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.PrefixRel (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.EPLPPrefixRel (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (reverse $2) $3 $4)) }
      | ExprPred6_7 L_POSTP { AbsIsoz.EPRel (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.PostfixRel (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.EPPostPPostfixRel (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2))) }
      | ExprPred6_7 L_ELP2 ExpSep ERPSRP_E2 { AbsIsoz.EPRel (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.PostfixRel (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.EPELPPostfixRel (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) (reverse $3) $4)) }
      | ExprPred6_7 ListIP_E {AbsIsoz.EPRel (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.InfixRel (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.EPInInfixRel (getAnnTermAndRemoveExprInPars1 $1) $1 (reverse $2))) }
      | ExprPred6_7 L_ELP3 ExpSep EREPSREP_E3 ExprPred6_7 { AbsIsoz.EPRel (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.InfixRel (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.EPELPInfixRel (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) (reverse $3) $4 $5)) }
      | L_LP4 ExpSep ERPSRP_E4 { AbsIsoz.EPRel (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.NofixRel (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.LPNofixRel (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (reverse $2) $3)) }
      | ExprPred6_7 { $1 }

ExprPred6_7 :: { ExprPredicate }
ExprPred6_7 : ExprPred7 { $1 }

ExprPred7 :: { ExprPredicate }
ExprPred7 : 'lambda' SchemaText 'spot' ExprPred8 { AbsIsoz.EPLambda (getAnnTermAndRemoveExprInPars1 $1) $2 $4 }
          | ExprPred8 { $1 }

ExprPred8 :: { ExprPredicate }
ExprPred8 : 'mu' SchemaText 'spot' ExprPred9 { AbsIsoz.EPMu (getAnnTermAndRemoveExprInPars1 $1) $2 $4 }
          | ExprPred9 { $1 }

ExprPred9 :: { ExprPredicate }
ExprPred9 : 'let' ListLocalDecl 'spot' ExprPred10 { AbsIsoz.EPLocalDef (getAnnTermAndRemoveExprInPars1 $1) (reverse $2) $4 }
          | ExprPred10 { $1 }

ExprPred10 :: { ExprPredicate }
ExprPred10 : 'if' ExprPredicate 'then' ExprPredicate 'else' ExprPredicate { AbsIsoz.EPCond (getAnnTermAndRemoveExprInPars1 $1) $2 $4 $6 }
           | ExprPred11 { $1 }

ExprPred11 :: { ExprPredicate }
ExprPred11 : ExprPred11 'compose' ExprPred12 { AbsIsoz.EPComp (getAnnTermAndRemoveExprInPars1 $1) $1 $3 } 
           | ExprPred12 { $1 }

ExprPred12 :: { ExprPredicate }
ExprPred12 : ExprPred12 'pipe' ExprPred13 { AbsIsoz.EPPipe (getAnnTermAndRemoveExprInPars1 $1) $1 $3 } 
           | ExprPred13 { $1 }

ExprPred13 :: { ExprPredicate }
ExprPred13 : ExprPred13 'hide' L_LPARENT ListDeclName L_RPARENT { AbsIsoz.EPHide (getAnnTermAndRemoveExprInPars1 $1) $1 (reverse $4) }
           | ExprPred14 { $1 }

ExprPred14 :: { ExprPredicate }
ExprPred14 : ExprPred14 'project' ExprPred15 { AbsIsoz.EPProj (getAnnTermAndRemoveExprInPars1 $1) $1 $3}
           | ExprPred15 { $1 }

ExprPred15 :: { ExprPredicate }
ExprPred15 : 'pre' ExprPred16 { AbsIsoz.EPPre (getAnnTermAndRemoveExprInPars1 $1) $2 }
           | ExprPred16 { $1 }

ExprPred16 :: { ExprPredicate }
ExprPred16 : ExprPred16 'cross' ExprPred17 -- { AbsIsoz.EPProd (getAnnTermAndRemoveExprInPars (addAnnTerm (ParserParDepthAnn n) $1)) [$1, $3]}
                            {% do n <- getParserParDepth;
                                  returnA (AbsIsoz.EPProd (getAnnTermAndRemoveExprInPars (addAnnTerm (ParserParDepthAnn n) $1)) [$1, $3])
                            }
           | ExprPred16 L_I ExprPred17 { AbsIsoz.EPFuncAppl (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.InfixApp (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.EPInInfixApp (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) $3)) }
           | ExprPred16 L_EL3 ExpSep ERESRE_E3 ExprPred17 { AbsIsoz.EPFuncAppl (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.InfixApp (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.EPELInfixApp (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) (reverse $3) $4 $5)) }
           | ExprPred17 { $1 }

ExprPred17 :: { ExprPredicate }
ExprPred17 : 'powerset' ExprPred17 { AbsIsoz.EPPower (getAnnTermAndRemoveExprInPars1 $1) $2 } 
           | L_PRE ExprPred17 { AbsIsoz.EPFuncAppl (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.PrefixApp (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.EPPrePrefixApp (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) $2)) }
           | L_L1 ExpSep ERESRE_E1 ExprPred17 { AbsIsoz.EPFuncAppl (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.PrefixApp (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.EPLPrefixApp (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (reverse $2) $3 $4)) }
           | ExprPred18 { $1 }

ExprPred18 :: { ExprPredicate }
ExprPred18 : ExprPred18 L_POST { AbsIsoz.EPFuncAppl (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.PostfixApp (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.EPPostPostfixApp (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2))) }
           | ExprPred18 L_EL2 ExpSep ERSR_E2 { AbsIsoz.EPFuncAppl (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.PostfixApp (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.EPELPostfixApp (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) (reverse $3) $4)) }
           | ExprPred19 { $1 }

ExprPred19 :: { ExprPredicate }
ExprPred19 : ExprPred19 ExprPred191 { AbsIsoz.EPAppl (getAnnTermAndRemoveExprInPars1 $1) $1 $2 }
           | ExprPred191 { $1 }

ExprPred191 :: { ExprPredicate }
ExprPred191 : L_L4 ExpSep ERSR_E4 { AbsIsoz.EPFuncAppl (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.NofixApp (getAnnTermAndRemoveExprInPars1 $1) (AbsIsoz.EPLNofixApp (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (reverse $2) $3)) }
            | ExprPred20 { $1 }

ExprPred20 :: { ExprPredicate }
ExprPred20 : ExprPred20 L_STROKE { AbsIsoz.EPDecor (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) }
           | ExprPred21 { $1 }

ExprPred21 :: { ExprPredicate }
ExprPred21 : ExprPred21 L_LBRACKET3 ListRenamePair L_RBRACKET { AbsIsoz.EPRename (getAnnTermAndRemoveExprInPars1 $1) $1 (reverse $3) }
           | ExprPred22 { $1 }

ExprPred22 :: { ExprPredicate }
ExprPred22 : ExprPred23 '.' RefName { AbsIsoz.EPBindSel (getAnnTermAndRemoveExprInPars1 $1) $1 $3 }
           | ExprPred23 '.' L_NUMERAL { AbsIsoz.EPTupleSel (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $3) }
           | ExprPred23 { $1 }

ExprPred23 :: { ExprPredicate }
ExprPred23 : 'theta' ExprPred24 ListStroke { AbsIsoz.EPTheta (getAnnTermAndRemoveExprInPars1 $1) $2 (reverse $3) }
           | ExprPred24 { $1 }

ExprPred24 :: { ExprPredicate }
ExprPred24 : RefName { AbsIsoz.EPRef (getAnnTermAndRemoveExprInPars1 $1) $1}
           | RefName L_LBRACKET ListExprPredicate L_RBRACKET { AbsIsoz.EPGenRef (getAnnTermAndRemoveExprInPars1 $1) $1 (reverse $3) }
           | L_NUMERAL { AbsIsoz.EPNum (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) } 
           | 'true' { AbsIsoz.EPTrue (getAnnTermAndRemoveExprInPars1 $1) }
           | 'false' { AbsIsoz.EPFalse (getAnnTermAndRemoveExprInPars1 $1) }
           | ExprPred25 { $1 }

ExprPred25 :: { ExprPredicate }
ExprPred25 : L_LBRACE L_RBRACE { AbsIsoz.EPSet (getAnnTermAndRemoveExprInPars1 $1) [] }
           | ExprPred26 { $1 }

ExprPred26 :: { ExprPredicate }
ExprPred26 : L_LBRACE ListExprPredicate L_RBRACE { AbsIsoz.EPSet (getAnnTermAndRemoveExprInPars1 $1) (reverse $2) }
           | ExprPred27 { $1 }

ExprPred27 :: { ExprPredicate }
ExprPred27 : L_LBRACE SchemaText 'spot' ExprPredicate L_RBRACE { AbsIsoz.EPSetComp (getAnnTermAndRemoveExprInPars1 $1) $2 $4 }
           -- Characteristic set compression (exclude empty set and { expr })
           -- Also means ({ x }) should be parsed as singleton set, and not CharSetCompExpr
           -- This has been implemented in the later stage through syntactic transformation of CharSetCompExpr in SynTransformerOne
           | L_LBRACE SchemaText L_RBRACE { AbsIsoz.EPCharSetComp (getAnnTermAndRemoveExprInPars1 $1) $2 }
           | ExprPred28 { $1 }

ExprPred28 :: { ExprPredicate }
ExprPred28 : L_LBRACKET SchemaText L_RBRACKET { AbsIsoz.EPSchema (getAnnTermAndRemoveExprInPars1 $1) $2 }
           | ExprPred29 { $1 }

ExprPred29 :: { ExprPredicate }
ExprPred29 : L_LBLOT L_RBLOT { AbsIsoz.EPBindExt (getAnnTermAndRemoveExprInPars1 $1) [] }
           | L_LBLOT ListLocalDecl L_RBLOT { AbsIsoz.EPBindExt (getAnnTermAndRemoveExprInPars1 $1) (reverse $2) }
           | ExprPred30 { $1 }

ExprPred30 :: { ExprPredicate }
ExprPred30 : L_LPARENT ExprPredicate ',' ListExprPredicate L_RPARENT { AbsIsoz.EPTupleExt (getAnnTermAndRemoveExprInPars1 $1) $2 (reverse $4) }
           | ExprPred31 { $1 }

ExprPred31 :: { ExprPredicate }
ExprPred31 : L_LPARENT 'mu' SchemaText L_RPARENT { AbsIsoz.EPCharMu (getAnnTermAndRemoveExprInPars1 $1) $3 }
           | ExprPred32 { $1 }

ExprPred32 :: { ExprPredicate }
ExprPred32 : L_LPARENT ExprPredicate L_RPARENT     
                { % do n <- getParserParDepth; setParserParDepth (n+1);
                       returnA (addAnnTerm ExprInParAnn $2)
                }

ListExprPredicate :: { [ExprPredicate] }
ListExprPredicate : --{- empty -}  { [] }
                 ExprPredicate  { [ $1 ] }
               | ListExprPredicate ',' ExprPredicate { flip (:) $1 $3 }
-------------------------------------------------------
ListRenamePair :: { [ RenamePair ] }
ListRenamePair : RenamePair {[$1]}
               | ListRenamePair ',' RenamePair { flip (:) $1 $3 }

RenamePair :: { RenamePair } 
RenamePair : DeclName '/' DeclName { AbsIsoz.RenamePair (getAnnTermAndRemoveExprInPars1 $1) $3 $1 }

ListStroke :: { [String] }
ListStroke : L_STROKE ListStroke { flip (:) $2 (prToken $1) }
           | {- empty -} { [] }

SchemaText :: { SchemaText }
SchemaText : {- empty -} { AbsIsoz.SchemaTextEmpty [] }
           | DeclPart { AbsIsoz.SchemaTextDecl (getAnnTermAndRemoveExprInPars1 $1) $1 }
-- clause 12.2.7
           | '|' ExprPredicate_P { AbsIsoz.EPSchemaTextPred (getAnnTermAndRemoveExprInPars1 $1) $2 }
           | DeclPart '|' ExprPredicate_P { AbsIsoz.EPSchemaText (getAnnTermAndRemoveExprInPars1 $1) $1 $3 }

DeclPart :: { DeclPart }
DeclPart : ListDecl { AbsIsoz.DeclPart (getAnnTermAndRemoveExprInPars1 $1) ($1) } 

ListLocalDecl :: { [Declaration] }
ListLocalDecl : LocalDecl { [$1] }
              | ListLocalDecl ';' LocalDecl { flip (:) $1 $3 }

ListDecl :: { [Declaration] }
ListDecl : Declaration { [$1] }
         | Declaration ';' ListDecl { (:) $1 (reverse $3) }
         | Declaration L_NL ListDecl { (:) $1 (reverse $3) }

ListDecl1 :: { [Declaration] }
ListDecl1 : Declaration1 { [$1] }
         | ListDecl1 ';' Declaration1 { flip (:) $1 $3 }
         | ListDecl1 L_NL Declaration1 { flip (:) $1 $3 }

LocalDecl :: { Declaration }
LocalDecl : DeclName '==' ExprPredicate { AbsIsoz.EPAbbrDecl (getAnnTermAndRemoveExprInPars1 $1) $1 $3 } 

Declaration1 :: { Declaration }
Declaration1 : ListDeclName ':' ExprPredicate { AbsIsoz.EPDeclaration (getAnnTermAndRemoveExprInPars1 $1) (reverse $1) $3 }
            | DeclName '==' ExprPredicate { AbsIsoz.EPAbbrDecl (getAnnTermAndRemoveExprInPars1 $1) $1 $3 } 
-- Expression in Declaration is not supported yet 
            | ExprPredicate { AbsIsoz.EPExprDecl (getAnnTermAndRemoveExprInPars1 $1) $1 } 

Declaration :: { Declaration } 
Declaration : L_START ListDeclName ':' ExprPredicate { AbsIsoz.EPDeclaration (getAnnTermAndRemoveExprInPars1 $2) (reverse $2) $4 }
            | DeclName ':' ExprPredicate { AbsIsoz.EPDeclaration (getAnnTermAndRemoveExprInPars1 $1) [$1] $3 }
            | DeclName '==' ExprPredicate { AbsIsoz.EPAbbrDecl (getAnnTermAndRemoveExprInPars1 $1) $1 $3 } 
-- Expression in Declaration is not supported yet 
            | ExprPredicate { AbsIsoz.EPExprDecl (getAnnTermAndRemoveExprInPars1 $1) $1 } 

OperatorTemplate :: { OperatorTemplate }
OperatorTemplate : 'relation' Template 
                        {% do {m <- getOpTable; setOpTable (Map.insert (getOpNameFromTemplate $2) "relation" m); returnA (AbsIsoz.RelationTemplate (getAnnTermAndRemoveExprInPars1 $1) $2)}
                        }
                 | 'function' CategoryTemplate 
                        {% do {m <- getOpTable; setOpTable (Map.insert (getOpNameFromCatTemplate $2) ("function:" ++ (getPrec $2) ++ ":" ++ (getAssoc $2)) m); returnA (AbsIsoz.FunctionTemplate (getAnnTermAndRemoveExprInPars1 $1) $2)}
                        } 
                 | 'generic' CategoryTemplate
                        {% do {m <- getOpTable; setOpTable (Map.insert (getOpNameFromCatTemplate $2) ("generic:" ++ (getPrec $2) ++ ":" ++ (getAssoc $2)) m); returnA (AbsIsoz.GenericTemplate (getAnnTermAndRemoveExprInPars1 $1) $2)}
                        }

CategoryTemplate :: { CategoryTemplate }
CategoryTemplate : PrefixTemplate { AbsIsoz.PrefixCatTemplate (getAnnTermAndRemoveExprInPars1 $1) $1 }
                 | PostfixTemplate { AbsIsoz.PostfixCatTemplate (getAnnTermAndRemoveExprInPars1 $1) $1 }
                 | Prec Assoc InfixTemplate { AbsIsoz.InfixCatTemplate (getAnnTermAndRemoveExprInPars1 $1) $1 $2 $3 }
                 | NofixTemplate { AbsIsoz.NofixCatTemplate (getAnnTermAndRemoveExprInPars1 $1) $1 }

Prec :: { Prec }
Prec : L_NUMERAL { AbsIsoz.Prec (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) }

Assoc :: { Assoc }
Assoc : 'leftassoc' { AbsIsoz.LeftAssoc (getAnnTermAndRemoveExprInPars1 $1) }
      | 'rightassoc' { AbsIsoz.RightAssoc (getAnnTermAndRemoveExprInPars1 $1) }

Template :: { Template }
Template : PrefixTemplate { AbsIsoz.TPrefixTemplate (getAnnTermAndRemoveExprInPars1 $1) $1 }
         | PostfixTemplate { AbsIsoz.TPostfixTemplate (getAnnTermAndRemoveExprInPars1 $1) $1 }
         | InfixTemplate { AbsIsoz.TInfixTemplate (getAnnTermAndRemoveExprInPars1 $1) $1 }
         | NofixTemplate  { AbsIsoz.TNofixTemplate (getAnnTermAndRemoveExprInPars1 $1) $1 }

PrefixTemplate :: { PrefixTemplate }
PrefixTemplate : L_LPARENT PrefixName L_RPARENT { AbsIsoz.PrefixTemplate (getAnnTermAndRemoveExprInPars1 $1) $2 }
               | L_LPARENT 'powerset' '_' L_RPARENT { AbsIsoz.PowerPrefixTemplate (getAnnTermAndRemoveExprInPars1 $1) }

PostfixTemplate :: { PostfixTemplate }
PostfixTemplate : L_LPARENT PostfixName L_RPARENT { AbsIsoz.PostfixTemplate (getAnnTermAndRemoveExprInPars1 $1) $2 }

InfixTemplate :: { InfixTemplate }
InfixTemplate : L_LPARENT InfixName L_RPARENT { AbsIsoz.InfixTemplate (getAnnTermAndRemoveExprInPars1 $1) $2 }

NofixTemplate :: { NofixTemplate }
NofixTemplate : L_LPARENT NofixName L_RPARENT { AbsIsoz.NofixTemplate (getAnnTermAndRemoveExprInPars1 $1) $2 }

ListDeclName :: { [DeclName] }
ListDeclName : DeclName { [$1] }
             | ListDeclName ',' DeclName { flip (:) $1 $3 }
        
DeclName :: { DeclName }
DeclName : L_NAME { AbsIsoz.DName (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) }
         | OpName { AbsIsoz.OpName (getAnnTermAndRemoveExprInPars1 $1) $1 }

RefName :: { RefName }
RefName : L_NAME { AbsIsoz.RName (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) }
        | L_LPARENT OpName L_RPARENT { AbsIsoz.OpRName (getAnnTermAndRemoveExprInPars1 $1) $2 }

OpName :: { OpName }
OpName : PrefixName { AbsIsoz.PrefixOpName (getAnnTermAndRemoveExprInPars1 $1) $1 } 
       | PostfixName { AbsIsoz.PostfixOpName (getAnnTermAndRemoveExprInPars1 $1) $1 } 
       | InfixName { AbsIsoz.InfixOpName (getAnnTermAndRemoveExprInPars1 $1) $1 }
       | NofixName { AbsIsoz.NofixOpName (getAnnTermAndRemoveExprInPars1 $1) $1 }

ListESSS :: { [ ESSS ] }
ListESSS : {- empty -} { [] } 
          | ListESSS ESSS { flip (:) $1 $2 }

ESSS :: { ESSS }
ESSS : '_' L_ES { AbsIsoz.ES (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }
     | ',,' L_SS { AbsIsoz.SS (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }

-- for NAME instead of _
ListESSS_N :: { [ ESSS_N ] }
ListESSS_N : {- empty -} { [] } 
           | ListESSS_N ESSS_N { flip (:) $1 $2 }

ESSS_N :: { ESSS_N }
ESSS_N : L_NAME L_ES { AbsIsoz.ES_N (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (prToken $2) }
       | L_NAME L_SS { AbsIsoz.SS_N (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (prToken $2) }

ESSS_E :: { ESSS_E }
ESSS_E : ExprPredicate L_ES     { AbsIsoz.EPES_E (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) }
       | ExprPredicateList L_SS { AbsIsoz.EPSS_E (getAnnTermAndRemoveExprInPars1 $1) (reverse $1) (prToken $2) }

ERESRE1 :: { ERESRE }
ERESRE1 : '_' L_ERE1 { AbsIsoz.ERE (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }
       | ',,' L_SRE1 { AbsIsoz.SRE (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }

ERESRE3 :: { ERESRE }
ERESRE3 : '_' L_ERE3 { AbsIsoz.ERE (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }
       | ',,' L_SRE3 { AbsIsoz.SRE (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }

ERESRE_N1 :: { ERESRE_N }
ERESRE_N1 : L_NAME L_ERE1 { AbsIsoz.ERE_N (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (prToken $2) }
         | L_NAME L_SRE1 { AbsIsoz.SRE_N (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (prToken $2) }

ERESRE_N3 :: { ERESRE_N }
ERESRE_N3 : L_NAME L_ERE3 { AbsIsoz.ERE_N (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (prToken $2) }
         | L_NAME L_SRE3 { AbsIsoz.SRE_N (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (prToken $2) }

ERESRE_E1 :: { ERESRE_E }
ERESRE_E1 : ExprPredicate L_ERE1 { AbsIsoz.EPERE_E (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) }
         | ExprPredicateList L_SRE1 { AbsIsoz.EPSRE_E (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) }

ERESRE_E3 :: { ERESRE_E }
ERESRE_E3 : ExprPredicate L_ERE3 { AbsIsoz.EPERE_E (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) }
         | ExprPredicateList L_SRE3 { AbsIsoz.EPSRE_E (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) }

EREPSREP1 :: { EREPSREP }
EREPSREP1 : '_' L_EREP1 { AbsIsoz.EREP (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }
         | ',,' L_SREP1 { AbsIsoz.SREP (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }

EREPSREP3 :: { EREPSREP }
EREPSREP3 : '_' L_EREP3 { AbsIsoz.EREP (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }
         | ',,' L_SREP3 { AbsIsoz.SREP (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }

EREPSREP_E1 :: { EREPSREP_E }
EREPSREP_E1 : ExprPredicate L_EREP1 { AbsIsoz.EPEREP_E (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) }
           | ExprPredicateList L_SREP1 { AbsIsoz.EPSREP_E (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) }

EREPSREP_E3 :: { EREPSREP_E }
EREPSREP_E3 : ExprPredicate L_EREP3 { AbsIsoz.EPEREP_E (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) }
           | ExprPredicateList L_SREP3 { AbsIsoz.EPSREP_E (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) }

ERSR2 :: { ERSR }
ERSR2 : '_' L_ER2 { AbsIsoz.ER (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }
     | ',,' L_SR2 { AbsIsoz.SR (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }

ERSR4 :: { ERSR }
ERSR4 : '_' L_ER4 { AbsIsoz.ER (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }
     | ',,' L_SR4 { AbsIsoz.SR (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }

ERSR_N2 :: { ERSR_N }
ERSR_N2 : L_NAME L_ER2 { AbsIsoz.ER_N (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (prToken $2) }
       | L_NAME L_SR2 { AbsIsoz.SR_N (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (prToken $2) }

ERSR_N4 :: { ERSR_N }
ERSR_N4 : L_NAME L_ER4 { AbsIsoz.ER_N (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (prToken $2) }
       | L_NAME L_SR4 { AbsIsoz.SR_N (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (prToken $2) }

ERSR_E2 :: { ERSR_E }
ERSR_E2 : ExprPredicate L_ER2 { AbsIsoz.EPER_E (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) }
       | ExprPredicateList L_SR2 { AbsIsoz.EPSR_E (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) }

ERSR_E4 :: { ERSR_E }
ERSR_E4 : ExprPredicate L_ER4 { AbsIsoz.EPER_E (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) }
       | ExprPredicateList L_SR4 { AbsIsoz.EPSR_E (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) }

ERPSRP2 :: { ERPSRP }
ERPSRP2 : '_' L_ERP2 { AbsIsoz.ERP (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }
       | ',,' L_SRP2 { AbsIsoz.SRP (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }

ERPSRP4 :: { ERPSRP }
ERPSRP4 : '_' L_ERP4 { AbsIsoz.ERP (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }
       | ',,' L_SRP4 { AbsIsoz.SRP (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }

ERPSRP_E2 :: { ERPSRP_E }
ERPSRP_E2 : ExprPredicate L_ERP2 { AbsIsoz.EPERP_E (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) }
         | ExprPredicateList L_SRP2 { AbsIsoz.EPSRP_E (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) }

ERPSRP_E4 :: { ERPSRP_E }
ERPSRP_E4 : ExprPredicate L_ERP4 { AbsIsoz.EPERP_E (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) }
         | ExprPredicateList L_SRP4 { AbsIsoz.EPSRP_E (getAnnTermAndRemoveExprInPars1 $1) $1 (prToken $2) }

PrefixName :: { PrefixName }
PrefixName : L_PRE '_' { AbsIsoz.PrePrefixName (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) } 
           | L_PREP '_' { AbsIsoz.PrePPrefixName (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) }
           | L_L1 ListESSS ERESRE1 '_' { AbsIsoz.LPrefixName (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (reverse $2) $3 }
           | L_LP1 ListESSS EREPSREP1 '_' { AbsIsoz.LPPrefixName (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (reverse $2) $3 }

PostfixName :: { PostfixName }
PostfixName : '_' L_POST { AbsIsoz.PostPostfixName (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }
            | '_' L_POSTP { AbsIsoz.PostPPostfixName (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }
            | '_' L_EL2 ListESSS ERSR2 { AbsIsoz.ELPostfixName (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) (reverse $3) $4 }
            | '_' L_ELP2 ListESSS ERPSRP2 { AbsIsoz.ELPPostfixName (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) (reverse $3) $4 }

InfixName :: { InfixName }
InfixName : '_' L_I '_' { AbsIsoz.InInfixName (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }
          | '_' L_IP '_' { AbsIsoz.InPInfixName (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) }
          | '_' L_EL3 ListESSS ERESRE3 '_' { AbsIsoz.ELInfixName (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) (reverse $3) $4 }
          | '_' L_ELP3 ListESSS EREPSREP3 '_' { AbsIsoz.ELPInfixName (getAnnTermAndRemoveExprInPars1 $1) (prToken $2) (reverse $3) $4 }

NofixName :: { NofixName }
NofixName : L_L4 ListESSS ERSR4 { AbsIsoz.LNofixName (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (reverse $2) $3 }
          | L_LP4 ListESSS ERPSRP4 { AbsIsoz.LPNofixName (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (reverse $2) $3 }

GenName :: { GenName }
GenName : PrefixGenName { AbsIsoz.PrefixGenName (getAnnTermAndRemoveExprInPars1 $1) $1 }
        | PostfixGenName { AbsIsoz.PostfixGenName (getAnnTermAndRemoveExprInPars1 $1) $1 }
        | InfixGenName { AbsIsoz.InfixGenName (getAnnTermAndRemoveExprInPars1 $1) $1 }
        | NofixGenName { AbsIsoz.NofixGenName (getAnnTermAndRemoveExprInPars1 $1) $1 }

PrefixGenName :: { PrefixGenName }
PrefixGenName : L_PRE L_NAME { AbsIsoz.PrePrefixGenName (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (prToken $2) }
              | L_L1 ListESSS_N ERESRE_N1 L_NAME { AbsIsoz.LPrefixGenName (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (reverse $2) $3 (prToken $4) }

PostfixGenName :: { PostfixGenName }
PostfixGenName : L_NAME L_POST { AbsIsoz.PostPostfixGenName (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (prToken $2) }
            | L_NAME L_EL2 ListESSS_N ERSR_N2 { AbsIsoz.ELPostfixGenName (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (prToken $2) (reverse $3) $4 }

InfixGenName :: { InfixGenName }
InfixGenName : L_NAME L_I L_NAME { AbsIsoz.InInfixGenName (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (prToken $2) (prToken $3) }
             | L_NAME L_EL3 ListESSS_N ERESRE_N3 L_NAME { AbsIsoz.ELInfixGenName (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (prToken $2) (reverse $3) $4 (prToken $5) }

NofixGenName :: { NofixGenName }
NofixGenName : L_L4 ListESSS_N ERSR_N4 { AbsIsoz.LNofixGenName (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) (reverse $2) $3 }

IP_E :: { IP_E }
IP_E : 'in' ExprPred6_7 { AbsIsoz.EPIP_E (getAnnTermAndRemoveExprInPars1 $1) "\8712" $2 } 
     | '=' ExprPred6_7 { AbsIsoz.EPIP_E (getAnnTermAndRemoveExprInPars1 $1) "=" $2 }
     | L_IP ExprPred6_7 { AbsIsoz.EPIP_E (getAnnTermAndRemoveExprInPars1 $1) (prToken $1) $2 }

ListIP_E :: {[IP_E]}
ListIP_E : IP_E { [$1] }
         | ListIP_E IP_E { flip (:) $1 $2 }

ExpSep :: { [ ESSS_E ] }
ExpSep : {- empty -} { [] } 
       | ExpSep ESSS_E { flip (:) $1 $2 }

ExprPredicateList :: { [ExprPredicate] }
ExprPredicateList : {- empty -} { [] } -- is empty to represent it is optional by [ Expression, { ,-tok , Expression } ]
               | ExprPredicate  { [ $1 ] }
               | ExprPredicateList ',' ExprPredicate { flip (:) $1 $3 }

{

returnA :: a -> Alex a
returnA = return

thenA :: Alex a -> (a -> Alex b) -> Alex b
thenA = (>>=)

happyError :: [Token] -> Alex a
happyError ts = alexError (err2Msg (ErrParser (EParseSyntaxError (tokenPos ts ++ case ts of
                [] -> []
                [Err _] -> " due to lexer error"
                _ -> " before '" ++ unwords (map (id . prToken) (take 4 ts)) ++ "'"
        ))))

myLexer = scanner 

-- | add a schema name to the map and then it can be used later to check if a name is a schema or not
setToSchemaExpr :: String -> Alex Bool 
setToSchemaExpr name = update
    where update = do
            m <- getSchemaExprMap
            setSchemaExprMap (Map.insert name True m)
            return True 

-- | check if a name is a schema or not
-- 
-- if it is a name prefixing with \Delta or \Xi, they should be removed in advance before checking
isASchema :: String -> Alex Bool
isASchema name = do
        m <- getSchemaExprMap
        case Map.lookup (realName name) m of 
            Just True -> return True 
            _         -> return False 
  -- remove prefixing Xi or Delta
  where realName ('\x0394': xs) = xs
        realName ('\x039E': xs) = xs
        realName s = s

-- | check if both expressions are schema expressions 
andSchemaExprTrue :: ExprPredicate -> ExprPredicate -> Alex Bool
andSchemaExprTrue e1 e2 = do r1 <- isSchemaExpr e1
                             r2 <- isSchemaExpr e2
                             case r1 of
                                True -> case r2 of
                                        True -> return True 
                                        False -> return False
                                False -> return False

-- | check if an expression predicate is a schema expression
isSchemaExpr :: ExprPredicate -> Alex Bool
isSchemaExpr (EPForall _ _ e) = ( isSchemaExpr e )
isSchemaExpr (EPExists _ _ e) = ( isSchemaExpr e )
isSchemaExpr (EPExists1 _ _ e) = ( isSchemaExpr e ) 
isSchemaExpr (EPEquiv _ e1 e2) = andSchemaExprTrue e1 e2 
isSchemaExpr (EPImpl _ e1 e2) = andSchemaExprTrue e1 e2
isSchemaExpr (EPAnd _ e1 e2) = andSchemaExprTrue e1 e2
isSchemaExpr (EPOr _ e1 e2) = andSchemaExprTrue e1 e2
isSchemaExpr (EPNeg _ e) = ( isSchemaExpr e)
isSchemaExpr (EPMu _ _ e) = ( isSchemaExpr e)
isSchemaExpr (EPLocalDef _ _ e) = isSchemaExpr e
isSchemaExpr (EPCond _ _ e1 e2 ) = andSchemaExprTrue e1 e2
isSchemaExpr (EPComp _ e1 e2  ) = andSchemaExprTrue e1 e2
isSchemaExpr (EPPipe _ e1 e2  ) = andSchemaExprTrue e1 e2
isSchemaExpr (EPHide _ e _) = ( isSchemaExpr e )
isSchemaExpr (EPProj _ e1 e2  ) = andSchemaExprTrue e1 e2
isSchemaExpr (EPPre _ e) = ( isSchemaExpr e)
isSchemaExpr (EPDecor _ e _) = ( isSchemaExpr e )
isSchemaExpr (EPRename _ e _) = ( isSchemaExpr e ) 
isSchemaExpr (EPTheta _ e _) = ( isSchemaExpr e )
isSchemaExpr (EPRef _ (RName _ r)) = isASchema r 
isSchemaExpr (EPGenRef _ (RName _ r) _) = isASchema r 
isSchemaExpr (EPSchema _ _) = return ( True)
isSchemaExpr (EPBindExt _ _) = return ( True)  
isSchemaExpr _ = return ( False )

-- | print an error indicating where a schema expression is expected but it is not a schema expression
notASchemaError :: Token -> String -> Alex a 
notASchemaError (PT pos _ (Just s)) msg = alexError ("[" ++ s ++ "] at " ++ showPosn pos ++ " is not a SchemaExpr [Expected " ++ "in " ++ msg)

{- |
 - rewrite CSExp according to precedences and associativities of union, intersection and difference.
 - in each loop, we just exchange one adjacent operators. 
 - and finally, if there is no more exchanges (the tree before correction is equal to the tree after correction)
 -  then terminate and return
 -}
rewriteCSExp :: CSExp -> Alex (Either String CSExp)
rewriteCSExp cs = loop cs
    where loop cs = do {r <- correctAdjNodes cs;
                case r of 
                    Left err -> returnA (Left err)
                    Right cs' -> if (cs == cs')
                                 then returnA (Right (cs')) 
                                 else loop cs'
            } 
{-
 - correct adjacent nodes in the tree 
 -  (reference: "Handling Operator Precedence in Arithmetic Expressions with Tree Transformations")
 - assume: 1) all operators are left associative and infix operators (that's the case for CSExp) 
 _
 - the rule: 
 -  1. the original tree is left-associative 
 -       therefore, (e1 \ e2 ∩ e3 ∪ e4) => (((e1 \ e2) ∩ e3) ∪ e4)
 -  2. traverse the tree from root (r) to its left subtree always. And for each step, apply the rule below
 -  3. suppose the current node (p) has the precedence (p_prec),
 -             its left subnode (q) has the precedence (q_prec), 
 -     1) if (p_prec == q_prec), no change of p and q, and move forward to q node 
 -     2) if (p_prec < q_prec), no change of p and q, and move forward to q node 
 -     3) if (p_prec > q_prec), a strongly bound operator shall not be a parent of another not so strong operator.
 -        q is the anchor node, move p to be the right subtree of q, then return (not move forward anymore since we just correct one step at one)
 -  
 -  For example, ((e1 \ e2) ∩ e3) is expressed as (∩ (\ e1 e2) e3)
 -      because the precedence of ∩ is larger than that of \, p_prec > q_prec, so finally
 -      (∩ (\ e1 e2) e3) => (\ e1 (∩ e2 e3)) => (e1 \ (e2 ∩ e3))
 -}
correctAdjNodes :: CSExp -> Alex (Either String CSExp)
correctAdjNodes s@(CSExpEmpty _) = returnA (Right s)
correctAdjNodes s@(CSExpExt _ _) = returnA (Right s)
correctAdjNodes s@(CSExpRef _ _) = returnA (Right s)
correctAdjNodes s@(CSExpUnion ann e1 e2) = case e1 of
                    CSExpEmpty _ -> returnA (Right s) 
                    CSExpExt _ _ -> returnA (Right s) 
                    CSExpRef _ _ -> returnA (Right s) 
                    -- adjacent notes have the same operator, so continue to move to its left
                    CSExpUnion ann' e1' e2' -> do { rr <- correctAdjNodes e1; 
                                         case rr of
                                            Left err -> returnA (Left err)
                                            Right e1'' -> returnA (Right (CSExpUnion ann e1'' e2)) 
                                           }
                    CSExpInter ann' e1' e2' -> do { r <- precCompare zChar_union zChar_inter; 
                            case r of
                                Left err -> returnA (Left err) 
                                Right r' -> do {if r' == 1 
                                                -- if it is necessary to exchange two nodes, just correct them and then return
                                                then returnA (Right (CSExpInter ann e1' (CSExpUnion ann' e2' e2))) 
                                                -- if it is not necessary to exchange two nodes, just continue to move to its left subtree
                                                else do { rr <- correctAdjNodes e1; 
                                                    case rr of
                                                         Left err -> returnA (Left err)
                                                         Right e1'' -> returnA (Right (CSExpUnion ann e1'' e2)) 
                                                        }
                                               }
                            }
                    CSExpDiff ann' e1' e2' -> do { r <- precCompare zChar_union zChar_diff; 
                            case r of
                                Left err -> returnA (Left err)
                                Right r' -> do {if r' == 1 
                                                then returnA (Right (CSExpDiff ann e1' (CSExpUnion ann' e2' e2) )) 
                                                else do { rr <- correctAdjNodes e1; 
                                                    case rr of
                                                         Left err -> returnA (Left err)
                                                         Right e1'' -> returnA (Right (CSExpUnion ann e1'' e2)) 
                                                        }
                                                }
                            }
correctAdjNodes s@(CSExpInter ann e1 e2) = case e1 of 
                    CSExpEmpty _ -> returnA (Right s) 
                    CSExpExt _ _ -> returnA (Right s) 
                    CSExpRef _ _ -> returnA (Right s) 
                    -- adjacent notes have the same operator, so continue to move to its left
                    CSExpInter _ _ _ -> do { rr <- correctAdjNodes e1; 
                                         case rr of
                                            Left err -> returnA (Left err)
                                            Right e1'' -> returnA (Right (CSExpInter ann e1'' e2)) 
                                           }
                    CSExpUnion ann' e1' e2' -> do { r <- precCompare zChar_inter zChar_union; 
                            case r of
                                Left err -> returnA (Left err)
                                Right r' -> do {if r' == 1 
                                                then returnA (Right (CSExpUnion ann e1' (CSExpInter ann' e2' e2))) 
                                                else do { rr <- correctAdjNodes e1; 
                                                    case rr of
                                                         Left err -> returnA (Left err)
                                                         Right e1'' -> returnA (Right (CSExpInter ann e1'' e2)) 
                                                        }
                                               }
                            }
                    CSExpDiff ann' e1' e2' -> do { r <- precCompare zChar_inter zChar_diff; 
                            case r of
                                Left err -> returnA (Left err)
                                Right r' -> do {if r' == 1 
                                                then returnA (Right (CSExpDiff ann e1' (CSExpInter ann' e2' e2))) 
                                                else do { rr <- correctAdjNodes e1; 
                                                    case rr of
                                                         Left err -> returnA (Left err)
                                                         Right e1'' -> returnA (Right (CSExpInter ann e1'' e2)) 
                                                        }
                                               }
                            }

correctAdjNodes s@(CSExpDiff ann e1 e2) = case e1 of
                    CSExpEmpty _ -> returnA (Right s) 
                    CSExpExt _ _ -> returnA (Right s) 
                    CSExpRef _ _ -> returnA (Right s) 
                    -- adjacent notes have the same operator, so continue to move to its left
                    CSExpDiff _ _ _ -> do { rr <- correctAdjNodes e1; 
                                         case rr of
                                            Left err -> returnA (Left err)
                                            Right e1'' -> returnA (Right (CSExpDiff ann e1'' e2)) 
                                          }
                    CSExpUnion ann' e1' e2' -> do { r <- precCompare zChar_diff zChar_union; 
                            case r of
                                Left err -> returnA (Left err)
                                Right r' -> do {if r' == 1 
                                                then returnA (Right (CSExpUnion ann e1' (CSExpDiff ann' e2' e2))) 
                                                else do { rr <- correctAdjNodes e1; 
                                                    case rr of
                                                         Left err -> returnA (Left err)
                                                         Right e1'' -> returnA (Right (CSExpDiff ann e1'' e2)) 
                                                        }
                                               }
                            }
                    CSExpInter ann' e1' e2' -> do { r <- precCompare zChar_diff zChar_inter; 
                            case r of
                                Left err -> returnA (Left err)
                                Right r' -> do {if r' == 1 
                                                then returnA (Right (CSExpInter ann e1' (CSExpDiff ann' e2' e2))) 
                                                else do { rr <- correctAdjNodes e1; 
                                                    case rr of
                                                         Left err -> returnA (Left err)
                                                         Right e1'' -> returnA (Right (CSExpDiff ann e1'' e2)) 
                                                        }
                                               }
                            }

{- |
 - Compare two operators's precedences from the state OpTable
 -  return result: 1 - op1 > op2; 0 - op1 = op2; -1 - op1 < op2
 -}
precCompare :: String -> String -> Alex (Either String Int)
precCompare op1 op2 = do
    m <- getOpTable
    case (Map.lookup op1 m) of
        Nothing -> alexError (err2Msg (ErrParser (EParseOpNotDef ("[" ++ op1 ++ "] (Can not found in the operator temples).")))) 
        Just f1 -> case (Map.lookup op2 m) of
            Nothing -> alexError (err2Msg (ErrParser (EParseOpNotDef ("[" ++ op2 ++ "] (Can not found in the operator temples).")))) 
            Just f2 -> if ((read prec1) :: Int) > ((read prec2) :: Int)
                        then returnA (Right 1)
                        else if ((read prec1) :: Int) == ((read prec2) :: Int)
                             then returnA (Right 0)
                             else returnA (Right (-1))
                where (_, prec1, _) = formatAnOpEntry f1
                      (_, prec2, _) = formatAnOpEntry f2

{- |
 - format the value of an entry in OpTable
 - return ("relation|function|generic", prec, assoc)
 -}
formatAnOpEntry :: String -> (String, String, String)
formatAnOpEntry f = if isPrefixOf "relation" f
                    then ("relation", "", "")
                    else if isPrefixOf "function" f
                         then ("function", (takeWhile (/= ':') (drop 9 f)),  (drop 1 (dropWhile (/= ':') (drop 9 f))))
                         else if isPrefixOf "generic" f
                              then ("generic", (takeWhile (/= ':') (drop 8 f)),  (drop 1 (dropWhile (/= ':') (drop 8 f))))
                              else ("error","","")
}
