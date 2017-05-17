{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{- |
Module      : Language.ISOZ.Parser.SynTransformerTwo
Description : 
Copyright   : (C) Kangfeng Ye, 2017
License     : BSD3
Maintainer  : ye.randall@gmail.com 
Stability   : experimental

Syntactical transformation to cope with "Clause 12 Syntactic transformation rules" in ISO Z Standard.

It provides

- synTraverse2Tree : traverse the syntax tree after synTraverse1Tree
-}
module Language.ISOZ.Parser.SynTransformerTwo where

import Language.ISOZ.ZChar
import Language.ISOZ.Common.AbsIsoz
import Data.Char
import Language.ISOZ.Parser.Mktuple
import Language.ISOZ.Common.Ann
import Language.ISOZ.Common.Type 
import Language.ISOZ.Lexer.ISOZLexer 
import Language.ISOZ.Parser.SynTransformerOne (getAnnTerm, addAnnTerm)
import Data.Map (Map)
import qualified Data.Map as Map

-- | Traverse of the syntax tree for syntactical transformation
synTraverse2Tree :: SynTraverse2 a => a -> a
synTraverse2Tree = traverse2 

-- | (12.2.7.1) Transform a declaration to an expression 
declToExpr :: Declaration -> Expression
declToExpr decl = case decl of 
            -- 12.2.7.1 Declaration (a comma-separated multiple declaration)
            Declaration a1 declnames expr   -> exprConsDecl declnames expr  
            -- 12.2.7.1 Declaration (a constant declaration)
            AbbrDecl a1 declname expr       -> VarConsExpr a1 (declNameToNAME declname) (SetExpr (getAnnTerm expr) [(expr)])
            -- 12.2.7.1 Declaration (Schema Expression as declaration)
            ExprDecl a1 expr                -> expr 
--          _                               -> error ("unsupported " ++ (show decl))
    where exprConsDecl [x] expr = VarConsExpr (getAnnTerm x) (declNameToNAME x) expr 
          exprConsDecl (x:xs) expr = AndExpr (getAnnTerm x) (exprConsDecl [x] expr) (exprConsDecl xs expr) 

-- | (12.2.7.1 Declaration) Transform a list of declarations to a construct expression 
exprConsFromDecls :: [Declaration] -> Expression
-- variable constructor expression from declarations
exprConsFromDecls [x] = declToExpr x 
exprConsFromDecls (x:xs) = AndExpr (getAnnTerm x) (declToExpr x) (exprConsFromDecls xs) 
    -- variable constructor expression from a declaration

-- | Transform AbbrDecl in Declaration to a pair from NAME to Expression
declToNameExprPair :: [Declaration] -> [(NAME, Expression)]
declToNameExprPair [] = []
declToNameExprPair (x:xs) = (toPair x) : (declToNameExprPair xs)
    where toPair (AbbrDecl a1 declname expr) = ((declNameToNAME declname), expr) 

-- | (12.2.7 Schema text) transform a schema text to an expression
transformSchema :: SchemaText -> Expression 
transformSchema (SchemaTextEmpty ann) = SetExpr ann [(BindExtExpr ann [])]
transformSchema (SchemaTextDecl ann (DeclPart a decls)) = SchemaConsExpr ann (exprConsFromDecls decls) (TruePred a) 
--transformSchema (SchemaTextDecl ann (DeclPart a decls)) = (exprConsFromDecls decls) 
transformSchema (SchemaTextPred ann pred) = SchemaConsExpr ann (SetExpr ann [(BindExtExpr ann [])]) pred 
transformSchema (SchemaText ann (DeclPart a decls) pred) = SchemaConsExpr ann (exprConsFromDecls decls) pred 

-- | Extrace NAME from a DeclName
declNameToNAME :: DeclName -> NAME
declNameToNAME (DName ann name) = name 
declNameToNAME (OpName ann name) = opNameToNAME name

-- | Extrace NAMEs from DeclNames
declNamesToNAMEs :: [DeclName] -> [NAME]
declNamesToNAMEs [] = []
declNamesToNAMEs (x:xs) = (declNameToNAME x) : (declNamesToNAMEs xs)

-- | Extrace NAME from a RefName 
refNameToNAME :: RefName -> NAME
refNameToNAME (RName ann name) = name 
refNameToNAME (OpRName ann name) = opNameToNAME name

-- | Extrace NAMEs from RefNames 
refNamesToNAMEs :: [RefName] -> [NAME]
refNamesToNAMEs [] = []
refNamesToNAMEs (x:xs) = (refNameToNAME x) : (refNamesToNAMEs xs)

-- | Extrace NAMEs from a list of ES and SS 
esssToNAME :: [ESSS] -> NAME
esssToNAME [] = ""
esssToNAME [x] = getName x
    where getName x = case x of
               ES _ es -> es 
               SS _ ss -> ss 
esssToNAME (x:xs) = (getName x) ++ opGlueSTROKE ++ (esssToNAME xs)
    where getName x = case x of
               ES _ es -> es 
               SS _ ss -> ss 

-- | Extrace NAME from a ERE or SRE
eresreToNAME :: ERESRE -> NAME
eresreToNAME e = case e of
                ERE _ ere -> ere 
                SRE _ sre -> sre 

-- | Extrace NAME from a EREP or SREP
erepsrepToNAME :: EREPSREP -> NAME
erepsrepToNAME e = case e of
                EREP _ ere -> ere 
                SREP _ sre -> sre 

-- | Extrace NAME from a ER or SR
ersrToNAME :: ERSR -> NAME
ersrToNAME e = case e of
                ER _ ere -> ere 
                SR _ sre -> sre 

-- | Extrace NAME from a ERP or SRP
erpsrpToNAME :: ERPSRP -> NAME
erpsrpToNAME e = case e of
                ERP _ ere -> ere 
                SRP _ sre -> sre 

-- | (12.2.8) Extrace NAME from a OpName 
opNameToNAME :: OpName -> NAME
opNameToNAME (PrefixOpName ann prefixname) = case prefixname of
                PrePrefixName a pre -> pre ++ opGlueSTROKE
                PrePPrefixName a prep -> prep ++ opGlueSTROKE 
                LPrefixName a l esss eresre -> l ++ opGlueSTROKE ++ (esssToNAME esss) ++ opGlueSTROKE ++ (eresreToNAME eresre) ++ opGlueSTROKE
                LPPrefixName a lp esss erepsrep -> lp ++ opGlueSTROKE ++ (esssToNAME esss) ++ opGlueSTROKE ++ (erepsrepToNAME erepsrep) ++ opGlueSTROKE
opNameToNAME (PostfixOpName a postfixname) = case postfixname of
                PostPostfixName a post -> opGlueSTROKE ++ post
                PostPPostfixName a postp -> opGlueSTROKE ++ postp
                ELPostfixName a el esss ersr -> opGlueSTROKE ++ el ++ opGlueSTROKE ++ (esssToNAME esss) ++ opGlueSTROKE ++ (ersrToNAME ersr) 
                ELPPostfixName a elp esss erpsrp -> opGlueSTROKE ++ elp ++ opGlueSTROKE ++ (esssToNAME esss) ++ opGlueSTROKE ++ (erpsrpToNAME erpsrp) 
opNameToNAME (InfixOpName ann infixName) = case infixName of
                InInfixName a ip -> opGlueSTROKE ++ ip ++ opGlueSTROKE  
                InPInfixName a inp -> opGlueSTROKE ++ inp ++ opGlueSTROKE  
                ELInfixName a el esss eresre -> opGlueSTROKE ++ el ++ opGlueSTROKE ++ (esssToNAME esss) ++ opGlueSTROKE ++ (eresreToNAME eresre) ++ opGlueSTROKE
                ELPInfixName a elp esss erepsrep -> opGlueSTROKE ++ elp ++ opGlueSTROKE ++ (esssToNAME esss) ++ opGlueSTROKE ++ (erepsrepToNAME erepsrep) ++ opGlueSTROKE
opNameToNAME (NofixOpName ann nofixName) = case nofixName of 
                LNofixName a l esss ersr -> l ++ opGlueSTROKE ++ (esssToNAME esss) ++ opGlueSTROKE ++ (ersrToNAME ersr)
                LPNofixName a lp esss erpsrp -> lp ++ opGlueSTROKE ++ (esssToNAME esss) ++ opGlueSTROKE ++ (erpsrpToNAME erpsrp)

-- | Return (es1⋈...⋈esn)
esssnToNAME :: [ESSS_N] -> NAME
esssnToNAME [] = ""
esssnToNAME [x] = getName x
    where getName x = case x of
               ES_N _ name es -> es
               SS_N _ name ss -> ss 
esssnToNAME (x:xs) = (getName x) ++ opGlueSTROKE ++ (esssnToNAME xs)
    where getName x = case x of
               ES_N _ name es -> es
               SS_N _ name ss -> ss 

-- | Return [i1,...,in]
esssnToNAME_i :: [ESSS_N] -> [NAME]
esssnToNAME_i [] = []
esssnToNAME_i [x] = [getName x]
    where getName x = case x of
               ES_N _ name es -> name
               SS_N _ name ss -> name
esssnToNAME_i (x:xs) = ((getName x) : ((esssnToNAME_i xs)))
    where getName x = case x of
               ES_N _ name es -> name
               SS_N _ name ss -> name 

-- | Extract NAME from a ERE_N or SRE_N 
eresrenToNAME :: ERESRE_N -> NAME
eresrenToNAME e = case e of
                ERE_N _ _ ere -> ere 
                SRE_N _ _ sre -> sre 

eresrenToNAME_i :: ERESRE_N -> NAME
eresrenToNAME_i e = case e of
                ERE_N _ name ere -> name
                SRE_N _ name sre -> name 

ersrnToNAME :: ERSR_N -> NAME
ersrnToNAME e = case e of
                ER_N _ _ ere -> ere 
                SR_N _ _ sre -> sre 

ersrnToNAME_i :: ERSR_N -> NAME
ersrnToNAME_i e = case e of
                ER_N _ name ere -> name
                SR_N _ name sre -> name

-- | (12.2.9) Generic name
genNameToNAME :: GenName -> (NAME, [NAME])
genNameToNAME (PrefixGenName ann pregname) = case pregname of 
                PrePrefixGenName a pre name -> (pre ++ opGlueSTROKE, [name]) 
                LPrefixGenName a l esss_n eresre_n name -> (l ++ opGlueSTROKE ++ (esssnToNAME esss_n) ++ opGlueSTROKE ++ (eresrenToNAME eresre_n) ++ opGlueSTROKE, (esssnToNAME_i esss_n) ++ [(eresrenToNAME_i eresre_n), name])
genNameToNAME (PostfixGenName ann postgenname) = case postgenname of
                PostPostfixGenName a name post -> (opGlueSTROKE ++ post, [name]) 
                ELPostfixGenName a name el esss_n ersr_n -> (opGlueSTROKE ++ el ++ opGlueSTROKE ++ (esssnToNAME esss_n) ++ opGlueSTROKE ++ (ersrnToNAME ersr_n), [name] ++ (esssnToNAME_i esss_n) ++ [(ersrnToNAME_i ersr_n)])
genNameToNAME (InfixGenName ann infixgname) = case infixgname of
                InInfixGenName a name1 ii name2 -> (opGlueSTROKE ++ ii ++ opGlueSTROKE, [name1, name2]) 
                ELInfixGenName a name1 el esss_n eresre_n name2 -> (opGlueSTROKE ++ el ++ opGlueSTROKE ++ (esssnToNAME esss_n) ++ opGlueSTROKE ++ (eresrenToNAME eresre_n) ++ opGlueSTROKE, [name1] ++ (esssnToNAME_i esss_n) ++ [(eresrenToNAME_i eresre_n)] ++ [name2])
genNameToNAME (NofixGenName ann nofixgname) = case nofixgname of
                LNofixGenName a l esss_n ersr_n -> (l ++ opGlueSTROKE ++ (esssnToNAME esss_n) ++ opGlueSTROKE ++ (ersrnToNAME ersr_n), (esssnToNAME_i esss_n) ++ [(ersrnToNAME_i ersr_n)])

-- | Return (es1⋈...esn)
essseToNAME :: [ESSS_E] -> NAME
essseToNAME [] = ""
essseToNAME [x] = getName x
    where getName x = case x of
               ES_E _ name es -> es
               SS_E _ name ss -> ss 
essseToNAME (x:xs) = (getName x) ++ opGlueSTROKE ++ (essseToNAME xs)
    where getName x = case x of
               ES_E _ name es -> es
               SS_E _ name ss -> ss 

-- | (12.2.12) ExpressionList to {(1, e1), (2, e2), ...}
transExpsList :: [Expression] -> Expression 
transExpsList [] = SetExpr [] []   
transExpsList [x] = SetExpr a [(TupleExtExpr a (NumExpr a "1") [x])]   
    where a = getAnnTerm x
transExpsList s@(x:xs) = SetExpr a (listE 1 s)
    where a = getAnnTerm x
          listE n [x] = [(TupleExtExpr a (NumExpr a (show n)) [x])]
          listE n (x:xs) = (TupleExtExpr a (NumExpr a (show n)) [x]) : (listE (n+1) xs)

-- | Return [e] 
essseToNAME_e :: [ESSS_E] -> [Expression]
essseToNAME_e [] = []
essseToNAME_e [x] = getExpr x
    where getExpr x = case x of
               ES_E _ e es -> [e] 
               SS_E _ e ss -> [(transExpsList e)] 
essseToNAME_e (x:xs) = (getExpr x) ++ (essseToNAME_e xs)
    where getExpr x = case x of
               ES_E _ e es -> [e] 
               SS_E _ e ss -> [(transExpsList e)] 

-- | 
erepsrepeToNAME :: EREPSREP_E -> NAME
erepsrepeToNAME e = case e of
                EREP_E _ _ ere -> ere 
                SREP_E _ _ sre -> sre 

-- | 
erepsrepeToNAME_e :: EREPSREP_E -> [Expression]
erepsrepeToNAME_e e = case e of
                EREP_E _ e ere -> [e]
                SREP_E _ e sre -> [(transExpsList e)] 

-- | 
erpsrpeToNAME :: ERPSRP_E -> NAME
erpsrpeToNAME e = case e of
                ERP_E _ _ ere -> ere 
                SRP_E _ _ sre -> sre 

-- | 
erpsrpeToNAME_e :: ERPSRP_E -> [Expression]
erpsrpeToNAME_e e = case e of
                ERP_E _ e ere -> [e]
                SRP_E _ e sre -> [(transExpsList e)] 

-- | (12.2.10) Relation operator application 
--
-- All relation operator applications are transformed to annotated membership predicates.
relationToPred :: Relation -> Predicate
relationToPred (PrefixRel ann  rel) = case rel of 
    PrePPrefixRel a prep expr -> MemPred ann expr (RefExprA a (prep ++ opGlueSTROKE))
    LPPrefixRel a lp listesss_e erepsrep_e expr -> 
             MemPred ann (TupleExtExpr a (head (essseToNAME_e listesss_e)) ((tail (essseToNAME_e listesss_e)) ++ (erepsrepeToNAME_e erepsrep_e) ++ [expr])) (RefExprA a (lp ++ opGlueSTROKE ++ (essseToNAME listesss_e) ++ opGlueSTROKE ++ (erepsrepeToNAME erepsrep_e) ++ opGlueSTROKE))
-- 
relationToPred (PostfixRel ann rel) = case rel of
    PostPPostfixRel a expr postp -> MemPred ann expr (RefExprA a (opGlueSTROKE ++ postp))
    ELPPostfixRel a expr elp listesss_e erpsrp_e ->  
             MemPred ann (TupleExtExpr a expr ((essseToNAME_e listesss_e) ++ (erpsrpeToNAME_e erpsrp_e))) (RefExprA a (opGlueSTROKE ++ elp ++ opGlueSTROKE ++ (essseToNAME listesss_e) ++ opGlueSTROKE ++ (erpsrpeToNAME erpsrp_e)))
-- turn (a = e1 = e2 = e3) into
--  (a = e1 ) and (e1 = e2) 
-- 12.2.10.3
relationToPred (InfixRel ann   rel) = case rel of
    InInfixRel a expr listip_e -> case listip_e of  
              [x] -> trans expr x 
              -- add additional TypeAnn in the beginning of a chained relation to identify it
              -- e1 ip1 e2 ip2 e3 => e1 ip1 (e2 : a1) /\ (e2 : a1) ip2 (e3 : a2)
              (x:xs) -> AndPred ((TypeAnn (Type $ TypeVar "Chain")): a) (trans expr (addTypeVar x 1)) (transList 1 (x:xs))
        where trans e1 (IP_E a1 op e2) 
                | op == "="     = MemPred a e1 (SetExpr a1 [e2])  
                | op == "\8712" = MemPred a e1 e2  
--                | otherwise     = MemPred a (TupleExtExpr a e1 [e2]) (RefExprA a1 op)  
                | otherwise     = MemPred a (TupleExtExpr a e1 [e2]) (RefExprA a1 (opGlueSTROKE ++ op ++ opGlueSTROKE))  
              trans1 (IP_E a1 op1 e1) s@(IP_E a2 op2 e2) = trans e1 s
              transList i [x,y] = (trans1 (addTypeVar x i) (addTypeVar y (i+1)))
              transList i (x:xs) = AndPred (getAnnTerm x) (trans1 (addTypeVar x i) (addTypeVar (head xs) (i+1))) (transList (i+1) xs)
              addTypeVar (IP_E a op e) i = IP_E a op (addAnnTerm (TypeAnn (Type (TypeVar ("a" ++ show i)))) e)
    -- 
    ELPInfixRel a expr1 elp listesss_e erepsrep_e expr2 -> 
             MemPred ann (TupleExtExpr a expr1 ((essseToNAME_e listesss_e) ++ (erepsrepeToNAME_e erepsrep_e) ++ [expr2])) (RefExprA a (opGlueSTROKE ++ elp ++ opGlueSTROKE ++ (essseToNAME listesss_e) ++ opGlueSTROKE ++ (erepsrepeToNAME erepsrep_e) ++ opGlueSTROKE))
-- 
relationToPred (NofixRel ann   rel) = case rel of
    LPNofixRel a lp listesss_e erpsrp_e -> 
             MemPred ann (TupleExtExpr a (head (essseToNAME_e listesss_e)) ((tail (essseToNAME_e listesss_e)) ++ (erpsrpeToNAME_e erpsrp_e))) (RefExprA a (lp ++ opGlueSTROKE ++ (essseToNAME listesss_e) ++ opGlueSTROKE ++ (erpsrpeToNAME erpsrp_e)))
            
-- | 
eresreeToNAME :: ERESRE_E -> NAME
eresreeToNAME e = case e of
                ERE_E _ _ ere -> ere 
                SRE_E _ _ sre -> sre 

-- | 
eresreeToNAME_e :: ERESRE_E -> [Expression]
eresreeToNAME_e e = case e of
                ERE_E _ e ere -> [e]
                SRE_E _ e sre -> [(transExpsList e)] 

-- | 
ersreToNAME :: ERSR_E -> NAME
ersreToNAME e = case e of
                ER_E _ _ er -> er 
                SR_E _ _ sr -> sr 

-- | 
ersreToNAME_e :: ERSR_E -> [Expression]
ersreToNAME_e e = case e of
                ER_E _ e er -> [e]
                SR_E _ e sr -> [(transExpsList e)] 

-- | Remove strokes from an operator name
--
-- TODO: 
-- -     1. [x] op_1
-- -     2. [x] op^1
-- -     3. others ...
removeStrokesFromOp :: String -> String
removeStrokesFromOp [] = []  
removeStrokesFromOp (x:xs) = case x of
                                '\x2197' -> []  -- zChar_wordgluene 
                                '\x2198' -> []  -- zChar_wordgluese
                                _                -> x: (removeStrokesFromOp xs) 

-- | Lookup an operator's mode (function or generic) from the map
--
-- Notes: if (op) is an operator, then (op_1) should be an operator as well
--      so we need to remove all strokes from the operator name, then lookup
--      from the map
lookupOperatorMode :: String -> Map String Bool -> Maybe Bool 
lookupOperatorMode op m = Map.lookup (removeStrokesFromOp op) m
 
-- | 12.2.11 Function and generic operator application
-- 
-- All function operator applications are transformed to annotated application expressions.
-- 
-- All generic operator applications are transformed to annotated generic instantiation expressions.
-- 
appToExpr :: Application -> Map String Bool -> Expression
-- 12.2.11.1 PrefixApp
-- Function
--  pre⋈ e
--  ln⋈es1⋈...⋈esn-1⋈ere⋈ (e1,e2,...,en-1,en)
--  ln⋈es1⋈...⋈an-1⋈sre⋈ (e1,e2,...,an-1,en)
-- Generic 
--  pre⋈ [e]
--  ln⋈es1⋈...⋈esn-1⋈ere⋈ [e1,e2,...,en-1,en]
--  ln⋈es1⋈...⋈an-1⋈sre⋈ [e1,e2,...,an-1,en]
appToExpr (PrefixApp ann  app) m = case app of  
    PrePrefixApp a pre expr -> -- ApplExpr a (RefExprA a (pre ++ opGlueSTROKE)) expr  
            case lookupOperatorMode pre m of 
                 Just isfunc -> case isfunc of 
                                True ->  ApplExpr ann (RefExprA a (pre ++ opGlueSTROKE)) expr 
                                -- generic 
                                False -> GenRefExprA ann (pre ++ opGlueSTROKE) [expr]
                 Nothing ->  ApplExpr ann (RefExprA a (pre ++ opGlueSTROKE)) expr 
    LPrefixApp a l listesss_e eresre_e expr -> -- ApplExpr a (RefExprA a (l ++ opGlueSTROKE ++ (essseToNAME listesss_e) ++ opGlueSTROKE ++ (eresreeToNAME eresre_e) ++ opGlueSTROKE)) (TupleExtExpr a (head (essseToNAME_e listesss_e)) ((tail (essseToNAME_e listesss_e)) ++ (eresreeToNAME_e eresre_e) ++ [expr]))
            case lookupOperatorMode l m of 
                 Just isfunc -> case isfunc of 
                                True -> ApplExpr ann (RefExprA a (l ++ opGlueSTROKE ++ (essseToNAME listesss_e) ++ opGlueSTROKE ++ (eresreeToNAME eresre_e) ++ opGlueSTROKE)) (TupleExtExpr a (head (essseToNAME_e listesss_e)) ((tail (essseToNAME_e listesss_e)) ++ (eresreeToNAME_e eresre_e) ++ [expr]))
                                False -> -- ApplExpr a (RefExprA a (l ++ opGlueSTROKE ++ (essseToNAME listesss_e) ++ opGlueSTROKE ++ (eresreeToNAME eresre_e) ++ opGlueSTROKE)) (TupleExtExpr a (head (essseToNAME_e listesss_e)) ((tail (essseToNAME_e listesss_e)) ++ (eresreeToNAME_e eresre_e) ++ [expr]))
                                        GenRefExprA a (l ++ opGlueSTROKE ++ (essseToNAME listesss_e) ++ opGlueSTROKE ++ (eresreeToNAME eresre_e) ++ opGlueSTROKE) ((essseToNAME_e listesss_e) ++ (eresreeToNAME_e eresre_e) ++ [expr])
                 Nothing -> ApplExpr ann (RefExprA a (l ++ opGlueSTROKE ++ (essseToNAME listesss_e) ++ opGlueSTROKE ++ (eresreeToNAME eresre_e) ++ opGlueSTROKE)) (TupleExtExpr a (head (essseToNAME_e listesss_e)) ((tail (essseToNAME_e listesss_e)) ++ (eresreeToNAME_e eresre_e) ++ [expr]))
-- 12.2.11.2 PostfixApp
-- Function
--  ⋈post e
--  ⋈el⋈es2...⋈esn-1⋈er (e1,e2,...,en-1,en)
--  ⋈el⋈es2...⋈esn-1⋈sr (e1,e2,...,an-1,en)
-- Generic 
--  ⋈post [e]
--  ⋈el⋈es2...⋈esn-1⋈er [e1,e2,...,en-1,en]
--  ⋈el⋈es2...⋈esn-1⋈sr [e1,e2,...,an-1,en]
appToExpr (PostfixApp ann app) m = case app of   
    PostPostfixApp a expr post -> -- ApplExpr a (RefExprA a (opGlueSTROKE ++ post)) expr  
            case lookupOperatorMode post m of 
                 Just isfunc -> case isfunc of 
                                True -> ApplExpr ann (RefExprA a (opGlueSTROKE ++ post)) expr
                                -- generic 
                                False -> GenRefExprA ann (opGlueSTROKE ++ post) [expr]
                 Nothing -> ApplExpr ann (RefExprA a (opGlueSTROKE ++ post)) expr
    ELPostfixApp a expr el listesss_e ersr_e -> 
            case lookupOperatorMode el m of 
                 Just isfunc -> case isfunc of 
                                True -> ApplExpr ann (RefExprA a (opGlueSTROKE ++ el ++ opGlueSTROKE ++ (essseToNAME listesss_e) ++ opGlueSTROKE ++ (ersreToNAME ersr_e))) (TupleExtExpr a expr ((essseToNAME_e listesss_e) ++ (ersreToNAME_e ersr_e)))
                                -- generic 
                                False -> GenRefExprA ann (opGlueSTROKE ++ el ++ opGlueSTROKE ++ (essseToNAME listesss_e) ++ opGlueSTROKE ++ (ersreToNAME ersr_e)) ([expr] ++ (essseToNAME_e listesss_e) ++ (ersreToNAME_e ersr_e)) 
                 Nothing -> ApplExpr ann (RefExprA a (opGlueSTROKE ++ el ++ opGlueSTROKE ++ (essseToNAME listesss_e) ++ opGlueSTROKE ++ (ersreToNAME ersr_e))) (TupleExtExpr a expr ((essseToNAME_e listesss_e) ++ (ersreToNAME_e ersr_e)))
-- 12.2.11.3 Infix 
-- Function
--  ⋈in⋈ (e1,e2)
--  ⋈el⋈es2...⋈esn-1⋈ere⋈ (e1,e2,...,en-1,en)
--  ⋈el⋈es2...⋈esn-1⋈sre⋈ (e1,e2,...,an-1,en)
-- Generic 
--  ⋈in⋈ [e1,e2]
--  ⋈el⋈es2...⋈esn-1⋈ere⋈ [e1,e2,...,en-1,en]
--  ⋈el⋈es2...⋈esn-1⋈sre⋈ [e1,e2,...,an-1,en]
appToExpr (InfixApp ann   app) m = case app of 
    InInfixApp a expr1 ii expr2 -> 
            case lookupOperatorMode ii m of 
                 Just isfunc -> case isfunc of 
                                True -> ApplExpr ann (RefExprA a (opGlueSTROKE ++ ii ++ opGlueSTROKE)) (TupleExtExpr a expr1 [expr2])
                                -- generic 
                                False -> GenRefExprA ann (opGlueSTROKE ++ ii ++ opGlueSTROKE) [expr1, expr2] 
                 Nothing -> ApplExpr ann (RefExprA a (opGlueSTROKE ++ ii ++ opGlueSTROKE)) (TupleExtExpr a expr1 [expr2]) 
    ELInfixApp a expr1 el listesss_e eresre_e expr2 -> 
            case lookupOperatorMode el m of 
                 Just isfunc -> case isfunc of 
                                True -> ApplExpr ann (RefExprA a (opGlueSTROKE ++ el ++ opGlueSTROKE ++ (essseToNAME listesss_e) ++ opGlueSTROKE ++ (eresreeToNAME eresre_e) ++ opGlueSTROKE)) (TupleExtExpr a expr1 ((essseToNAME_e listesss_e) ++ (eresreeToNAME_e eresre_e) ++ [expr2]))
                                -- generic 
                                False -> GenRefExprA ann (opGlueSTROKE ++ el ++ opGlueSTROKE ++ (essseToNAME listesss_e) ++ opGlueSTROKE ++ (eresreeToNAME eresre_e) ++ opGlueSTROKE) ([expr1] ++ (essseToNAME_e listesss_e) ++ (eresreeToNAME_e eresre_e) ++ [expr2])
                 Nothing -> ApplExpr ann (RefExprA a (opGlueSTROKE ++ el ++ opGlueSTROKE ++ (essseToNAME listesss_e) ++ opGlueSTROKE ++ (eresreeToNAME eresre_e) ++ opGlueSTROKE)) (TupleExtExpr a expr1 ((essseToNAME_e listesss_e) ++ (eresreeToNAME_e eresre_e) ++ [expr2]))
-- 12.2.11.4
-- Function
--  ln⋈es1...⋈esn-1⋈er (e1,en-1,er)
--  ln⋈es1...⋈esn-1⋈sr (e1,en-1,an)
-- Generic 
--  ln⋈es1...⋈esn-1⋈er [e1,en-1,er]
--  ln⋈es1...⋈esn-1⋈sr [e1,en-1,an]
appToExpr (NofixApp ann   app) m = case app of
    LNofixApp a l listesss_e ersr_e -> 
            case lookupOperatorMode l m of 
                 Just isfunc -> case isfunc of 
                                True -> case listesss_e of
                                    -- e1 to en-1 are empty, therefore (e1,en-1,er) or (e1,en-1,an) becomes only (er) or (an)
                                    --  so (e1,en-1,er) is not a TupleExtExpr and just an expression
                                    []  -> case (ersreToNAME_e ersr_e) of
                                        -- [] is impossible 
                                        -- where t is SetExpr [(1,a), (2, b)]
                                        [t]     -> ApplExpr ann (RefExprA a (l ++ opGlueSTROKE ++ opGlueSTROKE ++ (ersreToNAME ersr_e))) t 
                                        t:ts    -> ApplExpr ann (RefExprA a (l ++ opGlueSTROKE ++ opGlueSTROKE ++ (ersreToNAME ersr_e))) (TupleExtExpr a t ts) 
                                    _   -> let l_esss_e = (essseToNAME_e listesss_e)
                                               l_ersr_e = (ersreToNAME_e ersr_e)
                                               l_all = l_esss_e ++ l_ersr_e
                                           -- e1 to en-1 are not empty, therefore (e1,en-1,er) or (e1,en-1,an) becomes at leat (e1, er) or (e1, an)
                                           --  so  (e1,en-1,er) or (e1,en-1,an) is a TupleExtExpr
                                           in ApplExpr ann (RefExprA a (l ++ opGlueSTROKE ++ (essseToNAME listesss_e) ++ opGlueSTROKE ++ (ersreToNAME ersr_e)))  (TupleExtExpr a (head l_all) (tail l_all)) 
                                -- generic 
                                False -> case listesss_e of
                                    []  -> GenRefExprA ann (l ++ opGlueSTROKE ++ opGlueSTROKE ++ (ersreToNAME ersr_e)) ((essseToNAME_e listesss_e) ++ (ersreToNAME_e ersr_e)) 
                                    _   -> GenRefExprA ann (l ++ opGlueSTROKE ++ (essseToNAME listesss_e) ++ opGlueSTROKE ++ (ersreToNAME ersr_e)) ((essseToNAME_e listesss_e) ++ (ersreToNAME_e ersr_e)) 
                 Nothing -> case listesss_e of
                    []  -> case (ersreToNAME_e ersr_e) of
                        -- []  -> ApplExpr ann (RefExprA a (l ++ opGlueSTROKE ++ opGlueSTROKE ++ (ersreToNAME ersr_e)))  (TupleExtExpr a (EmptySetExpr a) [])
                        [t]   -> ApplExpr ann (RefExprA a (l ++ opGlueSTROKE ++ opGlueSTROKE ++ (ersreToNAME ersr_e))) t 
                        t:ts  -> ApplExpr ann (RefExprA a (l ++ opGlueSTROKE ++ opGlueSTROKE ++ (ersreToNAME ersr_e))) (TupleExtExpr a t ts) 
                    _   -> let l_esss_e = (essseToNAME_e listesss_e)
                               l_ersr_e = (ersreToNAME_e ersr_e)
                               l_all = l_esss_e ++ l_ersr_e
                           -- e1 to en-1 are not empty, therefore (e1,en-1,er) or (e1,en-1,an) becomes at leat (e1, er) or (e1, an)
                           --  so  (e1,en-1,er) or (e1,en-1,an) is a TupleExtExpr
                           in ApplExpr ann (RefExprA a (l ++ opGlueSTROKE ++ (essseToNAME listesss_e) ++ opGlueSTROKE ++ (ersreToNAME ersr_e)))  (TupleExtExpr a (head l_all) (tail l_all)) 


--
class SynTraverse2 a where
    traverse2 :: a -> a
    traverse2List :: [a] -> [a]
    traverse2List = map (traverse2)

instance SynTraverse2 a => SynTraverse2 [a] where
  traverse2 = traverse2List

instance SynTraverse2 Specification where
  traverse2 e = case e of
    SpecSect ann sections -> SpecSectA ann (traverse2 sections)
    -- 12.2.1.1
    SpecAnony ann paragraphs -> -- SpecAnony ann (traverse2 paragraphs)
        traverse2 (SpecSect ann [(InheritingSection (getAnnTerm paragraphs) zChar_zedchar "Specification" ["standard_toolkit"] zChar_endchar paragraphs)])
    SpecEmpty ann -> SpecEmpty ann

instance SynTraverse2 Section where
  traverse2 e = case e of
    BaseSection ann zed sectname end paragraphs -> traverse2 (InheritingSection ann zed sectname [] end paragraphs)
    InheritingSection ann zed sectname listname end paragraphs -> InheritingSectionA ann sectname listname (traverse2 paragraphs)  
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 Paragraph where
  traverse2 e = case e of
    GivenPara ann listname -> GivenParaA ann listname
    AxdefPara ann ax schematext end -> -- AxdefPara ann ax (traverse2 schematext) end 
                AxdefParaA ann (transformSchema (traverse2 schematext))
    -- 12.2.3.1 Schema definition paragraph
    SchdefPara ann sch name schematext end -> traverse2 (AxdefPara ann zChar_axchar (SchemaTextDecl ann (DeclPart ann [(AbbrDecl ann (DName ann name) (SchemaExpr (getAnnTerm schematext) schematext))])) zChar_endchar)
    GenAxdefPara ann ax formals schematext end -> -- GenAxdefPara ann ax formals (traverse2 schematext) end  
                GenAxdefParaA ann (formalsToString formals) (transformSchema (traverse2 schematext)) 
            where formalsToString (Formals ann listname) = listname
    -- 12.2.3.2 Generic schema definition paragraph
    GenSchdefPara ann sch formals name schematext end -> traverse2 (GenAxdefPara ann (zChar_axchar++zChar_genchar) formals (SchemaTextDecl ann (DeclPart ann [(AbbrDecl ann (DName ann name) (SchemaExpr (getAnnTerm schematext) schematext))])) zChar_endchar) 
    GenConjecturePara ann name formals pred -> GenConjectureParaA ann name (formalsToString formals) (traverse2 pred)
            where formalsToString (Formals ann listname) = listname
    -- 12.2.3.3 Horizontal definition paragraph
    HDefPara ann name expr -> traverse2 (AxdefPara ann zChar_axchar (SchemaTextDecl ann (DeclPart ann [(AbbrDecl ann name expr)])) zChar_endchar)
    -- 12.2.3.4 Generic horizontal definition paragraph 
--    GenHDefPara ann name formals expr -> traverse2 (GenAxdefPara ann (zChar_axchar++zChar_genchar) formals (SchemaTextDecl ann (DeclPart ann [(AbbrDecl ann (DName ann name) expr)])) zChar_endchar) 
    GenHDefPara ann name formals expr -> traverse2 (GenAxdefPara ann (zChar_axchar++zChar_genchar) formals (SchemaTextDecl ann (DeclPart ann [(AbbrDecl ann name expr)])) zChar_endchar) 
    -- 12.2.9 Generic name 
    GenOpDefPara ann genname expr -> -- GenOpDefPara ann (traverse2 genname) (traverse2 expr) 
            traverse2 (GenHDefPara ann (DName ann (fst (genNameToNAME genname))) (Formals ann (snd (genNameToNAME genname))) expr)
            
    -- 12.2.3.5 
    FreetypePara ann listtype -> FreetypeParaA ann (fsort (traverse2 listtype))
                            where fsort [] = [] 
                                  fsort (x:xs) = (:) (fbsort x) (fsort xs)
                                  fbsort s@(Freetype a1 name xs) = (Freetype a1 name (fbrsort [] xs))
                                  -- sort a list of branches
                                  -- constant in the beginning of r and constructor in the end of r
                                  fbrsort r [] = r 
                                  fbrsort r (x:xs) = case x of
                                            s@(ConstantBranchA an dname) -> fbrsort (s : r) xs
                                            s@(ConstructorBranchA an dname expr) -> fbrsort (reverse (s: (reverse r))) xs
    ConjecturePara ann name pred -> ConjectureParaA ann name (traverse2 pred) 
    OperatorPara ann optemplate -> OperatorPara ann (traverse2 optemplate)  -- TODO: not necessary to be kept in annotated syntax but how to keep these information for later use to transform Applications when referring to function or generic
    ChannelPara ann cdecl -> ChannelPara ann (traverse2 cdecl)
    ChannelFromPara ann cdecl -> ChannelFromPara ann (traverse2 cdecl) 
    ChannelSetPara ann cdecl -> ChannelSetPara ann (traverse2 cdecl) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 Freetype where
  traverse2 e = case e of
    Freetype ann name listbranches -> Freetype ann (name) (traverse2 listbranches) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 Branch where
  traverse2 e = case e of
    ConstantBranch ann declname -> ConstantBranchA ann (declNameToNAME declname) 
    ConstructorBranch ann declname expr -> ConstructorBranchA ann (declNameToNAME declname) (traverse2 expr) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 Formals where
  traverse2 e = case e of
    Formals ann listname -> e 

instance SynTraverse2 Predicate where
  traverse2 e = case e of
    AndPred ann p1 p2 -> AndPred ann (traverse2 p1) (traverse2 p2) 
    -- 12.2.5.6 Disjunction predicate
    OrPred ann p1 p2 -> -- OrPred ann (traverse2 p1) (traverse2 p2) 
                traverse2 (NegPred ann (AndPred ann (NegPred (getAnnTerm p1) p1) (NegPred (getAnnTerm p2) p2)))
    ForallPred ann schtext p -> -- ForallPred ann (traverse2 schtext) (traverse2 p) 
                ForallPredA ann (transformSchema (traverse2 schtext)) (traverse2 p) 
    ForallPredA ann exp pred -> e 
    -- 12.2.5.3 Existential quantification predicate
    ExistsPred ann schtext p -> -- ExistsPred ann (traverse2 schtext) (traverse2 p) 
                traverse2 (NegPred ann (ForallPred ann schtext (NegPred (getAnnTerm p) p)))
    Exists1Pred ann schtext p -> -- Exists1Pred ann (traverse2 schtext) (traverse2 p) 
                Exists1PredA ann (transformSchema (traverse2 schtext)) (traverse2 p) 
    -- 12.2.5.4 Equivalence predicate
    EquivPred ann p1 p2 -> -- EquivPred ann (traverse2 p1) (traverse2 p2) 
                traverse2 (AndPred ann (ImplPred ann p1 p2) (ImplPred ann p2 p1))
    -- 12.2.5.5 Implication predicate
    ImplPred ann p1 p2 -> -- ImplPred ann (traverse2 p1) (traverse2 p2) 
                traverse2 (OrPred ann (NegPred ann p1) p2)
    NegPred ann p -> NegPred ann (traverse2 p) 
    RelPred ann relation -> -- RelPred ann (traverse2 relation) 
                traverse2 (relationToPred relation)
    -- 12.2.5.7 Schema predicate
    ExprPred ann e -> -- ExprPred ann (traverse2 p) 
                traverse2 (MemPred ann (ThetaExpr ann e []) e)
--                traverse2 (RelPred ann (InfixRel ann (InInfixRel ann (ThetaExpr ann p []) [(IP_E ann "\8712" p)])))
    TruePred ann -> TruePred ann 
    -- 12.2.5.8 Falsity predicate
    FalsePred ann -> 
                traverse2 (NegPred ann (TruePred ann))
    MemPred ann expr1 expr2 -> MemPred ann (traverse2 expr1) (traverse2 expr2)
    _           -> e
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 RenamePair where
  traverse2 e = case e of
    RenamePair ann newname oldname -> RenamePairA ann (declNameToNAME newname) (declNameToNAME oldname)
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 Expression where
  traverse2 e = case e of
    NumExpr ann name -> NumExpr ann name 
    ForallExpr ann schtext expr -> -- ForallExpr ann (traverse2 schtext) (traverse2 expr) 
                ForallExprA ann (transformSchema (traverse2 schtext)) (traverse2 expr)
    ExistsExpr ann schtext expr -> -- ExistsExpr ann (traverse2 schtext) (traverse2 expr) 
                traverse2 (NegExpr ann (ForallExpr ann schtext (NegExpr (getAnnTerm expr) expr)))
    Exists1Expr ann schtext expr -> -- Exists1Expr ann (traverse2 schtext) (traverse2 expr) 
                Exists1ExprA ann (transformSchema (traverse2 schtext)) (traverse2 expr)
--    LambdaExpr schtext expr -> LambdaExpr (traverse2 schtext) (traverse2 expr) 
--    9.3.1 Function construction expression
--    \lambda t @ e => { t @ (chartuple t, e) }
--    it has been transformed in SynTransformerOne
    LambdaExpr ann schtext expr -> e 
    MuExpr ann schtext expr -> -- MuExpr ann (traverse2 schtext) (traverse2 expr) 
                MuExprA ann (transformSchema (traverse2 schtext)) (traverse2 expr)
    LocalDefExpr ann decllist expr -> -- LocalDefExpr ann (traverse2 decllist) (traverse2 expr) 
                traverse2 (MuExpr ann (SchemaTextDecl ann (DeclPart ann decllist)) expr)
    EquivExpr ann expr1 expr2 -> -- EquivExpr ann (traverse2 expr1) (traverse2 expr2) 
                traverse2 (AndExpr ann (ImplExpr ann expr1 expr2) (ImplExpr ann expr2 expr1))
    ImplExpr ann expr1 expr2 -> -- ImplExpr ann (traverse2 expr1) (traverse2 expr2) 
                traverse2 (OrExpr ann (NegExpr ann expr1) expr2)
    AndExpr ann expr1 expr2 -> AndExpr ann (traverse2 expr1) (traverse2 expr2) 
    OrExpr ann expr1 expr2 -> -- OrExpr ann (traverse2 expr1) (traverse2 expr2) 
                traverse2 (NegExpr ann (AndExpr ann (NegExpr (getAnnTerm expr1) expr1) (NegExpr (getAnnTerm expr2) expr2)))
    NegExpr ann expr -> NegExpr ann (traverse2 expr) 
    -- 12.2.6.6 Conditional expression
    CondExpr ann pred expr1 expr2 -> -- CondExpr ann (traverse2 pred) (traverse2 expr1) (traverse2 expr2) 
                traverse2 (MuExpr ann 
                            (SchemaText ann (DeclPart ann 
                                    [(Declaration ann [(DName ann "ii")] (SetExpr ann [expr1, expr2]))]) 
                                    (OrPred ann (AndPred ann pred 
                                                    (RelPred ann (InfixRel ann (InInfixRel ann 
                                                        (RefExpr ann (RName ann "ii")) [(IP_E ann "=" expr1)])))) 
                                                (AndPred ann (NegPred ann pred)
                                                    (RelPred ann (InfixRel ann (InInfixRel ann 
                                                        (RefExpr ann (RName ann "ii")) [(IP_E ann "=" expr2)]))))
                                    )
                            ) 
                            (RefExpr ann (RName ann "ii"))
                          )
    CompExpr ann expr1 expr2 -> CompExpr ann (traverse2 expr1) (traverse2 expr2) 
    PipeExpr ann expr1 expr2 -> PipeExpr ann (traverse2 expr1) (traverse2 expr2) 
    HideExpr ann expr namelist -> -- HideExpr ann (traverse2 expr) (traverse2 namelist) 
                    HideExprA ann (traverse2 expr) (declNamesToNAMEs namelist)
    --  12.2.6.7 Schema projection expression
    ProjExpr ann expr1 expr2 -> -- ProjExpr ann (traverse2 expr1) (traverse2 expr2) 
                    traverse2 (SetCompExpr ann 
                                (SchemaTextDecl ann (DeclPart ann [(ExprDecl ann expr1), (ExprDecl ann expr2)])) 
                                (ThetaExpr (getAnnTerm expr2) expr2 []))
    PreExpr ann expr -> PreExpr ann (traverse2 expr) 
    -- 12.2.6.8 Cartesian product expression
    ProdExpr ann exprlist -> -- ProdExpr ann (traverse2 (convert exprlist)) 
                    traverse2 (SetCompExpr ann 
                                (SchemaTextDecl ann (DeclPart ann (genListDecl 0 exprlist'))) 
                                (TupleExtExpr ann (RefExpr ann (RName ann ("i" ++ "0"))) (genListExpr 1 (tail exprlist'))))
                    where genListDecl i [] = [] 
                          genListDecl i (x:xs) = (Declaration ann [(DName ann ("i" ++ show i))] x) : (genListDecl (i+1) xs)
                          genListExpr i [] = [] 
                          genListExpr i (x:xs) = (RefExpr ann (RName ann ("i" ++ show i))) : (genListExpr (i+1) xs)
                          exprlist' = convert (getParserParDepthFromListAnnsN ann) exprlist
                          convert n [] = []
                          convert n (s:xs) = case s of 
                                            ProdExpr ann1 list  -> if (getParserParDepthFromListAnnsN ann1) == n 
                                                                   then (convert n list) ++ (convert n xs)
                                                                   else (s : (convert n xs))
                                            _                   -> (s : (convert n xs))

    PowerExpr ann expr -> PowerExpr ann (traverse2 expr) 
    FuncApplExpr ann app -> FuncApplExpr ann (traverse2 app) 
--                    (appToExpr (traverse2 app))
    ApplExpr ann expr1 expr2 -> ApplExpr ann (traverse2 expr1) (traverse2 expr2) 
    DecorExpr ann expr decor -> DecorExpr ann (traverse2 expr) (decor) 
    RenameExpr ann expr namepairlist -> RenameExpr ann (traverse2 expr) (traverse2 namepairlist)
    BindSelExpr ann expr refname -> BindSelExprA ann (traverse2 expr) (refNameToNAME refname) 
    TupleSelExpr ann expr numeral -> TupleSelExpr ann (traverse2 expr) (numeral)  
    ThetaExpr ann expr strokelist -> ThetaExpr ann (traverse2 expr) strokelist 
    RefExpr ann refname -> RefExprA ann (refNameToNAME (traverse2 refname))
-- Todo: where does RefExprA come from?
    RefExprA ann refname -> RefExprA ann (refname)
    GenRefExpr ann refname exprlist -> GenRefExprA ann (refNameToNAME (traverse2 refname)) (traverse2 exprlist)
    EmptySetExpr ann -> e 
    SetExpr ann exprlist -> SetExpr ann (traverse2 exprlist) 
    SetCompExpr ann schematext expr -> SetCompExprA ann (transformSchema (traverse2 schematext)) (traverse2 expr) 
    SetCompExprA ann _ _ -> e
--    CharSetCompExpr schematext -> CharSetCompExpr (traverse2 schematext) 
--    9.3.2 Characteristic set comprehension expression 
--    {t} => { t @ chartuple t }
    CharSetCompExpr ann schematext -> traverse2 (SetCompExpr ann (traverse2 schematext) (chartuple (traverse2 schematext)))
    SchemaExpr ann schematext -> -- SchemaExpr ann (traverse2 schematext) 
                transformSchema (traverse2 (schematext))
    BindExtExpr ann decllist -> BindExtExprA ann (declToNameExprPair (traverse2 decllist))
    TupleExtExpr ann expr1 exprlist -> TupleExtExpr ann (traverse2 expr1) (traverse2 exprlist) 
--    CharMuExpr schematext -> CharMuExpr (traverse2 schematext) 
--    9.3.2 Characteristic definite description expression 
--    {\mu t} => \mu t @ chartuple t
    CharMuExpr ann schematext -> e --MuExpr ann (traverse2 schematext) (chartuple (traverse2 schematext))
    VarConsExpr ann name expr -> VarConsExpr ann name (traverse2 expr)
    SchemaConsExpr ann expr pred -> SchemaConsExpr ann expr pred 
    r   -> error ("Not supported (" ++ show r ++ ")")
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)


instance SynTraverse2 SchemaText where
  traverse2 e = case e of
    SchemaTextEmpty ann -> SchemaTextEmpty ann
    SchemaTextDecl ann decl -> SchemaTextDecl ann (traverse2 decl)
    SchemaTextPred ann pred -> SchemaTextPred ann (traverse2 pred) 
    SchemaText ann decl pred -> SchemaText ann (traverse2 decl) (traverse2 pred) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 DeclPart where
  traverse2 e = case e of
    DeclPart ann decls -> DeclPart ann (traverse2 decls) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 Declaration where
  traverse2 e = case e of
    Declaration ann declnames expr -> Declaration ann (traverse2 declnames) (traverse2 expr) 
    AbbrDecl ann declname expr -> AbbrDecl ann (traverse2 declname) (traverse2 expr)
    ExprDecl ann expr -> ExprDecl ann (traverse2 expr) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 OperatorTemplate where
  traverse2 e = case e of
    RelationTemplate ann template -> RelationTemplate ann (traverse2 template) 
    FunctionTemplate ann template -> FunctionTemplate ann (traverse2 template)
    GenericTemplate ann template -> GenericTemplate ann (traverse2 template)
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 CategoryTemplate where
  traverse2 e = case e of
    PrefixCatTemplate ann template -> PrefixCatTemplate ann (traverse2 template) 
    PostfixCatTemplate ann template -> PostfixCatTemplate ann (traverse2 template) 
    InfixCatTemplate ann prec assoc template -> InfixCatTemplate ann prec assoc (traverse2 template) 
    NofixCatTemplate ann template -> NofixCatTemplate ann (traverse2 template) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 Prec where
  traverse2 e = case e of
    Prec ann _ -> e

instance SynTraverse2 Assoc where
  traverse2 e = case e of
    LeftAssoc ann -> e
    RightAssoc ann -> e

instance SynTraverse2 Template where
  traverse2 e = case e of
    TPrefixTemplate ann template -> TPrefixTemplate ann (traverse2 template)
    TPostfixTemplate ann template -> TPostfixTemplate ann (traverse2 template)
    TInfixTemplate ann template -> TInfixTemplate ann (traverse2 template)
    TNofixTemplate ann template -> TNofixTemplate ann (traverse2 template)
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 PrefixTemplate where
  traverse2 e = case e of
    PrefixTemplate ann prefixname  -> PrefixTemplate ann (traverse2 prefixname) 
    PowerPrefixTemplate ann        -> PowerPrefixTemplate ann 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 PostfixTemplate where
  traverse2 e = case e of
    PostfixTemplate ann postfixname  -> PostfixTemplate ann (traverse2 postfixname) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 InfixTemplate where
  traverse2 e = case e of
    InfixTemplate ann infixname  -> InfixTemplate ann (traverse2 infixname) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 NofixTemplate where
  traverse2 e = case e of
    NofixTemplate ann nofixname  -> NofixTemplate ann (traverse2 nofixname) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)


instance SynTraverse2 DeclName where
  traverse2 e = case e of
    DName ann name -> DName ann name 
    OpName ann name -> OpName ann name 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 RefName where
  traverse2 e = case e of
    RName ann name -> e 
    OpRName ann name -> OpRName ann (traverse2 name)
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 OpName where
  traverse2 e = case e of
    PrefixOpName ann name -> PrefixOpName ann (traverse2 name)
    PostfixOpName ann name -> PostfixOpName ann (traverse2 name) 
    InfixOpName ann name -> InfixOpName ann (traverse2 name) 
    NofixOpName ann name -> NofixOpName ann (traverse2 name)
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 PrefixName where
  traverse2 e = case e of
    PrePrefixName ann pre -> e 
    PrePPrefixName ann prep -> e 
    LPrefixName ann l listesss eresre -> LPrefixName ann l (traverse2 listesss) (traverse2 eresre) 
    LPPrefixName ann l listesss erepsrep -> LPPrefixName ann l (traverse2 listesss) (traverse2 erepsrep)  
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 PostfixName where
  traverse2 e = case e of
    PostPostfixName ann post -> e
    PostPPostfixName ann postp -> e
    ELPostfixName ann el listesss ersr -> ELPostfixName ann el (traverse2 listesss) (traverse2 ersr)
    ELPPostfixName ann elp listesss erpsrp -> ELPPostfixName ann elp (traverse2 listesss) (traverse2 erpsrp)
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 InfixName where
  traverse2 e = case e of
    InInfixName ann ii -> e
    InPInfixName ann ip -> e
    ELInfixName ann el listesss eresre -> ELInfixName ann el (traverse2 listesss) (traverse2 eresre)
    ELPInfixName ann elp listesss erepsrep -> ELPInfixName ann elp (traverse2 listesss) (traverse2 erepsrep)
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 NofixName where
  traverse2 e = case e of
    LNofixName ann l listesss ersr -> LNofixName ann l (traverse2 listesss) (traverse2 ersr)
    LPNofixName ann l listesss erpsrp -> LPNofixName ann l (traverse2 listesss) (traverse2 erpsrp)
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 ESSS where
  traverse2 e = case e of
    ES ann es -> ES ann (es) 
    SS ann ss -> SS ann (ss) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 ESSS_N where
  traverse2 e = case e of
    ES_N ann name es -> e 
    SS_N ann name ss -> e 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 ESSS_E where
  traverse2 e = case e of
    ES_E ann expr es -> ES_E ann (traverse2 expr) es 
    SS_E ann expr ss -> SS_E ann (traverse2 expr) ss 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 ERESRE where
  traverse2 e = case e of
    ERE ann es -> ERE ann (es) 
    SRE ann ss -> SRE ann (ss) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 ERESRE_N where
  traverse2 e = case e of
    ERE_N ann name es -> e 
    SRE_N ann name ss -> e 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 ERESRE_E where
  traverse2 e = case e of
    ERE_E ann expr es -> ERE_E ann (traverse2 expr) es 
    SRE_E ann expr ss -> SRE_E ann (traverse2 expr) ss 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 EREPSREP where
  traverse2 e = case e of
    EREP ann es -> EREP ann (es) 
    SREP ann ss -> SREP ann (ss) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 EREPSREP_E where
  traverse2 e = case e of
    EREP_E ann expr es -> EREP_E ann (traverse2 expr) es 
    SREP_E ann expr ss -> SREP_E ann (traverse2 expr) ss 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 ERSR where
  traverse2 e = case e of
    ER ann es -> ER ann (es) 
    SR ann ss -> SR ann (ss) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 ERSR_N where
  traverse2 e = case e of
    ER_N ann name es -> e 
    SR_N ann name ss -> e 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 ERSR_E where
  traverse2 e = case e of
    ER_E ann expr es -> ER_E ann (traverse2 expr) es 
    SR_E ann expr ss -> SR_E ann (traverse2 expr) ss 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 ERPSRP where
  traverse2 e = case e of
    ERP ann es -> ERP ann (es) 
    SRP ann ss -> SRP ann (ss) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 ERPSRP_E where
  traverse2 e = case e of
    ERP_E ann expr es -> ERP_E ann (traverse2 expr) es 
    SRP_E ann expr ss -> SRP_E ann (traverse2 expr) ss 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 GenName where
  traverse2 e = case e of
    PrefixGenName ann name -> PrefixGenName ann (traverse2 name)
    PostfixGenName ann name -> PostfixGenName ann (traverse2 name)
    InfixGenName ann name -> InfixGenName ann (traverse2 name)
    NofixGenName ann name -> NofixGenName ann (traverse2 name)
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 PrefixGenName where
  traverse2 e = case e of
    PrePrefixGenName ann pre name -> e 
    LPrefixGenName ann l listesss_n eresre_n name -> LPrefixGenName ann l (traverse2 listesss_n) (traverse2 eresre_n) name 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 PostfixGenName where
  traverse2 e = case e of
    PostPostfixGenName ann name post -> e 
    ELPostfixGenName ann name el listesss_n ersr_n -> ELPostfixGenName ann name el (traverse2 listesss_n) (traverse2 ersr_n) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 InfixGenName where
  traverse2 e = case e of
    InInfixGenName ann name1 ii name2 -> e 
    ELInfixGenName ann name1 el listesss_n eresre_n name2 -> ELInfixGenName ann name1 el (traverse2 listesss_n) (traverse2 eresre_n) name2 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 NofixGenName where
  traverse2 e = case e of
    LNofixGenName ann l listesss_n ersr_n -> LNofixGenName ann l (traverse2 listesss_n) (traverse2 ersr_n) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 Relation where
  traverse2 e = case e of
    PrefixRel ann  rel -> PrefixRel ann (traverse2 rel) 
    PostfixRel ann rel -> PostfixRel ann (traverse2 rel) 
    InfixRel ann   rel -> InfixRel ann (traverse2 rel) 
    NofixRel ann   rel -> NofixRel ann (traverse2 rel) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 PrefixRel where
  traverse2 e = case e of
    PrePPrefixRel ann prep expr -> PrePPrefixRel ann prep (traverse2 expr)
    LPPrefixRel ann lp listesss_e erepsrep_e expr -> LPPrefixRel ann lp listesss_e erepsrep_e expr 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 PostfixRel where
  traverse2 e = case e of
    PostPPostfixRel ann expr postp -> PostPPostfixRel ann (traverse2 expr) postp 
    ELPPostfixRel ann expr elp listesss_e erpsrp_e -> ELPPostfixRel ann (traverse2 expr) elp (traverse2 listesss_e) (traverse2 erpsrp_e) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 IP_E where
  traverse2 e = case e of
    IP_E ann str expr -> IP_E ann str (traverse2 expr) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 InfixRel where
  traverse2 e = case e of
    InInfixRel ann expr listip_e -> InInfixRel ann (traverse2 expr) (traverse2 listip_e) 
    ELPInfixRel ann expr1 elp listesss_e erepsrep_e expr2 -> ELPInfixRel ann (traverse2 expr1) elp (traverse2 listesss_e) (traverse2 erepsrep_e) (traverse2 expr2) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 NofixRel where
  traverse2 e = case e of
    LPNofixRel ann lp listesss_e erepserp_e -> LPNofixRel ann lp (traverse2 listesss_e) (traverse2 erepserp_e) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 Application where
  traverse2 e = case e of
    PrefixApp ann  app -> PrefixApp ann  (traverse2 app)
    PostfixApp ann app -> PostfixApp ann (traverse2 app) 
    InfixApp ann   app -> InfixApp ann (traverse2 app) 
    NofixApp ann   app -> NofixApp ann (traverse2 app) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 PrefixApp where
  traverse2 e = case e of
    PrePrefixApp ann pre expr -> PrePrefixApp ann pre (traverse2 expr) 
    LPrefixApp ann l listesss_e eresre_e expr -> LPrefixApp ann l (traverse2 listesss_e) (traverse2 eresre_e) (traverse2 expr) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 PostfixApp where
  traverse2 e = case e of
    PostPostfixApp ann expr post -> PostPostfixApp ann (traverse2 expr) post 
    ELPostfixApp ann expr el listesss_e ersr_e -> ELPostfixApp ann (traverse2 expr) el (traverse2 listesss_e) (traverse2 ersr_e) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 InfixApp where
  traverse2 e = case e of
    InInfixApp ann expr1 ii expr2 -> InInfixApp ann (traverse2 expr1) ii (traverse2 expr2)
    ELInfixApp ann expr1 el listesss_e eresre_e expr2 -> ELInfixApp ann (traverse2 expr1) el (traverse2 listesss_e) (traverse2 eresre_e) (traverse2 expr2) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 NofixApp where
  traverse2 e = case e of
    LNofixApp ann l listesss_e erse_e -> LNofixApp ann l (traverse2 listesss_e) (traverse2 erse_e) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 ChannelDecl where
  traverse2 e = case e of
    ChannelDecl ann simplec -> ChannelDecl ann (traverse2 simplec) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 SimpleCDecl where
  traverse2 e = case e of
    SyncSimpleCDecl ann listname -> e
    TypedSimpleCDecl ann listname expr -> TypedSimpleCDecl ann listname (traverse2 expr) 
    GenericTypedSimpleCDecl ann formals listname expr -> GenericTypedSimpleCDecl ann (traverse2 formals) listname (traverse2 expr)
--    SchemaSimpleCDecl expr -> SchemaSimpleCDecl (traverse2 expr) 
    SimpleCDecl2 ann listname decl2 -> case decl2 of
                            SyncSimpleCDecl2 a1 -> SyncSimpleCDecl ann listname
                            TypedSimpleCDecl2 a1 expr -> TypedSimpleCDecl ann listname (traverse2 expr) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 ChannelFromDecl where
  traverse2 e = case e of
    ChannelFromDecl ann fromcdecl -> ChannelFromDecl ann (traverse2 fromcdecl) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 FromCDecl where
  traverse2 e = case e of
    RefFromCDecl ann expr -> RefFromCDecl ann (traverse2 expr)
    RefDecorFromCDecl ann expr -> RefDecorFromCDecl ann (traverse2 expr)
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 ChannelSetDecl where
  traverse2 e = case e of
    ChannelSetDecl ann name expr -> ChannelSetDecl ann name (traverse2 expr) 
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)

instance SynTraverse2 CSExp where
  traverse2 e = case e of
    CSExpEmpty ann  -> e 
    CSExpExt ann exprs -> CSExpExt ann (traverse2 exprs)
    CSExpRef ann expr -> CSExpRef ann (traverse2 expr) 
    CSExpUnion ann csexp1 csexp2  -> CSExpUnion ann (traverse2 csexp1) (traverse2 csexp2)
    CSExpInter ann csexp1 csexp2  -> CSExpInter ann (traverse2 csexp1) (traverse2 csexp2)
    CSExpDiff ann csexp1 csexp2  -> CSExpDiff ann (traverse2 csexp1) (traverse2 csexp2)
  traverse2List [] = []
  traverse2List (x:xs) = (:) (traverse2 x) (traverse2 xs)
