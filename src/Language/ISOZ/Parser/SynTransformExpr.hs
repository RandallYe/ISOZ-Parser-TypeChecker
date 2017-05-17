{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{- |
Module      : Language.ISOZ.Parser.SynTransformExpr
Description : 
Copyright   : (C) Kangfeng Ye, 2017
License     : BSD3
Maintainer  : ye.randall@gmail.com 
Stability   : experimental

Syntactical transformation to cope with operator precedences and associativities for expressions.

It provides

     - synTraverseExprTree : traverse the syntax tree, particularly expressions
-}

module Language.ISOZ.Parser.SynTransformExpr where

import Language.ISOZ.Common.AbsIsoz
import Language.ISOZ.ZChar 
import Language.ISOZ.Common.Error
import Data.Char
import Data.List
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Language.ISOZ.Parser.Mktuple
import Language.ISOZ.Common.Ann
import Language.ISOZ.Lexer.ISOZLexer 
import Language.ISOZ.Common.OpTableMonad 
import Text.Read (readMaybe)
import Language.ISOZ.Parser.SynTransformerOne (getAnnTerm, setAnnTerm, getAnnTermAndRemoveExprInPars)

-- | Traverse the syntax tree.
synTraverseExprTree :: SynTraverseExpr a => a -> Either String a 
synTraverseExprTree = \s -> case runOpTableType (traverseExpr s) of
                        Left (err) -> Left err
                        Right (Left err) -> Left err
                        Right (Right e) -> Right e

class SynTraverseExpr a where
    traverseExpr :: a -> OpTableType (Either String a)
    traverseExprList :: [a] -> OpTableType (Either String [a])
    traverseExprList [] = return (Right [])
    traverseExprList (x:xs) = do 
        m <- traverseExpr x
        case m of 
            Left err -> return (Left err)
            Right e -> do 
                n <- traverseExpr xs
                case n of
                     Left err -> return (Left err)
                     Right es -> return (Right ((:) e es))

instance SynTraverseExpr a => SynTraverseExpr [a] where
  traverseExpr = traverseExprList

{- |
 1. Application (Expr1 Expr2) binds more tightly than Function Application (prefix, postfix, infix, and nofix) handled in *.y by Expr18 and Expr19 
 2. Function Application

    - Cartesian product & Infix < Powerset & Prefix < Postfix  (loose --> tight) handled in *.y by Expr16, Expr17, Expr18 and Expr19 
 3. Infix application (precedence for difference operators, and associativity for the same operators) 

    - For expression: a op1 b op2 c   

        * if prec(op1) > prec(op2), then ((a op1 b) op2 c).
        * if prec(op1) < prec(op2), then (a op1 (b op2 c)).
        * if prec(op1) = prec(op2), then

            - if op1 != op2 then error("two difference operators should have the difference precedence)
            - if op1 = op2 then  

                * if op1 is right-associative, (a op1 (b op1 c))
                * if op1 is left-associative,  ((a op1 b) op1 c)

   - Infix is left-associative by default (given in the production for Expr16 in *.y) 
   
   @
   Expr16 : Expr16 'cross' Expr17 { AbsIsoz.ProdExpr }
          | Expr16 L_I Expr17 { AbsIsoz.FuncApplExpr }
          | Expr16 L_EL ExpSep ERESRE_E Expr17 { AbsIsoz.FuncApplExpr }
   @
 -}
instance SynTraverseExpr Expression where
  traverseExpr e = case e of
    NumExpr ann name -> return (Right e)
    ForallExpr ann schtext expr -> do
            sch <- (traverseExpr schtext)
            n <- (traverseExpr expr) 
            case sch of
                Left err -> return (Left err)
                Right sch1  -> case n of
                    Left err1 -> return (Left err1)
                    Right s  -> return (Right (ForallExpr ann sch1 s))
    ExistsExpr ann schtext expr -> do
            sch <- (traverseExpr schtext)
            n <- (traverseExpr expr) 
            case sch of
                Left err -> return (Left err)
                Right sch1  -> case n of
                    Left err1 -> return (Left err1)
                    Right s  -> return (Right (ExistsExpr ann sch1 s))
    Exists1Expr ann schtext expr -> do -- 
            sch <- (traverseExpr schtext)
            n <- (traverseExpr expr) 
            case sch of
                Left err -> return (Left err)
                Right sch1  -> case n of
                    Left err1 -> return (Left err1)
                    Right s  -> return (Right (Exists1Expr ann sch1 s))
    -- LambdaExpr ann schtext expr -> SetCompExpr ann (traverse1 schtext) (TupleExtExpr ann (chartuple (traverse1 schtext)) [expr])
    MuExpr ann schtext expr -> do -- 
            sch <- (traverseExpr schtext)
            n <- (traverseExpr expr) 
            case sch of
                Left err -> return (Left err)
                Right sch1  -> case n of
                    Left err1 -> return (Left err1)
                    Right s  -> return (Right (MuExpr ann sch1 s))
    LocalDefExpr ann decllist expr -> do
            dl <- (traverseExpr decllist)
            n <- (traverseExpr expr) 
            case dl of
                Left err -> return (Left err)
                Right dl' -> case n of 
                    Left err1 -> return (Left err1)
                    Right s  -> return (Right (LocalDefExpr ann dl' s))
    EquivExpr ann expr1 expr2 -> do
            e1 <- (traverseExpr expr1)
            e2 <- (traverseExpr expr2) 
            case e1 of
                Left err -> return (Left err)
                Right e1' -> case e2 of 
                    Left err1 -> return (Left err1)
                    Right e2'  -> return (Right (EquivExpr ann e1' e2')) 
    ImplExpr ann expr1 expr2 -> do
            e1 <- (traverseExpr expr1)
            e2 <- (traverseExpr expr2) 
            case e1 of
                Left err -> return (Left err)
                Right e1' -> case e2 of 
                    Left err1 -> return (Left err1)
                    Right e2'  -> return (Right (ImplExpr ann e1' e2')) 
    AndExpr ann expr1 expr2 -> do
            e1 <- (traverseExpr expr1)
            e2 <- (traverseExpr expr2) 
            case e1 of
                Left err -> return (Left err)
                Right e1' -> case e2 of 
                    Left err1 -> return (Left err1)
                    Right e2'  -> return (Right (AndExpr ann e1' e2'))
    OrExpr ann expr1 expr2 -> do
            e1 <- (traverseExpr expr1)
            e2 <- (traverseExpr expr2) 
            case e1 of
                Left err -> return (Left err)
                Right e1' -> case e2 of 
                    Left err1 -> return (Left err1)
                    Right e2'  -> return (Right (OrExpr ann e1' e2'))
    NegExpr ann expr -> do
            e1 <- (traverseExpr expr)
            case e1 of
                Left err -> return (Left err)
                Right e1' -> return (Right (NegExpr ann e1'))
    CondExpr ann pred expr1 expr2 -> do 
            p <- (traverseExpr pred)
            e1 <- (traverseExpr expr1)
            e2 <- (traverseExpr expr2)
            case p of
                Left err -> return (Left err)
                Right p' -> case e1 of
                    Left err -> return (Left err)
                    Right e1' -> case e2 of 
                        Left err -> return (Left err)
                        Right e2'  -> return (Right (CondExpr ann p' e1' e2'))
    CompExpr ann expr1 expr2 -> do
            e1 <- (traverseExpr expr1)
            e2 <- (traverseExpr expr2) 
            case e1 of
                Left err -> return (Left err)
                Right e1' -> case e2 of 
                    Left err1 -> return (Left err1)
                    Right e2'  -> return (Right (CompExpr ann e1' e2'))
    PipeExpr ann expr1 expr2 -> do
            e1 <- (traverseExpr expr1)
            e2 <- (traverseExpr expr2) 
            case e1 of
                Left err -> return (Left err)
                Right e1' -> case e2 of 
                    Left err1 -> return (Left err1)
                    Right e2'  -> return (Right (PipeExpr ann e1' e2'))
    HideExpr ann expr namelist -> do
            e1 <- (traverseExpr expr)
            case e1 of
                Left err -> return (Left err)
                Right e1' -> return (Right (HideExpr ann e1' namelist))
    ProjExpr ann expr1 expr2 -> do
            e1 <- (traverseExpr expr1)
            e2 <- (traverseExpr expr2) 
            case e1 of
                Left err -> return (Left err)
                Right e1' -> case e2 of 
                    Left err1 -> return (Left err1)
                    Right e2'  -> return (Right (ProjExpr ann e1' e2'))
    PreExpr ann expr -> do
            e1 <- (traverseExpr expr)
            case e1 of
                Left err -> return (Left err)
                Right e1' -> return (Right (PreExpr ann e1'))
{- The precedence of product (cross) is 8 according to the standard
 - 
 - 1. (expr11 'op1' expr12) x (expr21 'op2' expr22)
 -      if prec(op1) > 8 && prec(op2) > 8, keep the same: (expr11 'op1' expr12) x (expr21 'op2' expr22)
 -      if prec(op1) < 8 && prec(op2) > 8, (expr11 'op1' (expr12 x (expr21 'op2' expr22)))
 -      if prec(op1) > 8 && prec(op2) < 8, (((expr11 'op1' expr12) x expr21) 'op2' expr22)
 -      if prec(op1) < 8 && prec(op2) < 8, (expr11 'op1' ((expr12 x expr21) 'op2' expr22))
 -                  traverseExpr (FuncApplExpr (InfixApp 
 -                      (InInfixApp expr11 
 -                                  'op1' 
 -                                  (FuncApplExpr (InfixApp 
 -                                      (InInfixApp (ProdExpr [expr12, expr21]) 'op2' expr22)))
 -                      )))
 - 2. (expr11 'op1' expr12) x (expr2)   (where expr2 denotes the expression other than InfixApp)
 -      if prec(op1) > 8, keep the same: (expr11 'op1' expr12) x (expr2)
 -      if prec(op1) < 8, (expr11 'op1' (expr12 x expr2))
 - 3. (expr1) x (expr21 'op2' expr22)   (where expr1 denotes the expression other than InfixApp)
 -      if prec(op2) > 8, keep the same: (expr1) x (expr21 'op2' expr22)
 -      if prec(op2) < 8, ((expr1 x expr21) 'op2' expr22)
 - 4. (expr1) x (expr2)   (where expr1 and expr2 denote the expressions other than InfixApp)
 -      (expr1) x (expr2)
 -
 - In sum,
 -}
    ProdExpr ann exprlist -> do
        r <- rightTrans (ProdExpr ann exprlist) []
        case r of
            Left err -> return (Left err)
            Right e' -> return (Right e')
            -- e1 <- (traverseExpr exprlist)
            -- case e1 of
            --     Left err -> return (Left err)
            --     Right e1' -> do --return (Right (ProdExpr ann e1'))
            --         r <- rightTrans (ProdExpr ann e1') []
            --         case r of
            --             Left err -> return (Left err)
            --             Right e' -> return (Right e')
    PowerExpr ann expr -> do
            e1 <- (traverseExpr expr)
            case e1 of
                Left err -> return (Left err)
                Right e1' -> return (Right (PowerExpr ann e1'))
    -- an ApplExpr can be infix, prefix, postfix and nofix
    FuncApplExpr ann app -> 
        -- only apply rightTrans to infix
        if isInfix e 
        then do r <- rightTrans (FuncApplExpr ann app) []; 
                case r of
                    Left err -> return (Left err)
                    Right e' -> return (Right e') 
        else do r <- traverseExpr app 
                case r of
                    Left err -> return (Left err)
                    Right app' -> return (Right (FuncApplExpr ann app'))
    ApplExpr ann expr1 expr2 -> do
            e1 <- (traverseExpr expr1)
            e2 <- (traverseExpr expr2) 
            case e1 of
                Left err -> return (Left err)
                Right e1' -> case e2 of 
                    Left err1 -> return (Left err1)
                    Right e2'  -> return (Right (ApplExpr ann e1' e2'))
    DecorExpr ann expr decor -> do
            e1 <- (traverseExpr expr)
            case e1 of
                Left err -> return (Left err)
                Right e1' -> return (Right (DecorExpr ann e1' decor))
    RenameExpr ann expr namepairlist -> do
            e1 <- (traverseExpr expr)
            case e1 of
                Left err -> return (Left err)
                Right e1' -> return (Right (RenameExpr ann e1' namepairlist))
    BindSelExpr ann expr refname -> do
            e1 <- (traverseExpr expr)
            case e1 of
                Left err -> return (Left err)
                Right e1' -> return (Right (BindSelExpr ann e1' refname))
    TupleSelExpr ann expr numeral -> do
            e1 <- (traverseExpr expr)
            case e1 of
                Left err -> return (Left err)
                Right e1' -> return (Right (TupleSelExpr ann e1' numeral))
    ThetaExpr ann expr strokelist -> do
            e1 <- (traverseExpr expr)
            case e1 of
                Left err -> return (Left err)
                Right e1' -> return (Right (ThetaExpr ann e1' strokelist))
    RefExpr ann refname -> return (Right e)
    GenRefExpr ann refname exprlist -> do
            e1 <- (traverseExpr exprlist)
            case e1 of
                Left err -> return (Left err)
                Right e1' -> return (Right (GenRefExpr ann refname e1'))
    SetExpr ann exprlist -> do
            e1 <- (traverseExpr exprlist)
            case e1 of
                Left err -> return (Left err)
                Right e1' -> return (Right (SetExpr ann e1'))
    SetCompExpr ann schematext expr -> do
            sch <- (traverseExpr schematext)
            n <- (traverseExpr expr) 
            case sch of
                Left err -> return (Left err)
                Right sch1  -> case n of
                    Left err1   -> return (Left err1)
                    Right s     -> return (Right (SetCompExpr ann sch1 s))
    -- CharSetCompExpr ann schematext -> 
    SchemaExpr ann schematext -> do
            sch <- (traverseExpr schematext)
            case sch of
                Left err -> return (Left err)
                Right sch1  -> return (Right (SchemaExpr ann sch1))
    BindExtExpr ann decllist -> do
            dl <- (traverseExpr decllist)
            case dl of
                Left err    -> return (Left err)
                Right dl'   -> return (Right (BindExtExpr ann dl'))
    TupleExtExpr ann expr1 exprlist -> do
            e1 <- (traverseExpr expr1)
            e2 <- (traverseExpr exprlist) 
            case e1 of
                Left err -> return (Left err)
                Right e1' -> case e2 of
                    Left err1 -> return (Left err1)
                    Right e2'  -> return (Right (TupleExtExpr ann e1' e2'))
    -- CharMuExpr ann schematext -> 
--  traverseExprList [] = return (Right [])
--  traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)

{- |
Compare two infix operators' precedences and associativity

- if prec(op1) > prec(op2), return 1 (op1 binds tighter than op2) 
- if prec(op1) < prec(op2), return 0 (op2 binds tighter than op1) 
- if prec(op1) = prec(op2), and 

    * if assoc1 == assoc2, and 
    
        - leftassoc        return 1 (op1 binds tighter than op2) 
        - rightassoc       return 0 (op2 binds tighter than op1) 
        
    * else                 return -1 (error ("different associativities shouldn't be used in the same precedence!"))
 -}
compareOpPrecAssoc :: String -> String -> OpTableType (Either String Int)
compareOpPrecAssoc op1 op2 = do
        r1 <- getPrecAndAssoc op1
        r2 <- getPrecAndAssoc op2
        case r1 of
            Left err                -> return (Left err)
            Right (prec1, assoc1)   -> case r2 of
                Left err                -> return (Left err)
                Right (prec2, assoc2)   -> case (comp prec1 prec2) of
                    1       -> return (Right 1)
                    -1      -> return (Right 0)
                    0       -> if assoc1 == assoc2
                               then 
                                    if assoc1 == "leftassoc"
                                    then return (Right 0)
                                    else return (Right 1)
                               else return (Right (-1))
    where comp p1 p2 
            | p1 > p2     = 1
            | p1 == p2    = 0
            | otherwise   = -1 

{- |
Return (precedence, associativity) of an operator

@
getPrecAndAssoc "+" == (30, "leftassoc")
getPrecAndAssoc "-" == (30, "leftassoc")
@
 -}
getPrecAndAssoc :: String -> OpTableType (Either String (Int, String))
getPrecAndAssoc a = do
    m <- getOpTableType
    case (Map.lookup a m) of
        Nothing -> return (Left (err2Msg (ErrParser (EParseOpNotDef ("[" ++ a ++ "] cannot be found in the operator table.")))))
        Just f1 ->  let (_, prec, assoc) = formatAnOpEntry1 f1 
                    in case ((readMaybe prec) :: Maybe Int) of
                        Nothing -> return (Left (err2Msg (ErrParser (EParseOpInvalidPrec ("Parse precedence [" ++ prec ++ "] error!"))))) 
                        Just p  -> return (Right (p, assoc))
                
{- |
Parse an operator's definition to a triple ("relation" or "function" or "generic", precedence, associativity)

For example, ("+", "function:30:leftassoc") and ("ℙ", "generic::")

@
formatAnOpEntry1 "function:30:leftassoc"    == ("function", "30", "leftassoc")
formatAnOpEntry1 "generic::"                == ("generic", "", "")
@
 -}
formatAnOpEntry1 :: String -> (String, String, String)
formatAnOpEntry1 f = if isPrefixOf "relation" f
                    then ("relation", "", "")
                    else if isPrefixOf "function" f
                         then ("function", (takeWhile (/= ':') (drop 9 f)),  (drop 1 (dropWhile (/= ':') (drop 9 f))))
                         else if isPrefixOf "generic" f
                              then ("generic", (takeWhile (/= ':') (drop 8 f)),  (drop 1 (dropWhile (/= ':') (drop 8 f))))
                              else ("error","","")

{- | 
Check if an expression is an infix or not. Here we only consider __/InfixApp and ProdExpr/__ as infix expressions, since other expressions have been parsed in *.y.
-}
isInfix :: Expression -> Bool
isInfix (ProdExpr ann exprlist) = True 
isInfix (FuncApplExpr ann11 (InfixApp ann12 (InInfixApp ann13 expr11 op1 expr12))) = True
isInfix (FuncApplExpr ann11 (InfixApp ann1 (ELInfixApp ann2 expr1 op esss_e eresre_e expr2))) = True
isInfix _ = False

-- | Check if an expression is within a parenthesis, such as (a + b)
isExprInPars :: Expression -> Bool
isExprInPars = isExprInParFromListAnns . getAnnTerm  

-- | Get an infix expression's tuple (left expr, op, right expr) 
--
-- @
-- splitExpr (ProdExpr ann [e1, e2])  == (e1, "x", e2)
-- splitExpr (FuncApplExpr ann11 (InfixApp ann12 (InInfixApp ann13 expr11 op1 expr12)))  == (expr11, op1, expr12)
-- @
splitExpr :: Expression -> (Expression, String, Expression)  
splitExpr (ProdExpr ann (e1:e2:xs)) = (e1, zChar_cross, e2)  
splitExpr (FuncApplExpr ann11 (InfixApp ann12 (InInfixApp ann13 expr11 op1 expr12))) = (expr11, op1, expr12)
splitExpr (FuncApplExpr ann11 (InfixApp ann12 (ELInfixApp ann13 expr11 op1 esss_e eresre_e expr12))) = (expr11, op1, expr12)
splitExpr (FuncApplExpr ann11 (PrefixApp ann12 (PrePrefixApp ann13 pre e))) = (NullExpr [], pre, e)
splitExpr (FuncApplExpr ann11 (PrefixApp ann12 (LPrefixApp ann13 pre esss_e eresre_e e))) = (NullExpr [], pre, e)
splitExpr (FuncApplExpr ann11 (PostfixApp ann12 (PostPostfixApp ann13 e post))) = (e, post, NullExpr [])
splitExpr (FuncApplExpr ann11 (PostfixApp ann12 (ELPostfixApp ann13 e post esss_e ersr_e))) = (e, post, NullExpr [])
splitExpr (FuncApplExpr ann11 (NofixApp ann12 (LNofixApp ann13 nof esss_e ersr_e))) = (NullExpr [], nof, NullExpr [])
splitExpr e = (e, "", e)

-- | Replace an infix's left expression by the specified expression, and then return new expression
replaceLeftExpr :: Expression -> Expression -> Expression
replaceLeftExpr (ProdExpr ann (e1:e2:xs)) e' = (ProdExpr ann (e':e2:xs)) 
replaceLeftExpr (FuncApplExpr ann11 (InfixApp ann12 (InInfixApp ann13 expr11 op1 expr12))) e' = (FuncApplExpr ann11 (InfixApp ann12 (InInfixApp ann13 e' op1 expr12)))
replaceLeftExpr (FuncApplExpr ann11 (InfixApp ann12 (ELInfixApp ann13 expr11 op1 esss_e eresre_e expr12))) e' = (FuncApplExpr ann11 (InfixApp ann12 (ELInfixApp ann13 e' op1 esss_e eresre_e expr12)))
replaceLeftExpr e e' = e' 

-- | Replace an infix's right expression by the specified expression, and then return new expression
replaceRightExpr :: Expression -> Expression -> Expression
replaceRightExpr (ProdExpr ann (e1:e2:xs)) e' = (ProdExpr ann (e1:e':xs)) 
replaceRightExpr (FuncApplExpr ann11 (InfixApp ann12 (InInfixApp ann13 expr11 op1 expr12))) e' = (FuncApplExpr ann11 (InfixApp ann12 (InInfixApp ann13 expr11 op1 e')))
replaceRightExpr (FuncApplExpr ann11 (InfixApp ann12 (ELInfixApp ann13 expr11 op1 esss_e eresre_e expr12))) e' = (FuncApplExpr ann11 (InfixApp ann12 (ELInfixApp ann13 expr11 op1 esss_e eresre_e e')))
replaceRightExpr e e' = e' 

-- | Replace all left expressions of a list of parents by the specified expression. 
-- Please note: __(where parent shall be the left expression of grandparent)__
--
-- For example, 
--
-- @
-- replaceInParent [e1@(e11 op1 e12), (e1 op2 e22)] e   == [e1'@(e op1 e12), (e1' op2 e22)] 
-- replaceInParent [e1@(e11 op1 e12), e2@(e1 op2 e22), (e2 op3 e32)] e   == [e1'@(e op1 e12), e2'@(e1' op2 e22), (e2' op3 e32)] 
-- @
replaceInParent :: [Expression] -> Expression -> [Expression]
replaceInParent [] e = []
--   -- if the parent is an infix
--   replaceInParent ((FuncApplExpr ann11 (InfixApp ann12 (InInfixApp ann13 expr11 op1 expr12))):xs) e = 
--           (FuncApplExpr ann11 (InfixApp ann12 (InInfixApp ann13 e op1 expr12))): (replaceInParent xs (FuncApplExpr ann11 (InfixApp ann12 (InInfixApp ann13 e op1 expr12))))
--   -- if the parent is a ProdExpr 
--   replaceInParent ((ProdExpr ann (e1:e2:[])):xs) e = 
--           (ProdExpr ann (e:e2:[])): (replaceInParent xs (ProdExpr ann (e:e2:[])))
--
replaceInParent pars e = if isInfix (head pars)
                         then (e' : (replaceInParent (tail pars) e'))
                         else pars
    -- new left expression
    where e' = (replaceLeftExpr (head pars) e)

-- | The implementation of the RIGHT-SUBORDINATE algorithm 
--
-- The brief description of the algorithm:
--
-- @
-- rightSub (if q is the parent of p)
--      while (parents is not empty && the precedence of the q is larger than that of the op in current p)
--          right subordination 
--              p' = E1p Op (E2p Oq E2q) 
--              remove q from parents, and then replace all q in its parents as p'
-- @
--
-- Reference: /Handling Operator Precedence in Arithmetic Expression with Tree Transformations/ by WlLF R. LALONDE and JIM DES RlVlERES 
--
rightSub :: Expression -> [Expression] -> OpTableType (Either String (Expression, [Expression]))
rightSub e [] = return (Right (e, []))
-- rightSub (FuncApplExpr ann11 (InfixApp ann12 (InInfixApp ann13 expr11 op1 expr12))) pars = 
--     case (head pars) of
--         (FuncApplExpr ann21 (InfixApp ann22 (InInfixApp ann23 expr21 op2 expr22))) -> do
--             r <- (compareOpPrecAssoc op2 op1)
--             case r of
--                 Left err -> return (Left err)
--                 Right r' -> case r' of
--                     1   -> 
--                         -- how to replace all p by p' (Ep1 Op (Ep2 Oq Eq2)) where Eq1 should be p
--                         let p' = (FuncApplExpr ann11 (InfixApp ann12 (InInfixApp ann13 expr11 op1 (FuncApplExpr ann21 (InfixApp ann22 (InInfixApp ann23 expr12 op2 expr22))))))
--                         in rightSub p' (replaceInParent (tail pars) p')
--                     0   -> return (Right ((FuncApplExpr ann11 (InfixApp ann12 (InInfixApp ann13 expr11 op1 expr12))), pars))
--                     -1  -> return (Left ("different associativities shouldn't be used in the same precedence!"))
rightSub e pars =   if (isInfix e) && (isInfix (head pars))
                    then do
                        r <- (compareOpPrecAssoc opq opp)
                        case r of
                            Left err -> return (Left err)
                            Right r' -> case r' of
                                1   -> 
                                    -- how to replace all p by p' (Ep1 Op (Ep2 Oq Eq2)) where Eq1 should be p
                                    let q' = replaceLeftExpr (head pars) expr2p
                                    in let p' = replaceRightExpr e q'
                                           pars' = (replaceInParent (tail pars) p')
                                       in rightSub p' pars' 
                                0   -> return (Right (e, pars))
                                -1  -> return (Left (err2Msg (ErrParser (EParseDiffAssocUsed ("different associativities shouldn't be used in the same precedence!")))))
                    else return (Right (e, pars)) 
    where (exp1p, opp, expr2p) = splitExpr e
          (exp1q, opq, expr2q) = splitExpr (head pars)

-- | Return a final expression from current expression p and its parents
--
-- - return e if parents is empty
-- - return the last one in pars: it is the transformed one, if parents is not empty
--
-- @ 
-- returnExpr rootE []       == rootE
-- returnExpr e11 [(e11 op1 e12), ((e11 op1 e12) op2 e22), (((e11 op1 e12) op2 e22) op3 e32)] == (((e11 op1 e12) op2 e22) op3 e32)
-- @ 
returnExpr :: Expression -> [Expression] -> Expression
returnExpr e [] = e
returnExpr e pars = last pars

{-| Right transformation function.

- 1. for the expression within a parenthesis, such as (e).  Just transform this e, then return it 
- 2. otherwise, 

    * if it is an infix, transform its right expression (since we assume right expression is not involved in expression transformation according to precedence and associativities).  

        - if it is the root expression, just shift 
        - if it is not the root expression, call rightSub and then shift

    * if it is not an infix, transform e and then return transformed expression 
   
 -}
rightTrans :: Expression    -- ^ current expression to be processed
        -> [Expression]     -- ^ all parents that have been visited 
        -> OpTableType (Either String Expression) -- ^ return transformed expression
rightTrans e pars 
    -- This is an expression in a parenthesis therefore it binds strongest and only need 
    -- to transform this expression and then replace e in all parents like others 
    | isExprInPars e    = do
            r <- traverseExpr (setAnnTerm (getAnnTermAndRemoveExprInPars e) e)
            case r of
                Left err -> return (Left err)
                Right e' -> return (Right (returnExpr e' (replaceInParent pars e')))
    | otherwise         =
        if (isInfix e) 
        then let (left, op, right) = splitExpr e
             in do 
                    r <- traverseExpr right -- visit its right expression
                    case r of
                        Left err -> return (Left err)
                        Right right' -> if (null pars) 
                                        then rightTrans e1 [replaceRightExpr e right'] -- shift
                                        else do
                                               r <- rightSub (replaceRightExpr e right') pars 
                                               case r of
                                                   Left err -> return (Left err)
                                                   Right (p', pars') -> let (e1p', op', e2p') = splitExpr p'
                                                                        in rightTrans e1p' (p':pars')  -- shift
        else do
            r <- traverseExpr e
            case r of
                Left err -> return (Left err)
                Right e' -> return (Right $ returnExpr e' pars) 
    where (e1, op, e2) = splitExpr e

-- transformExpr :: Expression -> OpTableType (Either String Expression)
-- transformExpr (ProdExpr ann (e1:e2:[])) = case e1 of
--         (FuncApplExpr ann (InfixApp ann1 (InInfixApp ann2 expr1 op expr2))) -> do
--                         r <- (compareOpPrecAssoc zChar_cross op)
--                         case r of
--                             Left err -> return (Left err)
--                             Right r' -> case r' of
--                                 -- "x" > op
--                                 -- (e11 op e12) x e2  => (e11 op (e12 x e2))
--                                 1   -> transformExpr (FuncApplExpr ann (InfixApp ann1 (InInfixApp ann2 expr1 op (ProdExpr ann (expr2:e2:[])))))  
--                                 -- "x" < op
--                                 -- (e11 op e12) x e2  => (e11 op e12) x e2
--                                 0   -> do
--                                     r <- transformExpr 
--                                 -- error 
--                                 -1  -> return (Left ("different associativities shouldn't be used in the same precedence!"))
-- transformExpr (ProdExpr ann exprlist) =  
-- transformExpr (FuncApplExpr ann (InfixApp ann1 (InInfixApp ann2 expr1 op expr2))) = 
-- transformExpr (FuncApplExpr ann (InfixApp ann1 (ELInfixApp ann2 expr1 op esss_e eresre_e expr2))) = 
-- transformExpr e = e 

instance SynTraverseExpr Specification where
  traverseExpr e = case e of
    SpecSect ann sections -> do
            setOpTableType (opmap) 
            n <- (traverseExpr sections) ; case n of
                Left err -> return (Left err)
                Right s  -> return (Right (SpecSect ann s))
      where opmap = getOpMapAnnFromListAnns ann 
    -- 12.2.1.1
    SpecAnony ann paragraphs -> do
            setOpTableType (opmap) 
            n <- (traverseExpr paragraphs) 
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (SpecAnony ann s)) 
      where opmap = getOpMapAnnFromListAnns ann 
    SpecEmpty ann -> return (Right (SpecEmpty ann))

instance SynTraverseExpr Section where
  traverseExpr e = case e of
    BaseSection ann zed sectname end paragraphs -> do
            n <- (traverseExpr paragraphs) 
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (BaseSection ann zed sectname end s)) 
    InheritingSection ann zed sectname listname end paragraphs -> do
            n <- (traverseExpr paragraphs) 
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (InheritingSection ann zed sectname listname end s)) 
--  traverseExprList [] = []
--  traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)

instance SynTraverseExpr Paragraph where
  traverseExpr e = case e of
    GivenPara ann listname -> return (Right (GivenPara ann listname))
    AxdefPara ann ax schematext end -> do
            n <- (traverseExpr schematext) 
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (AxdefPara ann ax s end)) 
    SchdefPara ann sch name schematext end -> do
            n <- (traverseExpr schematext) 
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (SchdefPara ann sch name s end)) 
    GenAxdefPara ann ax formals schematext end -> do
            n <- (traverseExpr schematext) 
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (GenAxdefPara ann ax formals s end)) 
    GenSchdefPara ann sch formals name schematext end -> do
            n <- (traverseExpr schematext) 
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (GenSchdefPara ann sch formals name s end)) 
    GenConjecturePara ann name formals pred -> do
            n <- (traverseExpr pred)
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (GenConjecturePara ann name formals s)) 
    HDefPara ann name expr -> do
            n <- (traverseExpr expr)
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (HDefPara ann name s)) 
    GenHDefPara ann name formals expr -> do
            n <- (traverseExpr expr)
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (GenHDefPara ann name formals s)) 
    GenOpDefPara ann genname expr -> do
            n <- (traverseExpr expr)
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (GenOpDefPara ann genname s)) 
    FreetypePara ann listtype -> do
            n <- (traverseExpr listtype)
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (FreetypePara ann s)) 
    ConjecturePara ann name pred -> do
            n <- (traverseExpr pred)
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (ConjecturePara ann name s)) 
    OperatorPara ann optemplate -> do
            n <- (traverseExpr optemplate)
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (OperatorPara ann s)) 
    ChannelPara ann cdecl -> do
            n <- (traverseExpr cdecl)
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (ChannelPara ann s)) 
    ChannelFromPara ann cdecl -> do
            n <- (traverseExpr cdecl)
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (ChannelFromPara ann s)) 
    ChannelSetPara ann cdecl -> do 
            n <- (traverseExpr cdecl)
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (ChannelSetPara ann s)) 
--  traverseExprList [] = []
--  traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr Freetype where
  traverseExpr e = case e of
    Freetype ann name listbranches -> do
            n <- (traverseExpr listbranches)
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (Freetype ann name s)) 
--  traverseExprList [] = []
--  traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr Branch where
  traverseExpr e = case e of
    ConstantBranch ann declname -> do
            n <- (traverseExpr declname)
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (ConstantBranch ann s)) 
    ConstructorBranch ann declname expr -> do
            dl <- (traverseExpr declname)
            n <- (traverseExpr expr)
            case dl of
                Left err -> return (Left err)
                Right dl'  -> case n of
                    Left err -> return (Left err)
                    Right s  -> return (Right (ConstructorBranch ann dl' s)) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr Formals where
  traverseExpr e = case e of
    Formals ann listname -> return (Right e)
-- 
instance SynTraverseExpr Predicate where
  traverseExpr e = case e of
    AndPred ann p1 p2 -> do
        p1r <- traverseExpr p1
        p2r <- traverseExpr p2
        case p1r of
            Left err -> return (Left err)
            Right p1r'  -> case p2r of
                Left err -> return (Left err)
                Right p2r'  -> return (Right (AndPred ann p1r' p2r'))
    OrPred ann p1 p2 ->  do
        p1r <- traverseExpr p1
        p2r <- traverseExpr p2
        case p1r of
            Left err -> return (Left err)
            Right p1r'  -> case p2r of
                Left err -> return (Left err)
                Right p2r'  -> return (Right (OrPred ann p1r' p2r'))
    ForallPred ann schtext p -> do
        sch <- traverseExpr schtext 
        pr <- traverseExpr p
        case sch of
            Left err -> return (Left err)
            Right sch'  -> case pr of
                Left err -> return (Left err)
                Right pr'  -> return (Right (ForallPred ann sch' pr'))
    ExistsPred ann schtext p -> do
        sch <- traverseExpr schtext 
        pr <- traverseExpr p
        case sch of
            Left err -> return (Left err)
            Right sch'  -> case pr of
                Left err -> return (Left err)
                Right pr'  -> return (Right (ExistsPred ann sch' pr'))
    Exists1Pred ann schtext p -> do
        sch <- traverseExpr schtext 
        pr <- traverseExpr p
        case sch of
            Left err -> return (Left err)
            Right sch'  -> case pr of
                Left err -> return (Left err)
                Right pr'  -> return (Right (Exists1Pred ann sch' pr'))
    EquivPred ann p1 p2 -> do
        p1r <- traverseExpr p1
        p2r <- traverseExpr p2
        case p1r of
            Left err -> return (Left err)
            Right p1r'  -> case p2r of
                Left err -> return (Left err)
                Right p2r'  -> return (Right (EquivPred ann p1r' p2r'))
    ImplPred ann p1 p2 -> do
        p1r <- traverseExpr p1
        p2r <- traverseExpr p2
        case p1r of
            Left err -> return (Left err)
            Right p1r'  -> case p2r of
                Left err -> return (Left err)
                Right p2r'  -> return (Right (ImplPred ann p1r' p2r'))
    NegPred ann p -> do 
        pr <- traverseExpr p
        case pr of
            Left err -> return (Left err)
            Right pr'  -> return (Right (NegPred ann pr'))
    RelPred ann relation -> do
        pr <- traverseExpr relation 
        case pr of
            Left err -> return (Left err)
            Right pr'  -> return (Right (RelPred ann pr'))
    ExprPred ann e -> do 
        pr <- traverseExpr e 
        case pr of
            Left err -> return (Left err)
            Right pr'  -> return (Right (ExprPred ann pr'))
    TruePred ann -> return (Right (TruePred ann))
    FalsePred ann -> return (Right e)
    MemPred ann expr1 expr2 -> do
        p1r <- traverseExpr expr1
        p2r <- traverseExpr expr2 
        case p1r of
            Left err -> return (Left err)
            Right p1r'  -> case p2r of
                Left err -> return (Left err)
                Right p2r'  -> return (Right (MemPred ann p1r' p2r'))
    _           -> return (Right e)
--  traverseExprList [] = []
--  traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr RenamePair where
  traverseExpr e = case e of
    RenamePair ann newname oldname -> return (Right e)
--  traverseExprList [] = []
--  traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr SchemaText where
  traverseExpr e = case e of
    SchemaTextEmpty ann -> return (Right e) 
    SchemaTextDecl ann decl -> do
        dl <- traverseExpr decl 
        case dl of
            Left err    -> return (Left err)
            Right dl'   -> return (Right (SchemaTextDecl ann dl')) 
    SchemaTextPred ann pred -> do
        pr <- traverseExpr pred 
        case pr of
            Left err    -> return (Left err)
            Right pr'   -> return (Right (SchemaTextPred ann pr')) 
    SchemaText ann decl pred -> do
        dl <- traverseExpr decl 
        pr <- traverseExpr pred 
        case dl of
            Left err    -> return (Left err)
            Right dl'   -> case pr of
                Left err    -> return (Left err)
                Right pr'   -> return (Right (SchemaText ann dl' pr')) 
--  traverseExprList [] = []
--  traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr DeclPart where
  traverseExpr e = case e of
    DeclPart ann decls -> do
        dl <- (traverseExpr decls) 
        case dl of
            Left err -> return (Left err)
            Right dl' -> return (Right (DeclPart ann dl'))
--  traverseExprList [] = []
--  traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr Declaration where
  traverseExpr e = case e of
    Declaration ann declnames expr -> do
        dl <- traverseExpr declnames 
        er <- traverseExpr expr
        case dl of
            Left err -> return (Left err)
            Right dl' -> case er of
                Left err -> return (Left err)
                Right er' -> return (Right (Declaration ann dl' er'))
    AbbrDecl ann declname expr -> do 
        dl <- traverseExpr declname
        er <- traverseExpr expr
        case dl of
            Left err -> return (Left err)
            Right dl' -> case er of
                Left err -> return (Left err)
                Right er' -> return (Right (AbbrDecl ann dl' er'))
    ExprDecl ann expr -> do 
        er <- traverseExpr expr 
        case er of
            Left err -> return (Left err)
            Right er' -> return (Right (ExprDecl ann er'))
--  traverseExprList [] = []
--  traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr OperatorTemplate where
  traverseExpr e = case e of
    RelationTemplate ann template -> do
        tr <- traverseExpr template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (RelationTemplate ann tr'))
    FunctionTemplate ann template -> do
        tr <- traverseExpr template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (FunctionTemplate ann tr'))
    GenericTemplate ann template -> do
        tr <- traverseExpr template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (GenericTemplate ann tr'))
--  traverseExprList [] = []
--  traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr CategoryTemplate where
  traverseExpr e = case e of
    PrefixCatTemplate ann template -> do
        tr <- traverseExpr template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (PrefixCatTemplate ann tr'))
    PostfixCatTemplate ann template -> do
        tr <- traverseExpr template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (PostfixCatTemplate ann tr'))
    InfixCatTemplate ann prec assoc template -> do
        tr <- traverseExpr template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (InfixCatTemplate ann prec assoc tr'))
    NofixCatTemplate ann template -> do
        tr <- traverseExpr template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (NofixCatTemplate ann tr'))
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr Prec where
  traverseExpr e = case e of
    Prec ann _ -> return (Right e)

instance SynTraverseExpr Assoc where
  traverseExpr e = case e of
    LeftAssoc ann -> return (Right e) 
    RightAssoc ann -> return (Right e) 
-- 
instance SynTraverseExpr Template where
  traverseExpr e = case e of
    TPrefixTemplate ann template -> do
        tr <- traverseExpr template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (TPrefixTemplate ann tr')) 
    TPostfixTemplate ann template -> do
        tr <- traverseExpr template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (TPostfixTemplate ann tr')) 
    TInfixTemplate ann template -> do
        tr <- traverseExpr template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (TInfixTemplate ann tr')) 
    TNofixTemplate ann template -> do
        tr <- traverseExpr template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (TNofixTemplate ann tr')) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr PrefixTemplate where
  traverseExpr e = case e of
    PrefixTemplate ann prefixname  -> do
        pr <- traverseExpr prefixname
        case pr of
            Left err    -> return (Left err)
            Right pr'   -> return (Right (PrefixTemplate ann pr'))
    PowerPrefixTemplate ann        -> return (Right e) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr PostfixTemplate where
  traverseExpr e = case e of
    PostfixTemplate ann postfixname  -> do
        pr <- traverseExpr postfixname
        case pr of
            Left err    -> return (Left err)
            Right pr'   -> return (Right (PostfixTemplate ann pr'))
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr InfixTemplate where
  traverseExpr e = case e of
    InfixTemplate ann infixname  -> do
        pr <- traverseExpr infixname
        case pr of
            Left err    -> return (Left err)
            Right pr'   -> return (Right (InfixTemplate ann pr'))
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr NofixTemplate where
  traverseExpr e = case e of
    NofixTemplate ann nofixname  -> do
        pr <- traverseExpr nofixname
        case pr of
            Left err    -> return (Left err)
            Right pr'   -> return (Right (NofixTemplate ann pr'))
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
-- 
instance SynTraverseExpr DeclName where
  traverseExpr e = case e of
    DName ann name -> return (Right e) 
    OpName ann name -> return (Right e)
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr RefName where
  traverseExpr e = case e of
    RName ann name -> return (Right e)
    OpRName ann name ->  return (Right e)
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr OpName where
  traverseExpr e = case e of
    PrefixOpName ann name -> return (Right e)
    PostfixOpName ann name -> return (Right e)
    InfixOpName ann name -> return (Right e)
    NofixOpName ann name -> return (Right e)
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr PrefixName where
  traverseExpr e = case e of
    PrePrefixName ann pre -> return (Right e)
    PrePPrefixName ann prep -> return (Right e) 
    LPrefixName ann l listesss eresre -> return (Right e)
    LPPrefixName ann l listesss erepsrep -> return (Right e)
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr PostfixName where
  traverseExpr e = case e of
    PostPostfixName ann post -> return (Right e)
    PostPPostfixName ann postp -> return (Right e)
    ELPostfixName ann el listesss ersr -> return (Right e)
    ELPPostfixName ann elp listesss erpsrp -> return (Right e)
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr InfixName where
  traverseExpr e = case e of
    InInfixName ann ii -> return (Right e) 
    InPInfixName ann ip -> return (Right e)
    ELInfixName ann el listesss eresre -> return (Right e)
    ELPInfixName ann elp listesss erepsrep -> return (Right e)
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr NofixName where
  traverseExpr e = case e of
    LNofixName ann l listesss ersr -> return (Right e) 
    LPNofixName ann l listesss erpsrp -> return (Right e)
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
-- instance SynTraverseExpr ESSS where
--   traverseExpr e = case e of
--     ES ann es -> ES ann (es) 
--     SS ann ss -> SS ann (ss) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
-- instance SynTraverseExpr ESSS_N where
--   traverseExpr e = case e of
--     ES_N ann name es -> e 
--     SS_N ann name ss -> e 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr ESSS_E where
  traverseExpr e = case e of
    ES_E ann expr es -> do
        er <- traverseExpr expr 
        case er of
            Left err  -> return (Left err) 
            Right er' -> return (Right (ES_E ann er' es)) 
    SS_E ann expr ss -> do
        er <- traverseExpr expr 
        case er of
            Left err  -> return (Left err) 
            Right er' -> return (Right (SS_E ann er' ss)) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
-- instance SynTraverseExpr ERESRE where
--   traverseExpr e = case e of
--     ERE ann es -> ERE ann (es) 
--     SRE ann ss -> SRE ann (ss) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
-- instance SynTraverseExpr ERESRE_N where
--   traverseExpr e = case e of
--     ERE_N ann name es -> e 
--     SRE_N ann name ss -> e 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr ERESRE_E where
  traverseExpr e = case e of
    ERE_E ann expr es -> do
        er <- traverseExpr expr 
        case er of
            Left err  -> return (Left err) 
            Right er' -> return (Right (ERE_E ann er' es)) 
    SRE_E ann expr ss -> do
        er <- traverseExpr expr 
        case er of
            Left err  -> return (Left err) 
            Right er' -> return (Right (SRE_E ann er' ss)) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
-- instance SynTraverseExpr EREPSREP where
--   traverseExpr e = case e of
--     EREP ann es -> EREP ann (es) 
--     SREP ann ss -> SREP ann (ss) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr EREPSREP_E where
  traverseExpr e = case e of
    EREP_E ann expr es -> do
        er <- traverseExpr expr 
        case er of
            Left err  -> return (Left err) 
            Right er' -> return (Right (EREP_E ann er' es)) 
    SREP_E ann expr ss -> do
        er <- traverseExpr expr 
        case er of
            Left err  -> return (Left err) 
            Right er' -> return (Right (SREP_E ann er' ss)) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
-- instance SynTraverseExpr ERSR where
--   traverseExpr e = case e of
--     ER ann es -> ER ann (es) 
--     SR ann ss -> SR ann (ss) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
-- instance SynTraverseExpr ERSR_N where
--   traverseExpr e = case e of
--     ER_N ann name es -> e 
--     SR_N ann name ss -> e 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr ERSR_E where
  traverseExpr e = case e of
    ER_E ann expr es -> do
        er <- traverseExpr expr 
        case er of
            Left err  -> return (Left err) 
            Right er' -> return (Right (ER_E ann er' es)) 
    SR_E ann expr ss -> do
        er <- traverseExpr expr 
        case er of
            Left err  -> return (Left err) 
            Right er' -> return (Right (SR_E ann er' ss)) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
-- instance SynTraverseExpr ERPSRP where
--   traverseExpr e = case e of
--     ERP ann es -> ERP ann (es) 
--     SRP ann ss -> SRP ann (ss) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr ERPSRP_E where
  traverseExpr e = case e of
    ERP_E ann expr es -> do
        er <- traverseExpr expr 
        case er of
            Left err  -> return (Left err) 
            Right er' -> return (Right (ERP_E ann er' es)) 
    SRP_E ann expr ss -> do
        er <- traverseExpr expr 
        case er of
            Left err  -> return (Left err) 
            Right er' -> return (Right (SRP_E ann er' ss)) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr GenName where
  traverseExpr e = case e of
    PrefixGenName ann name -> return (Right e)
    PostfixGenName ann name -> return (Right e)
    InfixGenName ann name -> return (Right e)
    NofixGenName ann name -> return (Right e) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr PrefixGenName where
  traverseExpr e = case e of
    PrePrefixGenName ann pre name -> return (Right e) 
    LPrefixGenName ann l listesss_n eresre_n name -> return (Right e)
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr PostfixGenName where
  traverseExpr e = case e of
    PostPostfixGenName ann name post -> return (Right e)
    ELPostfixGenName ann name el listesss_n ersr_n -> return (Right e) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr InfixGenName where
  traverseExpr e = case e of
    InInfixGenName ann name1 ii name2 -> return (Right e)
    ELInfixGenName ann name1 el listesss_n eresre_n name2 -> return (Right e) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr NofixGenName where
  traverseExpr e = case e of
    LNofixGenName ann l listesss_n ersr_n -> return (Right e) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr Relation where
  traverseExpr e = case e of
    PrefixRel ann  rel -> do
        rr <- traverseExpr rel
        case rr of
            Left err -> return (Left err)
            Right rr' -> return (Right (PrefixRel ann rr')) 
    PostfixRel ann rel -> do
        rr <- traverseExpr rel
        case rr of
            Left err -> return (Left err)
            Right rr' -> return (Right (PostfixRel ann rr')) 
    InfixRel ann   rel -> do
        rr <- traverseExpr rel
        case rr of
            Left err -> return (Left err)
            Right rr' -> return (Right (InfixRel ann rr')) 
    NofixRel ann   rel -> do
        rr <- traverseExpr rel
        case rr of
            Left err -> return (Left err)
            Right rr' -> return (Right (NofixRel ann rr')) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr PrefixRel where
  traverseExpr e = case e of
    PrePPrefixRel ann prep expr -> do
        er <- traverseExpr expr 
        case er of
            Left err -> return (Left err)
            Right er' -> return (Right (PrePPrefixRel ann prep er')) 
    LPPrefixRel ann lp listesss_e erepsrep_e expr -> do
        esr <- traverseExpr listesss_e 
        erpr <- traverseExpr erepsrep_e 
        er <- traverseExpr expr 
        case esr of
            Left err    -> return (Left err)
            Right esr'  -> case erpr of
                Left err    -> return (Left err)
                Right erpr' -> case er of
                    Left err  -> return (Left err)
                    Right er' -> return (Right (LPPrefixRel ann lp esr' erpr' er')) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr PostfixRel where
  traverseExpr e = case e of
    PostPPostfixRel ann expr postp -> do
        er <- traverseExpr expr 
        case er of
            Left err -> return (Left err)
            Right er' -> return (Right (PostPPostfixRel ann er' postp)) 
    ELPPostfixRel ann expr elp listesss_e erpsrp_e -> do
        er <- traverseExpr expr 
        esr <- traverseExpr listesss_e 
        erpr <- traverseExpr erpsrp_e 
        case esr of
            Left err    -> return (Left err)
            Right esr'  -> case erpr of
                Left err    -> return (Left err)
                Right erpr' -> case er of
                    Left err  -> return (Left err)
                    Right er' -> return (Right (ELPPostfixRel ann er' elp esr' erpr')) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr IP_E where
  traverseExpr e = case e of
    IP_E ann str expr -> do
        er <- traverseExpr expr
        case er of
            Left err    -> return (Left err)
            Right er'   -> return (Right (IP_E ann str er')) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr InfixRel where
  traverseExpr e = case e of
    InInfixRel ann expr listip_e -> do
        er <- traverseExpr expr
        ipr <- traverseExpr listip_e 
        case er of
            Left err    -> return (Left err)
            Right er'   -> case ipr of
                Left err    -> return (Left err)
                Right ipr'  -> return (Right (InInfixRel ann er' ipr')) 
    ELPInfixRel ann expr1 elp listesss_e erepsrep_e expr2 -> do
        er1 <- traverseExpr expr1
        er2 <- traverseExpr expr2
        esr <- traverseExpr listesss_e 
        epr <- traverseExpr erepsrep_e 
        case er1 of
            Left err    -> return (Left err)
            Right er1'   -> case er2 of
                Left err    -> return (Left err)
                Right er2'   ->  case esr of
                    Left err    -> return (Left err)
                    Right esr'  -> case epr of
                        Left err    -> return (Left err)
                        Right epr'  -> return (Right (ELPInfixRel ann er1' elp esr' epr' er2')) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr NofixRel where
  traverseExpr e = case e of
    LPNofixRel ann lp listesss_e erepserp_e -> do
        esr <- traverseExpr listesss_e 
        epr <- traverseExpr erepserp_e 
        case esr of
            Left err    -> return (Left err)
            Right esr'   -> case epr of
                Left err    -> return (Left err)
                Right epr'  -> return (Right (LPNofixRel ann lp esr' epr')) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr Application where
  traverseExpr e = case e of
    PrefixApp ann  app -> do
        ar <- traverseExpr app
        case ar of
            Left err -> return (Left err)
            Right ar'-> return (Right (PrefixApp ann  ar'))
    PostfixApp ann app -> do
        ar <- traverseExpr app
        case ar of
            Left err -> return (Left err)
            Right ar'-> return (Right (PostfixApp ann ar'))
    InfixApp ann   app -> do
        ar <- traverseExpr app
        case ar of
            Left err -> return (Left err)
            Right ar'-> return (Right (InfixApp ann ar'))
    NofixApp ann   app -> do
        ar <- traverseExpr app
        case ar of
            Left err -> return (Left err)
            Right ar'-> return (Right (NofixApp ann ar'))
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr PrefixApp where
  traverseExpr e = case e of
    PrePrefixApp ann pre expr -> do
        er <- traverseExpr expr
        case er of
            Left err    -> return (Left err)
            Right er'   -> return (Right (PrePrefixApp ann pre er'))
    LPrefixApp ann l listesss_e eresre_e expr -> do
        er <- traverseExpr expr
        esr <- traverseExpr listesss_e 
        ersr <- traverseExpr eresre_e 
        case er of
            Left err    -> return (Left err)
            Right er'   -> case esr of 
                Left err    -> return (Left err)
                Right esr'  -> case ersr of
                    Left err    -> return (Left err)
                    Right ersr' -> return (Right (LPrefixApp ann l esr' ersr' er')) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr PostfixApp where
  traverseExpr e = case e of
    PostPostfixApp ann expr post -> do
        er <- traverseExpr expr
        case er of
            Left err    -> return (Left err)
            Right er'   -> return (Right (PostPostfixApp ann er' post))
    ELPostfixApp ann expr el listesss_e ersr_e -> do
        er <- traverseExpr expr
        esr <- traverseExpr listesss_e 
        ersr <- traverseExpr ersr_e 
        case er of
            Left err    -> return (Left err)
            Right er'   -> case esr of 
                Left err    -> return (Left err)
                Right esr'  -> case ersr of
                    Left err    -> return (Left err)
                    Right ersr' -> return (Right (ELPostfixApp ann er' el esr' ersr')) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr InfixApp where
  traverseExpr e = case e of
    InInfixApp ann expr1 ii expr2 -> do
        er1 <- traverseExpr expr1 
        er2 <- traverseExpr expr2 
        case er1 of
            Left err    -> return (Left err)
            Right er1'   -> case er2 of
                Left err    -> return (Left err)
                Right er2'  -> return (Right (InInfixApp ann er1' ii er2')) 
    ELInfixApp ann expr1 el listesss_e eresre_e expr2 -> do
        er1 <- traverseExpr expr1
        esr <- traverseExpr listesss_e 
        epr <- traverseExpr eresre_e 
        er2 <- traverseExpr expr2
        case er1 of
            Left err    -> return (Left err)
            Right er1'   -> case er2 of
                Left err    -> return (Left err)
                Right er2'   ->  case esr of
                    Left err    -> return (Left err)
                    Right esr'  -> case epr of
                        Left err    -> return (Left err)
                        Right epr'  -> return (Right (ELInfixApp ann er1' el esr' epr' er2')) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr NofixApp where
  traverseExpr e = case e of
    LNofixApp ann l listesss_e erse_e -> do
        er1 <- traverseExpr listesss_e
        er2 <- traverseExpr erse_e 
        case er1 of
            Left err    -> return (Left err)
            Right er1'   -> case er2 of
                Left err    -> return (Left err)
                Right er2'  -> return (Right (LNofixApp ann l er1' er2')) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr ChannelDecl where
  traverseExpr e = case e of
    ChannelDecl ann simplec -> do
        sr <- traverseExpr simplec 
        case sr of
            Left err -> return (Left err)
            Right sr'-> return (Right (ChannelDecl ann sr'))
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr SimpleCDecl where
  traverseExpr e = case e of
    SyncSimpleCDecl ann listname -> return (Right e)
    TypedSimpleCDecl ann listname expr -> do
        er <- (traverseExpr expr) 
        case er of
            Left err  -> return (Left err)
            Right er' -> return (Right (TypedSimpleCDecl ann listname er'))
    GenericTypedSimpleCDecl ann formals listname expr -> do
        er <- (traverseExpr expr) 
        case er of
            Left err  -> return (Left err)
            Right er' -> return (Right (GenericTypedSimpleCDecl ann formals listname er')) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr ChannelFromDecl where
  traverseExpr e = case e of
    ChannelFromDecl ann fromcdecl -> do
        fdr <- (traverseExpr fromcdecl) 
        case fdr of
            Left err -> return (Left err)
            Right fdr' -> return (Right (ChannelFromDecl ann fdr'))
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr FromCDecl where
  traverseExpr e = case e of
    RefFromCDecl ann expr -> do
        er <- (traverseExpr expr) 
        case er of
            Left err -> return (Left err)
            Right er' -> return (Right (RefFromCDecl ann er'))
    RefDecorFromCDecl ann expr -> do
        er <- (traverseExpr expr) 
        case er of
            Left err -> return (Left err)
            Right er' -> return (Right (RefDecorFromCDecl ann er'))
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr ChannelSetDecl where
  traverseExpr e = case e of
    ChannelSetDecl ann name expr -> do
        er <- (traverseExpr expr)
        case er of
            Left err -> return (Left err)
            Right er' -> return (Right (ChannelSetDecl ann name er'))
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverseExpr CSExp where
  traverseExpr e = case e of
    CSExpEmpty ann  -> return (Right e)
    CSExpExt ann exprs -> do
        er <- traverseExpr exprs
        case er of
            Left err -> return (Left err)
            Right er' -> return (Right (CSExpExt ann er')) 
    CSExpRef ann expr -> do
        er <- traverseExpr expr
        case er of
            Left err -> return (Left err)
            Right er' -> return (Right (CSExpRef ann er')) 
    CSExpUnion ann csexp1 csexp2  -> do
        er1 <- traverseExpr csexp1
        er2 <- traverseExpr csexp2
        case er1 of
            Left err -> return (Left err)
            Right er1' -> case er2 of
                Left err -> return (Left err)
                Right er2' -> return (Right (CSExpUnion ann er1' er2')) 
    CSExpInter ann csexp1 csexp2  -> do
        er1 <- traverseExpr csexp1
        er2 <- traverseExpr csexp2
        case er1 of
            Left err -> return (Left err)
            Right er1' -> case er2 of
                Left err -> return (Left err)
                Right er2' -> return (Right (CSExpInter ann er1' er2')) 
    CSExpDiff ann csexp1 csexp2  -> do
        er1 <- traverseExpr csexp1
        er2 <- traverseExpr csexp2
        case er1 of
            Left err -> return (Left err)
            Right er1' -> case er2 of
                Left err -> return (Left err)
                Right er2' -> return (Right (CSExpDiff ann er1' er2')) 
--   traverseExprList [] = []
--   traverseExprList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
