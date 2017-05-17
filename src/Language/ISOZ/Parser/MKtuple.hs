{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{- |
Module      : Language.ISOZ.Parser.Mktuple
Description : 
Copyright   : (C) Kangfeng Ye, 2017
License     : BSD3
Maintainer  : ye.randall@gmail.com 
Stability   : experimental

Syntactical transformation to cope with "Clause 9: characteristic tuple" in ISO Z Standard.

-}

module Language.ISOZ.Parser.Mktuple where

import Language.ISOZ.Common.AbsIsoz
import Data.Char

-- | Converts a schema text to an expression as stated in Clause 9.2 
--
--  chartuple t = mktuple (charac t)
chartuple :: SchemaText -> Expression 
chartuple = (mktuple . charac)

-- | Get a list of strokes from a DecorExpr. For example, s'' is parsed as (DecorExpr (DecorExpr (RefExpr s) ') ')
--
-- @
-- getListStrokes (DecorExpr (DecorExpr (RefExpr s) ') ') == ["'", "'"]
-- @
getListStrokes :: Expression -> [String]
getListStrokes (DecorExpr _ e stroke) = (:) stroke (getListStrokes e)
getListStrokes _ = []

-- | Get the expression from a DecorExpr. For example, s'' is parsed as (DecorExpr (DecorExpr (RefExpr s) ') ')
--
-- @
-- getExpr (DecorExpr (DecorExpr (RefExpr s) ') ') ==  (DecorExpr (RefExpr s) ')
-- @
getExpr :: Expression -> Expression 
getExpr (DecorExpr _ e stroke) = getExpr e
getExpr e = e

-- | Turn sequences of expressions enclosed between metalanguage brackets < and >, such as <e1, e2, e3> into an expression
--
-- Definition:
--
-- - mktuple <e> = e
-- - mktuple <e1, e2, e3> = (e1, e2, e3)
--
-- @
-- mktuple [e]              == e
-- mktuple [e1, e2, e3]     == (e1, e2, e3)
-- @
mktuple :: [Expression] -> Expression
mktuple [e] = e 
mktuple (x:xs) = TupleExtExpr [] x xs 

{- |
A class to provide /charac/ for schema texts, declarations, and declaration names

Specification: 

- charac (d1; ...; dn | p) = charac (d1; ...; dn)
- charac (d1 ; ...; dn ) = charac d1 ^ ... ^ charac dn       where n >= 1 (concatenation)
- charac () = ⟨⟨| |⟩⟩
- charac (i1, ..., in : e) = ⟨i1 , ..., in ⟩
- charac (i == e)  = ⟨i⟩
- charac (e ∗) = ⟨θe∗⟩
 -}
class Mktuple a where
    charac :: a -> [Expression] 
    characList :: [a] -> [Expression] 
    characList list = concat (map (charac) list)

instance Mktuple a => Mktuple [a] where
  charac = characList 

instance Mktuple SchemaText where
  charac t = case t of
    SchemaText _ (DeclPart _ decllist) pred -> charac decllist
    SchemaTextEmpty ann -> [BindExtExpr ann []] 
    SchemaTextDecl _ (DeclPart _ decllist) -> charac decllist
    SchemaTextPred a pred -> [BindExtExpr a []] 

instance Mktuple Declaration where
  charac t = case t of
    Declaration _ listdeclname expr -> charac listdeclname
    AbbrDecl _ declname expr -> charac declname
    ExprDecl ann expr -> case expr of
                -- a reference to a schema. might have stroke as well???
                RefExpr _ refname     -> [ThetaExpr ann expr []]  
                DecorExpr _ e stroke  -> [ThetaExpr ann (getExpr expr) (reverse (getListStrokes expr))]  
                _                   -> []
  characList [] = [] 
  characList (x:xs) = concat [(charac x), (charac xs)]

instance Mktuple DeclName where
  charac t = case t of
        DName ann name      -> [RefExpr ann (RName ann name)]
        OpName ann opname   -> [RefExpr ann (OpRName ann opname)]
  characList [] = [] 
  characList (x:xs) = concat [(charac x), (charac xs)]

