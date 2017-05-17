{- |
Module      : Language.ISOZ.Common.Ann
Description : 
Copyright   : (C) Kangfeng Ye, 2017
License     : BSD3
Maintainer  : ye.randall@gmail.com 
Stability   : experimental

Annotation type and constructors (see clause 10 of ISO Z Standard)

-}
module Language.ISOZ.Common.Ann where

import Language.ISOZ.Common.Type
import Data.Map (Map)
import qualified Data.Map as Map

-- bang patterns (whose name comes from pronunciation of the “!” character as “bang”)
-- With bang patterns, we can hint at strictness on any binding form, making the 
-- function strict in that variable
-- | position (offset, line, column)
data Posn = Pn !Int !Int !Int
  deriving (Eq, Ord, Show, Read)

-- | Annotation type.
data Ann = PosAnn Posn  -- ^ position annotation
         | SectTypeEnvAnn SectTypeEnv -- ^ section type environment annotation
         | SigAnn Sig -- ^ signature annotation 
         | TypeAnn Type -- ^ type annotation
         | TypeVarAnn [String]  -- ^ type variable annotation (a list of temporarily allocated type variables)
        -- | an annotation to store the operator table for all operators defined in templates, usually it only occurs in the annotation list of Specification
         | OpMapAnn (Map String String) 
         | SchNameMapAnn (Map String Bool) -- ^ a map of defined schemas
         | ExprInParAnn         -- ^ indicate this expression is the outermost expression within a parenthesis (such as (a+b))
         | ParserParDepthAnn Int   -- ^ the parenthesis depth of a ProdExpr, for example, in ((A x B) x C): the depth of the first x is equal to that of the second + 1
  deriving (Eq, Ord, Show, Read)

-- | Check if two annotations have the same type or not.
isSameTypeAnn :: Ann -> Ann -> Bool
isSameTypeAnn (PosAnn _) (PosAnn _)                   = True 
isSameTypeAnn (PosAnn _) _                            = False
isSameTypeAnn (SectTypeEnvAnn _) (SectTypeEnvAnn _)   = True 
isSameTypeAnn (SectTypeEnvAnn _) _                    = False
isSameTypeAnn (SigAnn _) (SigAnn _)                   = True 
isSameTypeAnn (SigAnn _) _                            = False
isSameTypeAnn (TypeAnn _) (TypeAnn _)                 = True 
isSameTypeAnn (TypeAnn _) _                           = False
isSameTypeAnn (TypeVarAnn _) (TypeVarAnn _)           = True 
isSameTypeAnn (TypeVarAnn _) _                        = False 
isSameTypeAnn (ExprInParAnn) (ExprInParAnn)           = True 
isSameTypeAnn (ExprInParAnn) _                        = False 
isSameTypeAnn (ParserParDepthAnn _) (ParserParDepthAnn _) = True 
isSameTypeAnn (ParserParDepthAnn _) (_)               = False
isSameTypeAnn (SchNameMapAnn _) (SchNameMapAnn _)     = True 
isSameTypeAnn (SchNameMapAnn _) (_)                   = False

{- |
Add an Ann to a list of Ann. If there has been a same type of annotation, just override it.

Assumption: no duplicate type of Ann in the list.
-}
addAnnToAnnListHelper :: Ann -> [Ann] -> [Ann]
addAnnToAnnListHelper a [] = [a] 
addAnnToAnnListHelper a (x:xs) = if isSameTypeAnn a x then (a:xs) else (x: (addAnnToAnnListHelper a xs))
 
-- | Extract line number and column number from position information
extractLineCol :: Posn -> (Int, Int)
extractLineCol (Pn _ l c) = (l, c) 

-- | Get Posn from a list of annotations 
getPosnFromListAnns :: [Ann] -> Posn
getPosnFromListAnns [] = Pn 0 0 0 
getPosnFromListAnns ((PosAnn p) : xs) = p 
getPosnFromListAnns (x : xs) = getPosnFromListAnns xs

{-  
 - Three strokes used in annotations.
 -}
-- | Operator glue stroke ⋈
opGlueSTROKE = "\x22C8"     -- ⋈

-- | Generic type stroke ♠
genTypeSTROKE = "\x2660" -- ♠ 

-- | Given type stroke ♡
givenTypeSTROKE = "\x2661" -- ♡  

{- |
Remove an Ann from a list

 - the first parameter: the original list of Ann
 - the second parameter: the visited list of Ann 
 -}
getAndRemoveTypeVarAnn :: [Ann] -> [Ann] -> ([String], [Ann])
getAndRemoveTypeVarAnn [] annlist = ([], annlist) 
getAndRemoveTypeVarAnn ((TypeVarAnn vl) : xs) annlist = (vl, annlist ++ xs) 
getAndRemoveTypeVarAnn (x : xs) annlist = getAndRemoveTypeVarAnn xs (annlist ++ [x])

-- | Get operator map annotation from a list of annotations
getOpMapAnnFromListAnns :: [Ann] -> Map String String
getOpMapAnnFromListAnns [] = (Map.empty) 
getOpMapAnnFromListAnns ((OpMapAnn p) : xs) = p 
getOpMapAnnFromListAnns (x : xs) = getOpMapAnnFromListAnns xs

-- | Remove a SchNameMapAnn annotation in a list of annotations
removeOpMapFromListAnns :: [Ann] -> [Ann] 
removeOpMapFromListAnns [] = []
removeOpMapFromListAnns ((OpMapAnn p) : xs) = removeOpMapFromListAnns xs 
removeOpMapFromListAnns (x : xs) = (x: removeOpMapFromListAnns xs)

-- | Get schema name map annotation from a list of annotations
getSchNameMapAnnFromListAnns :: [Ann] -> Map String Bool
getSchNameMapAnnFromListAnns [] = (Map.empty) 
getSchNameMapAnnFromListAnns ((SchNameMapAnn p) : xs) = p 
getSchNameMapAnnFromListAnns (x : xs) = getSchNameMapAnnFromListAnns xs

-- | Remove a SchNameMapAnn annotation in a list of annotations
removeSchNameMapFromListAnns :: [Ann] -> [Ann] 
removeSchNameMapFromListAnns [] = []
removeSchNameMapFromListAnns ((SchNameMapAnn p) : xs) = removeSchNameMapFromListAnns xs 
removeSchNameMapFromListAnns (x : xs) = (x: removeSchNameMapFromListAnns xs)

-- | Check if there is an ExprInPar annotation in a list of annotations
isExprInParFromListAnns :: [Ann] -> Bool 
isExprInParFromListAnns [] = False 
isExprInParFromListAnns ((ExprInParAnn) : xs) = True
isExprInParFromListAnns (_ : xs) = isExprInParFromListAnns xs

-- | Remove an ExprInPar annotation in a list of annotations
removeExprInParFromListAnns :: [Ann] -> [Ann] 
removeExprInParFromListAnns [] = []
removeExprInParFromListAnns ((ExprInParAnn) : xs) = removeExprInParFromListAnns xs 
removeExprInParFromListAnns (x : xs) = (x: removeExprInParFromListAnns xs)

-- | Get ParserParDepthAnn from a list of annotations
getParserParDepthFromListAnns :: [Ann] -> (Bool, Int)
getParserParDepthFromListAnns [] = (False, 0) 
getParserParDepthFromListAnns ((ParserParDepthAnn n) : xs) = (True, n) 
getParserParDepthFromListAnns (x : xs) = getParserParDepthFromListAnns xs 

-- | Get the depth of ParserParDepthAnn from a list of annotations
getParserParDepthFromListAnnsN :: [Ann] -> Int
getParserParDepthFromListAnnsN ann = case getParserParDepthFromListAnns ann of 
                                        (False, _)  -> error ("No ParserParDepthAnn in the list [" ++ (show ann) ++ "].\n")
                                        (True, n)   -> n 

-- | Remove ParserParDepthAnn from a list of annotations
removeParserParDepthFromListAnns :: [Ann] -> [Ann] 
removeParserParDepthFromListAnns [] = [] 
removeParserParDepthFromListAnns ((ParserParDepthAnn n) : xs) = removeParserParDepthFromListAnns xs 
removeParserParDepthFromListAnns (x : xs) = (x : removeParserParDepthFromListAnns xs)

