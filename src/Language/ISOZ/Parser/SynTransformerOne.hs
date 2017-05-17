{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{- |
Module      : Language.ISOZ.Parser.SynTransformerOne
Description : 
Copyright   : (C) Kangfeng Ye, 2017
License     : BSD3
Maintainer  : ye.randall@gmail.com 
Stability   : experimental

The first step of syntactical transformation to cope with some special Token or productions introduced only for temporary use.

It provides

     - synTraverse1Tree : traverse the original syntax tree 
     - getAnnTerm       : return a list of all annotations for a term
     - addAnnTerm       : add an annotation to the list. If there's a same type, just override it
     - setAnnTerm       : set the annotations of a term to the new list of annotations
-}
module Language.ISOZ.Parser.SynTransformerOne where

import Language.ISOZ.Common.AbsIsoz
import Data.Char
import Language.ISOZ.Parser.Mktuple
import Language.ISOZ.Common.Ann
import Language.ISOZ.Lexer.ISOZLexer 


-- | Show position information from a list of annotations 
showPos :: [Ann] -> String
showPos anns = "at (" ++ (show l) ++ ":" ++ (show c) ++ ")"
    where (l, c) = (extractLineCol (getPosnFromListAnns anns))

-- | Traverse syntax tree.
synTraverse1Tree :: SynTraverse1 a => a -> a
synTraverse1Tree = traverse1 

-- | Get a term's annotations.
getAnnTerm :: SynTraverse1 a => a -> [Ann] 
getAnnTerm = getAnn

-- | Get a term's annotations but ExprInParAnn and ParserParDepthAnn are removed since it should not propagate to others.
getAnnTermAndRemoveExprInPars1 :: SynTraverse1 a => a -> [Ann] 
getAnnTermAndRemoveExprInPars1  = removeParserParDepthFromListAnns . removeExprInParFromListAnns . getAnn

-- | Get a term's annotations but ExprInParAnn is removed since it should not propagate to others.
getAnnTermAndRemoveExprInPars :: SynTraverse1 a => a -> [Ann] 
getAnnTermAndRemoveExprInPars  = removeExprInParFromListAnns . getAnn

-- | Add an Ann to the term.
--
-- Please note: if there's a same type annotation, then just override it.
addAnnTerm :: SynTraverse1 a => Ann -> a -> a
addAnnTerm = addAnn

-- | Set the annotations of a term to specified annotations
setAnnTerm :: SynTraverse1 a => [Ann] -> a -> a
setAnnTerm = setAnn

class SynTraverse1 a where
    traverse1 :: a -> a
    traverse1List :: [a] -> [a]
    traverse1List = map (traverse1)
    getAnn :: a -> [Ann]
    addAnn :: Ann -> a -> a 
    setAnn :: [Ann] -> a -> a 
    addAnnList :: Ann -> [a] -> [a]
    addAnnList newa = map (addAnn newa)
    setAnnList :: [Ann] -> [a] -> [a] 
    setAnnList newa = map (setAnn newa)

instance SynTraverse1 a => SynTraverse1 [a] where
  traverse1 = traverse1List
  getAnn [] = [] 
  getAnn (x:xs) = getAnn x
  addAnn = addAnnList 
  setAnn = setAnnList 

instance SynTraverse1 Specification where
  traverse1 e = case e of
    SpecSect ann sections -> SpecSect ann (traverse1 sections)
    SpecAnony ann paragraphs -> SpecAnony ann (traverse1 paragraphs)
    SpecEmpty ann -> SpecEmpty ann
  getAnn e = case e of
    SpecSect ann _ -> ann 
    SpecAnony ann _ -> ann 
    SpecEmpty ann -> ann 
  addAnn newa e = case e of
    SpecSect  ann p -> SpecSect  (addAnnToAnnListHelper newa ann) p 
    SpecAnony ann p -> SpecAnony (addAnnToAnnListHelper newa ann) p 
    SpecEmpty ann   -> SpecEmpty (addAnnToAnnListHelper newa ann)   
  setAnn newa e = case e of
    SpecSect  ann p -> SpecSect  newa p 
    SpecAnony ann p -> SpecAnony newa p 
    SpecEmpty ann   -> SpecEmpty newa   

instance SynTraverse1 Section where
  traverse1 e = case e of
    Section ann zed sectname secparent end paragraphs -> case secparent of
                -- transform BaseSection
                BaseSection2 a1 -> BaseSection ann zed sectname end (trav (traverse1 paragraphs))
                -- transform InheritingSection 
                InheritingSection2 a2 listname -> InheritingSection ann zed sectname listname end (trav (traverse1 paragraphs))
        -- AxParaA contains a list of paragraphs
        where trav [] = []
              trav (x:xs) = case x of
                    AxParaA ann zed listparas end -> listparas ++ (trav xs)
                    _                             -> x : (trav xs)
    BaseSection ann zed sectname end paragraphs -> e 
    InheritingSection ann zed sectname listname end paragraphs -> e
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    Section     ann zed sectname secparent end paragraphs -> ann
    BaseSection ann zed sectname end paragraphs -> ann 
    InheritingSection ann zed sectname listname end paragraphs -> ann
  addAnn newa e = case e of
    Section             ann zed sectname secparent end paragraphs   -> Section             (addAnnToAnnListHelper newa ann) zed sectname secparent end paragraphs
    BaseSection         ann zed sectname end paragraphs             -> BaseSection         (addAnnToAnnListHelper newa ann) zed sectname end paragraphs           
    InheritingSection   ann zed sectname listname end paragraphs    -> InheritingSection   (addAnnToAnnListHelper newa ann) zed sectname listname end paragraphs 
  setAnn newa e = case e of
    Section             ann zed sectname secparent end paragraphs   -> Section             newa zed sectname secparent end paragraphs
    BaseSection         ann zed sectname end paragraphs             -> BaseSection         newa zed sectname end paragraphs           
    InheritingSection   ann zed sectname listname end paragraphs    -> InheritingSection   newa zed sectname listname end paragraphs 

--instance SynTraverse1 SectionParents where
--  traverse1 e = case e of
--    BaseSection -> BaseSection 
--    InheritingSection listname -> 

instance SynTraverse1 Paragraph where
  traverse1 e = case e of
    AxPara ann zed parabody end -> AxParaA ann zed (trav parabody) end 
        -- convert a list of paragraph bodies into a list of paragraphs
        where trav [] = []
              trav (x:xs) = (trav1 x) : (trav xs)
              trav1 parabd = case parabd of 
                GivenPara2 a1 listname -> GivenPara a1 listname 
                HDefPara2 a4 name expr -> HDefPara a4 name (traverse1 expr)
                GenHDefPara2 a5 name formals expr -> GenHDefPara a5 name formals (traverse1 expr)
                GenOpDefPara2 a6 genname expr -> GenOpDefPara a6 genname (traverse1 expr)
                FreetypePara2 a7 listtype ->  FreetypePara a7 (traverse1 listtype)
                OperatorPara2 a9 optemplate -> OperatorPara a9 (traverse1 optemplate)
                ChannelPara2 a10 cdecl -> ChannelPara a10 (traverse1 cdecl)
                ChannelFromPara2 a11 cdecl -> ChannelFromPara a11 (traverse1 cdecl)
                ChannelSetPara2 a12 cdecl -> ChannelSetPara a12 (traverse1 cdecl) 
    AxdefPara ann ax schematext end -> AxdefPara ann ax (traverse1 schematext) end
    SchdefPara ann sch name schematext end -> SchdefPara ann sch (name) (traverse1 schematext) end 
    GenAxdefPara ann ax formals schematext end -> GenAxdefPara ann ax (traverse1 formals) (traverse1 schematext) end 
    GenSchdefPara ann sch formals name schematext end -> GenSchdefPara ann sch (traverse1 formals) (name) (traverse1 schematext) end 
    GenConjecturePara ann name formals p -> GenConjecturePara ann name (traverse1 formals) (traverse1 p)
    ConjecturePara ann name p -> ConjecturePara ann name (traverse1 p)
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    AxPara ann zed parabody end -> ann
    GivenPara ann _ -> ann
    GivenParaA ann _ -> ann
    AxdefPara ann ax schematext end -> ann 
    AxdefParaA ann _ -> ann 
    SchdefPara ann sch name schematext end -> ann 
    GenAxdefPara ann ax formals schematext end -> ann 
    GenAxdefParaA ann _ _ -> ann 
    GenSchdefPara ann sch formals name schematext end -> ann 
    GenConjecturePara ann name formals p -> ann
    GenConjectureParaA ann name _ _ -> ann
    HDefPara ann declname expression -> ann
    GenHDefPara ann name formals expression -> ann
    GenOpDefPara ann genname expression -> ann
    FreetypePara ann listfreetype -> ann 
    FreetypeParaA ann listfreetype -> ann 
    ConjecturePara ann name p -> ann 
    ConjectureParaA ann name p -> ann 
    OperatorPara ann optemplate -> ann
    ChannelPara ann chnlist -> ann 
    ChannelFromPara ann chnfromlist -> ann 
    ChannelSetPara ann _ -> ann 
  addAnn newa e = case e of
    AxPara ann zed parabody end                         -> AxPara               (addAnnToAnnListHelper newa ann) zed parabody end                      
    GivenPara ann p                                     -> GivenPara            (addAnnToAnnListHelper newa ann) p                                  
    GivenParaA ann p                                    -> GivenParaA           (addAnnToAnnListHelper newa ann) p                                 
    AxdefPara ann ax schematext end                     -> AxdefPara            (addAnnToAnnListHelper newa ann) ax schematext end                   
    AxdefParaA ann p                                    -> AxdefParaA           (addAnnToAnnListHelper newa ann) p                                  
    SchdefPara ann sch name schematext end              -> SchdefPara           (addAnnToAnnListHelper newa ann) sch name schematext end            
    GenAxdefPara ann ax formals schematext end          -> GenAxdefPara         (addAnnToAnnListHelper newa ann) ax formals schematext end        
    GenAxdefParaA ann p1 p2                             -> GenAxdefParaA        (addAnnToAnnListHelper newa ann) p1 p2                           
    GenSchdefPara ann sch formals name schematext end   -> GenSchdefPara        (addAnnToAnnListHelper newa ann) sch formals name schematext end 
    GenConjecturePara ann name formals p                -> GenConjecturePara    (addAnnToAnnListHelper newa ann) name formals p                  
    GenConjectureParaA ann name p1 p2                   -> GenConjectureParaA   (addAnnToAnnListHelper newa ann) name p1 p2                     
    HDefPara ann declname expression                    -> HDefPara             (addAnnToAnnListHelper newa ann) declname expression                 
    GenHDefPara ann name formals expression             -> GenHDefPara          (addAnnToAnnListHelper newa ann) name formals expression          
    GenOpDefPara ann genname expression                 -> GenOpDefPara         (addAnnToAnnListHelper newa ann) genname expression              
    FreetypePara ann listfreetype                       -> FreetypePara         (addAnnToAnnListHelper newa ann) listfreetype                     
    FreetypeParaA ann listfreetype                      -> FreetypeParaA        (addAnnToAnnListHelper newa ann) listfreetype                    
    ConjecturePara ann name p                           -> ConjecturePara       (addAnnToAnnListHelper newa ann) name p
    ConjectureParaA ann name p                          -> ConjectureParaA      (addAnnToAnnListHelper newa ann) name p                             
    OperatorPara ann optemplate                         -> OperatorPara         (addAnnToAnnListHelper newa ann) optemplate                      
    ChannelPara ann chnlist                             -> ChannelPara          (addAnnToAnnListHelper newa ann) chnlist                           
    ChannelFromPara ann chnfromlist                     -> ChannelFromPara      (addAnnToAnnListHelper newa ann) chnfromlist                   
    ChannelSetPara ann p                                -> ChannelSetPara       (addAnnToAnnListHelper newa ann) p                              
  setAnn newa e = case e of
    AxPara ann zed parabody end                         -> AxPara               newa zed parabody end                      
    GivenPara ann p                                     -> GivenPara            newa p                                  
    GivenParaA ann p                                    -> GivenParaA           newa p                                 
    AxdefPara ann ax schematext end                     -> AxdefPara            newa ax schematext end                   
    AxdefParaA ann p                                    -> AxdefParaA           newa p                                  
    SchdefPara ann sch name schematext end              -> SchdefPara           newa sch name schematext end            
    GenAxdefPara ann ax formals schematext end          -> GenAxdefPara         newa ax formals schematext end        
    GenAxdefParaA ann p1 p2                             -> GenAxdefParaA        newa p1 p2                           
    GenSchdefPara ann sch formals name schematext end   -> GenSchdefPara        newa sch formals name schematext end 
    GenConjecturePara ann name formals p                -> GenConjecturePara    newa name formals p                  
    GenConjectureParaA ann name p1 p2                   -> GenConjectureParaA   newa name p1 p2                     
    HDefPara ann declname expression                    -> HDefPara             newa declname expression                 
    GenHDefPara ann name formals expression             -> GenHDefPara          newa name formals expression          
    GenOpDefPara ann genname expression                 -> GenOpDefPara         newa genname expression              
    FreetypePara ann listfreetype                       -> FreetypePara         newa listfreetype                     
    FreetypeParaA ann listfreetype                      -> FreetypeParaA        newa listfreetype                    
    ConjecturePara ann name p                           -> ConjecturePara       newa name p                              
    ConjectureParaA ann name p                          -> ConjectureParaA      newa name p                             
    OperatorPara ann optemplate                         -> OperatorPara         newa optemplate                      
    ChannelPara ann chnlist                             -> ChannelPara          newa chnlist                           
    ChannelFromPara ann chnfromlist                     -> ChannelFromPara      newa chnfromlist                   
    ChannelSetPara ann p                                -> ChannelSetPara       newa p                              

instance SynTraverse1 ParagraphBody where
  traverse1 e = e 
  getAnn e = case e of
    GivenPara2 ann _            -> ann 
    HDefPara2 ann _ _           -> ann 
    GenHDefPara2 ann _ _ _      -> ann 
    GenOpDefPara2 ann _ _       -> ann 
    FreetypePara2 ann _         -> ann 
    OperatorPara2 ann _         -> ann 
    ChannelPara2 ann _          -> ann 
    ChannelFromPara2 ann _      -> ann 
    ChannelSetPara2 ann _       -> ann 
    ProcDeclPara2 ann _ _       -> ann
    GenProcDeclPara2 ann _ _ _  -> ann 
  addAnn newa e = e 
  setAnn newa e = e 

instance SynTraverse1 Freetype where
  traverse1 e = case e of
    Freetype ann name listbranches -> Freetype ann (name) (traverse1 listbranches) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    Freetype ann name listbranches -> ann 
  addAnn newa e = case e of
    Freetype ann name listbranches -> Freetype (addAnnToAnnListHelper newa ann) name listbranches 
  setAnn newa e = case e of
    Freetype ann name listbranches -> Freetype newa name listbranches 

instance SynTraverse1 Branch where
  traverse1 e = case e of
    ConstantBranch ann declname -> ConstantBranch ann (traverse1 declname)
    ConstructorBranch ann declname expr -> ConstructorBranch ann (traverse1 declname) (traverse1 expr) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    ConstantBranch ann declname -> ann 
    ConstructorBranch ann declname expr -> ann 
  addAnn newa e = case e of
    ConstantBranch      ann declname        -> ConstantBranch      (addAnnToAnnListHelper newa ann) declname      
    ConstructorBranch   ann declname expr   -> ConstructorBranch   (addAnnToAnnListHelper newa ann) declname expr 
  setAnn newa e = case e of
    ConstantBranch      ann declname        -> ConstantBranch      newa declname      
    ConstructorBranch   ann declname expr   -> ConstructorBranch   newa declname expr 

instance SynTraverse1 Formals where
  traverse1 e = case e of
    Formals ann listname -> e 
  getAnn e = case e of
    Formals ann listname -> ann
  addAnn newa e = case e of
    Formals ann listname -> Formals (addAnnToAnnListHelper newa ann) listname 
  setAnn newa e = case e of
    Formals ann listname -> Formals newa listname 

instance SynTraverse1 ExprPredicate where
  traverse1 e = e 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    EPForall ann schtext ep             -> ann
    EPExists ann schtext ep             -> ann 
    EPExists1 ann schtext ep            -> ann  
    EPEquiv ann ep1 ep2                 -> ann
    EPImpl ann ep1 ep2                     -> ann
    EPAnd ann ep1 ep2                     -> ann
    EPOr ann ep1 ep2                     -> ann
    EPNeg ann ep                        -> ann
    EPLambda ann schtext ep             -> ann
    EPMu ann schtext ep                 -> ann
    EPLocalDef ann decls ep             -> ann
    EPCond ann ep1 ep2 ep                 -> ann
    EPComp ann ep1 ep2                     -> ann
    EPPipe ann ep1 ep2                     -> ann
    EPHide ann ep declnames             -> ann
    EPProj ann ep1 ep2                    -> ann
    EPPre ann ep                        -> ann
    EPProd ann eps                        -> ann
    EPPower ann ep                        -> ann
    EPFuncAppl ann app                   -> ann
    EPAppl ann ep1 ep2                    -> ann
    EPDecor ann ep stroke                 -> ann
    EPRename ann ep rps                 -> ann
    EPBindSel ann ep refname             -> ann
    EPTupleSel ann ep num                 -> ann
    EPTheta ann ep stroke                 -> ann
    EPRef ann refname                    -> ann
    EPGenRef ann refname eps            -> ann
    EPEmptySet ann                        -> ann
    EPSet ann eps                        -> ann
    EPSetComp ann schtext ep             -> ann
    EPCharSetComp ann schtext            -> ann
    EPSchema ann schtext                -> ann
    EPVarCons ann name ep                -> ann
    EPSchemaCons ann ep1 ep2             -> ann
    EPBindExt ann decls                  -> ann
    EPTupleExt ann ep eps                  -> ann
    EPCharMu ann schtext                  -> ann
    EPNum ann n                         -> ann
    EPRel ann rel                        -> ann
    EPExprPred ann ep                    -> ann
    EPTrue ann                            -> ann
    EPFalse ann                            -> ann
  addAnn newa e = case e of
    EPForall ann schtext ep             -> EPForall (addAnnToAnnListHelper newa ann) schtext ep
    EPExists ann schtext ep             -> EPExists (addAnnToAnnListHelper newa ann) schtext ep               
    EPExists1 ann schtext ep            -> EPExists1 (addAnnToAnnListHelper newa ann) schtext ep               
    EPEquiv ann ep1 ep2                 -> EPEquiv (addAnnToAnnListHelper newa ann) ep1 ep2                  
    EPImpl ann ep1 ep2                     -> EPImpl (addAnnToAnnListHelper newa ann) ep1 ep2                      
    EPAnd ann ep1 ep2                     -> EPAnd (addAnnToAnnListHelper newa ann) ep1 ep2                      
    EPOr ann ep1 ep2                     -> EPOr (addAnnToAnnListHelper newa ann) ep1 ep2                      
    EPNeg ann ep                        -> EPNeg (addAnnToAnnListHelper newa ann) ep                         
    EPLambda ann schtext ep             -> EPLambda (addAnnToAnnListHelper newa ann) schtext ep              
    EPMu ann schtext ep                 -> EPMu (addAnnToAnnListHelper newa ann) schtext ep                  
    EPLocalDef ann decls ep             -> EPLocalDef (addAnnToAnnListHelper newa ann) decls ep              
    EPCond ann ep1 ep2 ep                 -> EPCond (addAnnToAnnListHelper newa ann) ep1 ep2 ep                  
    EPComp ann ep1 ep2                     -> EPComp (addAnnToAnnListHelper newa ann) ep1 ep2                      
    EPPipe ann ep1 ep2                     -> EPPipe (addAnnToAnnListHelper newa ann) ep1 ep2                      
    EPHide ann ep declnames             -> EPHide (addAnnToAnnListHelper newa ann) ep declnames              
    EPProj ann ep1 ep2                    -> EPProj (addAnnToAnnListHelper newa ann) ep1 ep2                     
    EPPre ann ep                        -> EPPre (addAnnToAnnListHelper newa ann) ep                         
    EPProd ann eps                        -> EPProd (addAnnToAnnListHelper newa ann) eps                         
    EPPower ann ep                        -> EPPower (addAnnToAnnListHelper newa ann) ep                         
    EPFuncAppl ann app                   -> EPFuncAppl (addAnnToAnnListHelper newa ann) app                    
    EPAppl ann ep1 ep2                    -> EPAppl (addAnnToAnnListHelper newa ann) ep1 ep2                     
    EPDecor ann ep stroke                 -> EPDecor (addAnnToAnnListHelper newa ann) ep stroke                  
    EPRename ann ep rps                 -> EPRename (addAnnToAnnListHelper newa ann) ep rps                  
    EPBindSel ann ep refname             -> EPBindSel (addAnnToAnnListHelper newa ann) ep refname              
    EPTupleSel ann ep num                 -> EPTupleSel (addAnnToAnnListHelper newa ann) ep num                  
    EPTheta ann ep stroke                 -> EPTheta (addAnnToAnnListHelper newa ann) ep stroke                  
    EPRef ann refname                    -> EPRef (addAnnToAnnListHelper newa ann) refname                     
    EPGenRef ann refname eps            -> EPGenRef (addAnnToAnnListHelper newa ann) refname eps             
    EPEmptySet ann                        -> EPEmptySet (addAnnToAnnListHelper newa ann)                         
    EPSet ann eps                        -> EPSet (addAnnToAnnListHelper newa ann) eps                         
    EPSetComp ann schtext ep             -> EPSetComp (addAnnToAnnListHelper newa ann) schtext ep              
    EPCharSetComp ann schtext            -> EPCharSetComp (addAnnToAnnListHelper newa ann) schtext             
    EPSchema ann schtext                -> EPSchema (addAnnToAnnListHelper newa ann) schtext                 
    EPVarCons ann name ep                -> EPVarCons (addAnnToAnnListHelper newa ann) name ep                 
    EPSchemaCons ann ep1 ep2             -> EPSchemaCons (addAnnToAnnListHelper newa ann) ep1 ep2              
    EPBindExt ann decls                  -> EPBindExt (addAnnToAnnListHelper newa ann) decls                   
    EPTupleExt ann ep eps                  -> EPTupleExt (addAnnToAnnListHelper newa ann) ep eps                   
    EPCharMu ann schtext                  -> EPCharMu (addAnnToAnnListHelper newa ann) schtext                   
    EPNum ann n                         -> EPNum (addAnnToAnnListHelper newa ann) n                          
    EPRel ann rel                        -> EPRel (addAnnToAnnListHelper newa ann) rel                         
    EPExprPred ann ep                    -> EPExprPred (addAnnToAnnListHelper newa ann) ep                     
    EPTrue ann                            -> EPTrue (addAnnToAnnListHelper newa ann)                             
    EPFalse ann                            -> EPFalse (addAnnToAnnListHelper newa ann)                             
  setAnn newa e = case e of
    EPForall ann schtext ep             -> EPForall newa schtext ep
    EPExists ann schtext ep             -> EPExists newa schtext ep               
    EPExists1 ann schtext ep            -> EPExists1 newa schtext ep               
    EPEquiv ann ep1 ep2                 -> EPEquiv newa ep1 ep2                  
    EPImpl ann ep1 ep2                     -> EPImpl newa ep1 ep2                      
    EPAnd ann ep1 ep2                     -> EPAnd newa ep1 ep2                      
    EPOr ann ep1 ep2                     -> EPOr newa ep1 ep2                      
    EPNeg ann ep                        -> EPNeg newa ep                         
    EPLambda ann schtext ep             -> EPLambda newa schtext ep              
    EPMu ann schtext ep                 -> EPMu newa schtext ep                  
    EPLocalDef ann decls ep             -> EPLocalDef newa decls ep              
    EPCond ann ep1 ep2 ep                 -> EPCond newa ep1 ep2 ep                  
    EPComp ann ep1 ep2                     -> EPComp newa ep1 ep2                      
    EPPipe ann ep1 ep2                     -> EPPipe newa ep1 ep2                      
    EPHide ann ep declnames             -> EPHide newa ep declnames              
    EPProj ann ep1 ep2                    -> EPProj newa ep1 ep2                     
    EPPre ann ep                        -> EPPre newa ep                         
    EPProd ann eps                        -> EPProd newa eps                         
    EPPower ann ep                        -> EPPower newa ep                         
    EPFuncAppl ann app                   -> EPFuncAppl newa app                    
    EPAppl ann ep1 ep2                    -> EPAppl newa ep1 ep2                     
    EPDecor ann ep stroke                 -> EPDecor newa ep stroke                  
    EPRename ann ep rps                 -> EPRename newa ep rps                  
    EPBindSel ann ep refname             -> EPBindSel newa ep refname              
    EPTupleSel ann ep num                 -> EPTupleSel newa ep num                  
    EPTheta ann ep stroke                 -> EPTheta newa ep stroke                  
    EPRef ann refname                    -> EPRef newa refname                     
    EPGenRef ann refname eps            -> EPGenRef newa refname eps             
    EPEmptySet ann                        -> EPEmptySet newa                         
    EPSet ann eps                        -> EPSet newa eps                         
    EPSetComp ann schtext ep             -> EPSetComp newa schtext ep              
    EPCharSetComp ann schtext            -> EPCharSetComp newa schtext             
    EPSchema ann schtext                -> EPSchema newa schtext                 
    EPVarCons ann name ep                -> EPVarCons newa name ep                 
    EPSchemaCons ann ep1 ep2             -> EPSchemaCons newa ep1 ep2              
    EPBindExt ann decls                  -> EPBindExt newa decls                   
    EPTupleExt ann ep eps                  -> EPTupleExt newa ep eps                   
    EPCharMu ann schtext                  -> EPCharMu newa schtext                   
    EPNum ann n                         -> EPNum newa n                          
    EPRel ann rel                        -> EPRel newa rel                         
    EPExprPred ann ep                    -> EPExprPred newa ep                     
    EPTrue ann                            -> EPTrue newa                             
    EPFalse ann                            -> EPFalse newa                             

instance SynTraverse1 Predicate where
  traverse1 e = case e of
    AndPred ann p1 p2 -> AndPred ann (traverse1 p1) (traverse1 p2) 
    OrPred ann p1 p2 ->  OrPred ann (traverse1 p1) (traverse1 p2) 
    ForallPred ann schtext p -> ForallPred ann (traverse1 schtext) (traverse1 p) 
    ExistsPred ann schtext p -> ExistsPred ann (traverse1 schtext) (traverse1 p) 
    Exists1Pred ann schtext p -> Exists1Pred ann (traverse1 schtext) (traverse1 p) 
    EquivPred ann p1 p2 -> EquivPred ann (traverse1 p1) (traverse1 p2) 
    ImplPred ann p1 p2 -> ImplPred ann (traverse1 p1) (traverse1 p2) 
    NegPred ann p -> NegPred ann (traverse1 p) 
    RelPred ann relation -> RelPred ann (traverse1 relation) 
    ExprPred ann p -> ExprPred ann (traverse1 p) 
    TruePred ann -> TruePred ann 
    FalsePred ann -> FalsePred ann 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    AndPred ann p1 p2 -> ann 
    OrPred ann p1 p2 -> ann 
    ForallPred ann schtext p -> ann 
    ForallPredA ann _ p -> ann 
    ExistsPred ann schtext p -> ann 
    Exists1Pred ann schtext p -> ann 
    Exists1PredA ann _ p -> ann 
    EquivPred ann p1 p2 -> ann 
    ImplPred ann p1 p2 -> ann 
    NegPred ann p -> ann 
    RelPred ann relation -> ann 
    ExprPred ann p -> ann 
    TruePred ann -> ann 
    FalsePred ann -> ann
    MemPred ann _ _ -> ann
  addAnn newa e = case e of
    AndPred ann p1 p2           -> AndPred (addAnnToAnnListHelper newa ann) p1 p2        
    OrPred ann p1 p2            -> OrPred       (addAnnToAnnListHelper newa ann) p1 p2         
    ForallPred ann schtext p    -> ForallPred   (addAnnToAnnListHelper newa ann) schtext p 
    ForallPredA ann e p         -> ForallPredA  (addAnnToAnnListHelper newa ann) e p      
    ExistsPred ann schtext p    -> ExistsPred   (addAnnToAnnListHelper newa ann) schtext p 
    Exists1Pred ann schtext p   -> Exists1Pred  (addAnnToAnnListHelper newa ann) schtext p
    Exists1PredA ann e p        -> Exists1PredA (addAnnToAnnListHelper newa ann) e p     
    EquivPred ann p1 p2         -> EquivPred    (addAnnToAnnListHelper newa ann) p1 p2      
    ImplPred ann p1 p2          -> ImplPred     (addAnnToAnnListHelper newa ann) p1 p2       
    NegPred ann p               -> NegPred      (addAnnToAnnListHelper newa ann) p            
    RelPred ann relation        -> RelPred      (addAnnToAnnListHelper newa ann) relation     
    ExprPred ann p              -> ExprPred     (addAnnToAnnListHelper newa ann) p           
    TruePred ann                -> TruePred     (addAnnToAnnListHelper newa ann)             
    FalsePred ann               -> FalsePred    (addAnnToAnnListHelper newa ann)            
    MemPred ann e1 e2           -> MemPred      (addAnnToAnnListHelper newa ann) e1 e2        
  setAnn newa e = case e of
    AndPred ann p1 p2           -> AndPred      newa p1 p2        
    OrPred ann p1 p2            -> OrPred       newa p1 p2         
    ForallPred ann schtext p    -> ForallPred   newa schtext p 
    ForallPredA ann e p         -> ForallPredA  newa e p      
    ExistsPred ann schtext p    -> ExistsPred   newa schtext p 
    Exists1Pred ann schtext p   -> Exists1Pred  newa schtext p
    Exists1PredA ann e p        -> Exists1PredA newa e p     
    EquivPred ann p1 p2         -> EquivPred    newa p1 p2      
    ImplPred ann p1 p2          -> ImplPred     newa p1 p2       
    NegPred ann p               -> NegPred      newa p            
    RelPred ann relation        -> RelPred      newa relation     
    ExprPred ann p              -> ExprPred     newa p           
    TruePred ann                -> TruePred     newa             
    FalsePred ann               -> FalsePred    newa            
    MemPred ann e1 e2           -> MemPred      newa e1 e2        

instance SynTraverse1 RenamePair where
  traverse1 e = case e of
    RenamePair ann newname oldname -> e
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    RenamePair ann newname oldname -> ann
  addAnn newa e = case e of
    RenamePair ann newname oldname -> RenamePair (addAnnToAnnListHelper newa ann) newname oldname 
  setAnn newa e = case e of
    RenamePair ann newname oldname -> RenamePair newa newname oldname 


-- Check if a schema text is { expr } only
-- If so, it should be regarded as singleton set, instead of Characteristic Set Comprehension
isSchemaTextAnExpr :: SchemaText -> Maybe Expression
isSchemaTextAnExpr (SchemaTextDecl _ (DeclPart _ [(ExprDecl _ expr)])) = Just expr
isSchemaTextAnExpr _ = Nothing 

instance SynTraverse1 Expression where
  traverse1 e = case e of
    NumExpr ann name -> NumExpr ann name 
    ForallExpr ann schtext expr -> ForallExpr ann (traverse1 schtext) (traverse1 expr) 
    ExistsExpr ann schtext expr -> ExistsExpr ann (traverse1 schtext) (traverse1 expr) 
    Exists1Expr ann schtext expr -> Exists1Expr ann (traverse1 schtext) (traverse1 expr) 
--    LambdaExpr schtext expr -> LambdaExpr (traverse1 schtext) (traverse1 expr) 
--    9.3.1 Function construction expression
--    \lambda t @ e => { t @ (chartuple t, e) }
    LambdaExpr ann schtext expr -> SetCompExpr ann (traverse1 schtext) (TupleExtExpr ann (chartuple (traverse1 schtext)) [expr])
    MuExpr ann schtext expr -> MuExpr ann (traverse1 schtext) (traverse1 expr) 
    LocalDefExpr ann decllist expr -> LocalDefExpr ann (traverse1 decllist) (traverse1 expr) 
    EquivExpr ann expr1 expr2 -> EquivExpr ann (traverse1 expr1) (traverse1 expr2) 
    ImplExpr ann expr1 expr2 -> ImplExpr ann (traverse1 expr1) (traverse1 expr2) 
    AndExpr ann expr1 expr2 -> AndExpr ann (traverse1 expr1) (traverse1 expr2) 
    OrExpr ann expr1 expr2 -> OrExpr ann (traverse1 expr1) (traverse1 expr2) 
    NegExpr ann expr -> NegExpr ann (traverse1 expr) 
    CondExpr ann pred expr1 expr2 -> CondExpr ann (traverse1 pred) (traverse1 expr1) (traverse1 expr2) 
    CompExpr ann expr1 expr2 -> CompExpr ann (traverse1 expr1) (traverse1 expr2) 
    PipeExpr ann expr1 expr2 -> PipeExpr ann (traverse1 expr1) (traverse1 expr2) 
    HideExpr ann expr namelist -> HideExpr ann (traverse1 expr) (traverse1 namelist) 
    ProjExpr ann expr1 expr2 -> ProjExpr ann (traverse1 expr1) (traverse1 expr2) 
    PreExpr ann expr -> PreExpr ann (traverse1 expr) 
    -- TODO: how to cope with (A x B) x C  and A x B x C in different ways
    -- (A x B) x C : should be ProdExpr [(ProdExpr [A, B]), C]
    -- while A x B x C should be ProdExpr [A, B, C]
    -- According to 4.2.7 Tuples and Cartesian products, A x B x C should be regarded as (A x (B x C))
    ProdExpr ann exprlist -> ProdExpr ann (traverse1 (exprlist)) -- ProdExpr ann (traverse1 (convert exprlist)) 
        -- convert embedded ProdExpr to list of expression
        where convert [] = []
              convert (s:xs) = case s of 
                                ProdExpr ann list -> concat [(convert list), (convert xs)]
                                _             -> (:) s (convert xs)
    PowerExpr ann expr -> PowerExpr ann (traverse1 expr) 
    FuncApplExpr ann app -> FuncApplExpr ann (traverse1 app) 
    ApplExpr ann expr1 expr2 -> ApplExpr ann (traverse1 expr1) (traverse1 expr2) 
    DecorExpr ann expr decor -> DecorExpr ann (traverse1 expr) (decor) 
    RenameExpr ann expr namepairlist -> RenameExpr ann (traverse1 expr) (traverse1 namepairlist)
    BindSelExpr ann expr refname -> BindSelExpr ann (traverse1 expr) (traverse1 refname) 
    TupleSelExpr ann expr numeral -> TupleSelExpr ann (traverse1 expr) (numeral)  
    ThetaExpr ann expr strokelist -> ThetaExpr ann (traverse1 expr) strokelist 
--    RefExprB ann refname rebody -> case rebody of 
--            RefExprBEmpty _         -> RefExpr ann (traverse1 refname)
--            RefExprBGen _ exprlist  -> GenRefExpr ann (traverse1 refname) (traverse1 exprlist)
    RefExpr ann refname -> RefExpr ann (traverse1 refname) 
    GenRefExpr ann refname exprlist -> GenRefExpr ann (traverse1 refname) (traverse1 exprlist)
    SetExpr ann exprlist -> SetExpr ann (traverse1 exprlist) 
    SetCompExpr ann schematext expr -> SetCompExpr ann (traverse1 schematext) (traverse1 expr) 
--    CharSetCompExpr schematext -> CharSetCompExpr (traverse1 schematext) 
--    9.3.2 Characteristic set comprehension expression 
--    {t} => { t @ chartuple t }
    CharSetCompExpr ann schematext -> case (isSchemaTextAnExpr schematext) of
                    -- singleton set extension
                    Just expr   -> SetExpr ann [(traverse1 expr)]
                    -- truly a CharSetCompExpr
                    Nothing     -> SetCompExpr ann (traverse1 schematext) (chartuple (traverse1 schematext))
--    CharSetCompExpr ann schematext -> SetCompExpr ann (traverse1 schematext) (chartuple (traverse1 schematext))
    SchemaExpr ann schematext -> SchemaExpr ann (traverse1 schematext) 
    BindExtExpr ann decllist -> BindExtExpr ann (traverse1 decllist)
    TupleExtExpr ann expr1 exprlist -> TupleExtExpr ann (traverse1 expr1) (traverse1 exprlist) 
--    CharMuExpr schematext -> CharMuExpr (traverse1 schematext) 
--    9.3.2 Characteristic definite description expression 
--    {\mu t} => \mu t @ chartuple t
    CharMuExpr ann schematext -> MuExpr ann (traverse1 schematext) (chartuple (traverse1 schematext))
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    NumExpr ann name -> ann
    ForallExpr ann schtext expr -> ann
    ForallExprA ann _ expr -> ann
    ExistsExpr ann schtext expr -> ann
    Exists1Expr ann schtext expr -> ann
    Exists1ExprA ann _ expr -> ann
    LambdaExpr ann schtext expr -> ann
    MuExpr ann schtext expr -> ann
    MuExprA ann _ expr -> ann
    LocalDefExpr ann decllist expr -> ann
    EquivExpr ann expr1 expr2 -> ann
    ImplExpr ann expr1 expr2 -> ann
    AndExpr ann expr1 expr2 -> ann
    OrExpr ann expr1 expr2 -> ann
    NegExpr ann expr -> ann
    CondExpr ann pred expr1 expr2 -> ann
    CompExpr ann expr1 expr2 -> ann
    PipeExpr ann expr1 expr2 -> ann
    HideExpr ann expr namelist -> ann
    HideExprA ann expr namelist -> ann
    ProjExpr ann expr1 expr2 -> ann
    PreExpr ann expr -> ann
    ProdExpr ann exprlist -> ann
    PowerExpr ann expr -> ann
    FuncApplExpr ann app -> ann
    ApplExpr ann expr1 expr2 -> ann
    DecorExpr ann expr decor -> ann
    RenameExpr ann expr namepairlist -> ann
    BindSelExpr ann expr refname -> ann
    BindSelExprA ann expr name -> ann
    TupleSelExpr ann expr numeral -> ann
    ThetaExpr ann expr strokelist -> ann
--    RefExprB ann refname rebody -> ann 
    RefExpr ann refname -> ann
    RefExprA ann name -> ann
    GenRefExpr ann refname exprlist -> ann
    GenRefExprA ann name exprlist -> ann
    EmptySetExpr ann -> ann
    SetExpr ann exprlist -> ann
    SetCompExpr ann schematext expr -> ann
    SetCompExprA ann _ expr -> ann
    CharSetCompExpr ann schematext -> ann
    SchemaExpr ann schematext -> ann
    VarConsExpr ann name e -> ann
    SchemaConsExpr ann ex p -> ann 
    BindExtExpr ann decllist -> ann
    BindExtExprA ann _ -> ann
    TupleExtExpr ann expr1 exprlist -> ann
    CharMuExpr ann schematext -> ann
  addAnn newa e = case e of
    NumExpr ann name -> NumExpr (addAnnToAnnListHelper newa ann) name  
    ForallExpr ann schtext expr -> ForallExpr (addAnnToAnnListHelper newa ann) schtext expr
    ForallExprA ann e' expr -> ForallExprA (addAnnToAnnListHelper newa ann) e' expr
    ExistsExpr ann schtext expr -> ExistsExpr (addAnnToAnnListHelper newa ann) schtext expr
    Exists1Expr ann schtext expr -> Exists1Expr (addAnnToAnnListHelper newa ann) schtext expr
    Exists1ExprA ann e' expr -> Exists1ExprA (addAnnToAnnListHelper newa ann) e' expr
    LambdaExpr ann schtext expr -> LambdaExpr (addAnnToAnnListHelper newa ann) schtext expr
    MuExpr ann schtext expr -> MuExpr (addAnnToAnnListHelper newa ann) schtext expr
    MuExprA ann e' expr -> MuExprA (addAnnToAnnListHelper newa ann) e' expr
    LocalDefExpr ann decllist expr -> LocalDefExpr (addAnnToAnnListHelper newa ann) decllist expr
    EquivExpr ann expr1 expr2 -> EquivExpr (addAnnToAnnListHelper newa ann) expr1 expr2
    ImplExpr ann expr1 expr2 -> ImplExpr (addAnnToAnnListHelper newa ann) expr1 expr2
    AndExpr ann expr1 expr2 -> AndExpr (addAnnToAnnListHelper newa ann) expr1 expr2
    OrExpr ann expr1 expr2 -> OrExpr (addAnnToAnnListHelper newa ann) expr1 expr2
    NegExpr ann expr -> NegExpr (addAnnToAnnListHelper newa ann) expr
    CondExpr ann pred expr1 expr2 -> CondExpr (addAnnToAnnListHelper newa ann) pred expr1 expr2
    CompExpr ann expr1 expr2 -> CompExpr (addAnnToAnnListHelper newa ann) expr1 expr2
    PipeExpr ann expr1 expr2 -> PipeExpr (addAnnToAnnListHelper newa ann) expr1 expr2
    HideExpr ann expr namelist -> HideExpr (addAnnToAnnListHelper newa ann) expr namelist
    HideExprA ann expr namelist -> HideExprA (addAnnToAnnListHelper newa ann) expr namelist
    ProjExpr ann expr1 expr2 -> ProjExpr (addAnnToAnnListHelper newa ann) expr1 expr2
    PreExpr ann expr -> PreExpr (addAnnToAnnListHelper newa ann) expr
    ProdExpr ann exprlist -> ProdExpr (addAnnToAnnListHelper newa ann) exprlist
    PowerExpr ann expr -> PowerExpr (addAnnToAnnListHelper newa ann) expr
    FuncApplExpr ann app -> FuncApplExpr (addAnnToAnnListHelper newa ann) app
    ApplExpr ann expr1 expr2 -> ApplExpr (addAnnToAnnListHelper newa ann) expr1 expr2
    DecorExpr ann expr decor -> DecorExpr (addAnnToAnnListHelper newa ann) expr decor
    RenameExpr ann expr namepairlist -> RenameExpr (addAnnToAnnListHelper newa ann) expr namepairlist
    BindSelExpr ann expr refname -> BindSelExpr (addAnnToAnnListHelper newa ann) expr refname
    BindSelExprA ann expr name -> BindSelExprA (addAnnToAnnListHelper newa ann) expr name
    TupleSelExpr ann expr numeral -> TupleSelExpr (addAnnToAnnListHelper newa ann) expr numeral
    ThetaExpr ann expr strokelist -> ThetaExpr (addAnnToAnnListHelper newa ann) expr strokelist
--    RefExprB ann refname rebody -> RefExprB (addAnnToAnnListHelper newa ann) refname rebody
    RefExpr ann refname -> RefExpr (addAnnToAnnListHelper newa ann) refname
    RefExprA ann name -> RefExprA (addAnnToAnnListHelper newa ann) name
    GenRefExpr ann refname exprlist -> GenRefExpr (addAnnToAnnListHelper newa ann) refname exprlist
    GenRefExprA ann name exprlist -> GenRefExprA (addAnnToAnnListHelper newa ann) name exprlist
    EmptySetExpr ann -> EmptySetExpr (addAnnToAnnListHelper newa ann)
    SetExpr ann exprlist -> SetExpr (addAnnToAnnListHelper newa ann) exprlist
    SetCompExpr ann schematext expr -> SetCompExpr (addAnnToAnnListHelper newa ann) schematext expr
    SetCompExprA ann e' expr -> SetCompExprA (addAnnToAnnListHelper newa ann) e' expr
    CharSetCompExpr ann schematext -> CharSetCompExpr (addAnnToAnnListHelper newa ann) schematext
    SchemaExpr ann schematext -> SchemaExpr (addAnnToAnnListHelper newa ann) schematext
    VarConsExpr ann name e -> VarConsExpr (addAnnToAnnListHelper newa ann) name e
    SchemaConsExpr ann ex p -> SchemaConsExpr (addAnnToAnnListHelper newa ann) ex p 
    BindExtExpr ann decllist -> BindExtExpr (addAnnToAnnListHelper newa ann) decllist
    BindExtExprA ann e' -> BindExtExprA (addAnnToAnnListHelper newa ann) e'
    TupleExtExpr ann expr1 exprlist -> TupleExtExpr (addAnnToAnnListHelper newa ann) expr1 exprlist
    CharMuExpr ann schematext -> CharMuExpr (addAnnToAnnListHelper newa ann) schematext
  setAnn newa e = case e of
    NumExpr ann name -> NumExpr newa name  
    ForallExpr ann schtext expr -> ForallExpr newa schtext expr
    ForallExprA ann e' expr -> ForallExprA newa e' expr
    ExistsExpr ann schtext expr -> ExistsExpr newa schtext expr
    Exists1Expr ann schtext expr -> Exists1Expr newa schtext expr
    Exists1ExprA ann e' expr -> Exists1ExprA newa e' expr
    LambdaExpr ann schtext expr -> LambdaExpr newa schtext expr
    MuExpr ann schtext expr -> MuExpr newa schtext expr
    MuExprA ann e' expr -> MuExprA newa e' expr
    LocalDefExpr ann decllist expr -> LocalDefExpr newa decllist expr
    EquivExpr ann expr1 expr2 -> EquivExpr newa expr1 expr2
    ImplExpr ann expr1 expr2 -> ImplExpr newa expr1 expr2
    AndExpr ann expr1 expr2 -> AndExpr newa expr1 expr2
    OrExpr ann expr1 expr2 -> OrExpr newa expr1 expr2
    NegExpr ann expr -> NegExpr newa expr
    CondExpr ann pred expr1 expr2 -> CondExpr newa pred expr1 expr2
    CompExpr ann expr1 expr2 -> CompExpr newa expr1 expr2
    PipeExpr ann expr1 expr2 -> PipeExpr newa expr1 expr2
    HideExpr ann expr namelist -> HideExpr newa expr namelist
    ProjExpr ann expr1 expr2 -> ProjExpr newa expr1 expr2
    PreExpr ann expr -> PreExpr newa expr
    ProdExpr ann exprlist -> ProdExpr newa exprlist
    PowerExpr ann expr -> PowerExpr newa expr
    FuncApplExpr ann app -> FuncApplExpr newa app
    ApplExpr ann expr1 expr2 -> ApplExpr newa expr1 expr2
    DecorExpr ann expr decor -> DecorExpr newa expr decor
    RenameExpr ann expr namepairlist -> RenameExpr newa expr namepairlist
    BindSelExpr ann expr refname -> BindSelExpr newa expr refname
    BindSelExprA ann expr name -> BindSelExprA newa expr name
    TupleSelExpr ann expr numeral -> TupleSelExpr newa expr numeral
    ThetaExpr ann expr strokelist -> ThetaExpr newa expr strokelist
--    RefExprB ann refname rebody -> RefExprB newa refname rebody
    RefExpr ann refname -> RefExpr newa refname
    RefExprA ann name -> RefExprA newa name
    GenRefExpr ann refname exprlist -> GenRefExpr newa refname exprlist
    GenRefExprA ann name exprlist -> GenRefExprA newa name exprlist
    EmptySetExpr ann -> EmptySetExpr newa
    SetExpr ann exprlist -> SetExpr newa exprlist
    SetCompExpr ann schematext expr -> SetCompExpr newa schematext expr
    SetCompExprA ann e' expr -> SetCompExprA newa e' expr
    CharSetCompExpr ann schematext -> CharSetCompExpr newa schematext
    SchemaExpr ann schematext -> SchemaExpr newa schematext
    VarConsExpr ann name e -> VarConsExpr newa name e
    SchemaConsExpr ann ex p -> SchemaConsExpr newa ex p 
    BindExtExpr ann decllist -> BindExtExpr newa decllist
    BindExtExprA ann e' -> BindExtExprA newa e'
    TupleExtExpr ann expr1 exprlist -> TupleExtExpr newa expr1 exprlist
    CharMuExpr ann schematext -> CharMuExpr newa schematext


instance SynTraverse1 SchemaText where
  traverse1 e = case e of
    SchemaTextEmpty ann -> SchemaTextEmpty ann
    SchemaTextDecl ann decl -> SchemaTextDecl ann (traverse1 decl)
    SchemaTextPred ann pred -> SchemaTextPred ann (traverse1 pred) 
    SchemaText ann decl pred -> SchemaText ann (traverse1 decl) (traverse1 pred) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    SchemaTextEmpty ann -> ann
    SchemaTextDecl ann decl -> ann
    SchemaTextPred ann pred -> ann
    SchemaText ann decl pred -> ann
  addAnn newa e = case e of
    SchemaTextEmpty ann -> SchemaTextEmpty (addAnnToAnnListHelper newa ann)
    SchemaTextDecl ann decl -> SchemaTextDecl (addAnnToAnnListHelper newa ann) decl 
    SchemaTextPred ann pred -> SchemaTextPred (addAnnToAnnListHelper newa ann) pred 
    SchemaText ann decl pred ->SchemaText (addAnnToAnnListHelper newa ann) decl pred 
  setAnn newa e = case e of
    SchemaTextEmpty ann -> SchemaTextEmpty newa 
    SchemaTextDecl ann decl -> SchemaTextDecl newa decl 
    SchemaTextPred ann pred -> SchemaTextPred newa pred 
    SchemaText ann decl pred ->SchemaText newa decl pred 

instance SynTraverse1 DeclPart where
  traverse1 e = case e of
    DeclPart ann decls -> DeclPart ann (traverse1 decls) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    DeclPart ann decls -> ann
  addAnn newa e = case e of
    DeclPart ann decls -> DeclPart (addAnnToAnnListHelper newa ann) decls 
  setAnn newa e = case e of
    DeclPart ann decls -> DeclPart newa decls 

instance SynTraverse1 Declaration where
  traverse1 e = case e of
    Declaration ann declnames expr -> Declaration ann (traverse1 declnames) (traverse1 expr) 
    AbbrDecl ann declname expr -> AbbrDecl ann (traverse1 declname) (traverse1 expr) 
    ExprDecl ann expr -> ExprDecl ann (traverse1 expr) 
    Declaration1 ann name decl1 -> case decl1 of
                        BasicDecl1 a1 listdeclname expr -> Declaration ann ((:) (traverse1 (DName ann name)) (traverse1 listdeclname)) (traverse1 expr)
                        AbbrDecl1 a1 expr               -> AbbrDecl ann (traverse1 (DName ann name)) (traverse1 expr)
                        ExprDecl1 a1                   -> ExprDecl ann (traverse1 (RefExpr ann (RName ann name)))
    Declaration2 ann opname decl1 -> case decl1 of
                        BasicDecl1 a1 listdeclname expr -> Declaration ann ((:) (traverse1 (OpName ann opname)) (traverse1 listdeclname)) (traverse1 expr)
                        AbbrDecl1 a1 expr               -> AbbrDecl ann (traverse1 (OpName ann opname)) (traverse1 expr)
                        ExprDecl1 a1                   -> ExprDecl ann (traverse1 (RefExpr ann (OpRName ann opname)))
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    Declaration ann declnames expr -> ann
    EPDeclaration ann declnames expr -> ann
    AbbrDecl ann declname expr -> ann
    EPAbbrDecl ann declname expr -> ann
    ExprDecl ann expr -> ann
    EPExprDecl ann expr -> ann
    Declaration1 ann name decl1 -> ann
    Declaration2 ann opname decl1 -> ann
  addAnn newa e = case e of
    Declaration     ann declnames expr  -> Declaration     (addAnnToAnnListHelper newa ann) declnames expr
    AbbrDecl        ann declname expr   -> AbbrDecl        (addAnnToAnnListHelper newa ann) declname expr 
    ExprDecl        ann expr            -> ExprDecl        (addAnnToAnnListHelper newa ann) expr          
    Declaration1    ann name decl1      -> Declaration1    (addAnnToAnnListHelper newa ann) name decl1    
    Declaration2    ann opname decl1    -> Declaration2    (addAnnToAnnListHelper newa ann) opname decl1  
  setAnn newa e = case e of
    Declaration     ann declnames expr  -> Declaration     newa declnames expr
    AbbrDecl        ann declname expr   -> AbbrDecl        newa declname expr 
    ExprDecl        ann expr            -> ExprDecl        newa expr          
    Declaration1    ann name decl1      -> Declaration1    newa name decl1    
    Declaration2    ann opname decl1    -> Declaration2    newa opname decl1  
                        

instance SynTraverse1 OperatorTemplate where
  traverse1 e = case e of
    RelationTemplate ann template -> RelationTemplate ann (traverse1 template) 
    FunctionTemplate ann template -> FunctionTemplate ann (traverse1 template)
    GenericTemplate ann template -> GenericTemplate ann (traverse1 template)
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    RelationTemplate ann template -> ann
    FunctionTemplate ann template -> ann
    GenericTemplate ann template -> ann
  addAnn newa e = case e of
    RelationTemplate ann template -> RelationTemplate (addAnnToAnnListHelper newa ann) template
    FunctionTemplate ann template -> FunctionTemplate (addAnnToAnnListHelper newa ann) template
    GenericTemplate  ann template -> GenericTemplate  (addAnnToAnnListHelper newa ann) template
  setAnn newa e = case e of
    RelationTemplate ann template -> RelationTemplate newa template
    FunctionTemplate ann template -> FunctionTemplate newa template
    GenericTemplate  ann template -> GenericTemplate  newa template

instance SynTraverse1 CategoryTemplate where
  traverse1 e = case e of
    PrefixCatTemplate ann template -> PrefixCatTemplate ann (traverse1 template) 
    PostfixCatTemplate ann template -> PostfixCatTemplate ann (traverse1 template) 
    InfixCatTemplate ann prec assoc template -> InfixCatTemplate ann prec assoc (traverse1 template) 
    NofixCatTemplate ann template -> NofixCatTemplate ann (traverse1 template) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    PrefixCatTemplate ann template -> ann
    PostfixCatTemplate ann template -> ann
    InfixCatTemplate ann prec assoc template -> ann
    NofixCatTemplate ann template -> ann
  addAnn newa e = case e of
    PrefixCatTemplate   ann template            -> PrefixCatTemplate   (addAnnToAnnListHelper newa ann) template           
    PostfixCatTemplate  ann template            -> PostfixCatTemplate  (addAnnToAnnListHelper newa ann) template           
    InfixCatTemplate    ann prec assoc template -> InfixCatTemplate    (addAnnToAnnListHelper newa ann) prec assoc template
    NofixCatTemplate    ann template            -> NofixCatTemplate    (addAnnToAnnListHelper newa ann) template           
  setAnn newa e = case e of
    PrefixCatTemplate   ann template            -> PrefixCatTemplate   newa template           
    PostfixCatTemplate  ann template            -> PostfixCatTemplate  newa template           
    InfixCatTemplate    ann prec assoc template -> InfixCatTemplate    newa prec assoc template
    NofixCatTemplate    ann template            -> NofixCatTemplate    newa template           

instance SynTraverse1 Prec where
  traverse1 e = case e of
    Prec ann _ -> e
  getAnn e = case e of
    Prec ann _ -> ann
  addAnn newa e = case e of
    Prec ann p -> Prec (addAnnToAnnListHelper newa ann) p 
  setAnn newa e = case e of
    Prec ann p -> Prec newa p 

instance SynTraverse1 Assoc where
  traverse1 e = case e of
    LeftAssoc ann -> e
    RightAssoc ann -> e
  getAnn e = case e of
    LeftAssoc ann -> ann 
    RightAssoc ann -> ann 
  addAnn newa e = case e of
    LeftAssoc   ann -> LeftAssoc   (addAnnToAnnListHelper newa ann) 
    RightAssoc  ann -> RightAssoc  (addAnnToAnnListHelper newa ann) 
  setAnn newa e = case e of
    LeftAssoc   ann -> LeftAssoc   newa 
    RightAssoc  ann -> RightAssoc  newa 

instance SynTraverse1 Template where
  traverse1 e = case e of
    TPrefixTemplate ann template -> TPrefixTemplate ann (traverse1 template)
    TPostfixTemplate ann template -> TPostfixTemplate ann (traverse1 template)
    TInfixTemplate ann template -> TInfixTemplate ann (traverse1 template)
    TNofixTemplate ann template -> TNofixTemplate ann (traverse1 template)
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    TPrefixTemplate ann template -> ann
    TPostfixTemplate ann template -> ann
    TInfixTemplate ann template -> ann
    TNofixTemplate ann template -> ann
  addAnn newa e = case e of
    TPrefixTemplate     ann template -> TPrefixTemplate     (addAnnToAnnListHelper newa ann) template
    TPostfixTemplate    ann template -> TPostfixTemplate    (addAnnToAnnListHelper newa ann) template
    TInfixTemplate      ann template -> TInfixTemplate      (addAnnToAnnListHelper newa ann) template
    TNofixTemplate      ann template -> TNofixTemplate      (addAnnToAnnListHelper newa ann) template
  setAnn newa e = case e of
    TPrefixTemplate     ann template -> TPrefixTemplate     newa template
    TPostfixTemplate    ann template -> TPostfixTemplate    newa template
    TInfixTemplate      ann template -> TInfixTemplate      newa template
    TNofixTemplate      ann template -> TNofixTemplate      newa template

instance SynTraverse1 PrefixTemplate where
  traverse1 e = case e of
    PrefixTemplate ann prefixname  -> PrefixTemplate ann (traverse1 prefixname) 
    PowerPrefixTemplate ann        -> PowerPrefixTemplate ann 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    PrefixTemplate ann prefixname  -> ann
    PowerPrefixTemplate ann        -> ann
  addAnn newa e = case e of
    PrefixTemplate      ann prefixname  -> PrefixTemplate      (addAnnToAnnListHelper newa ann) prefixname
    PowerPrefixTemplate ann             -> PowerPrefixTemplate (addAnnToAnnListHelper newa ann)           
  setAnn newa e = case e of
    PrefixTemplate      ann prefixname  -> PrefixTemplate      newa prefixname
    PowerPrefixTemplate ann             -> PowerPrefixTemplate newa           

instance SynTraverse1 PostfixTemplate where
  traverse1 e = case e of
    PostfixTemplate ann postfixname  -> PostfixTemplate ann (traverse1 postfixname) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    PostfixTemplate ann postfixname  -> ann
  addAnn newa e = case e of
    PostfixTemplate ann postfixname  -> PostfixTemplate (addAnnToAnnListHelper newa ann) postfixname 
  setAnn newa e = case e of
    PostfixTemplate ann postfixname  -> PostfixTemplate newa postfixname

instance SynTraverse1 InfixTemplate where
  traverse1 e = case e of
    InfixTemplate ann infixname  -> InfixTemplate ann (traverse1 infixname) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    InfixTemplate ann infixname  -> ann
  addAnn newa e = case e of
    InfixTemplate ann infixname  -> InfixTemplate (addAnnToAnnListHelper newa ann) infixname
  setAnn newa e = case e of
    InfixTemplate ann infixname  -> InfixTemplate newa infixname

instance SynTraverse1 NofixTemplate where
  traverse1 e = case e of
    NofixTemplate ann nofixname  -> NofixTemplate ann (traverse1 nofixname) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    NofixTemplate ann nofixname  -> ann
  addAnn newa e = case e of
    NofixTemplate ann nofixname  -> NofixTemplate (addAnnToAnnListHelper newa ann) nofixname  
  setAnn newa e = case e of
    NofixTemplate ann nofixname  -> NofixTemplate newa nofixname  

instance SynTraverse1 DeclName where
  traverse1 e = case e of
    DName ann name -> DName ann name 
    OpName ann name -> OpName ann name 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    DName ann name -> ann
    OpName ann name -> ann
  addAnn newa e = case e of
    DName   ann name -> DName   (addAnnToAnnListHelper newa ann) name
    OpName  ann name -> OpName  (addAnnToAnnListHelper newa ann) name
  setAnn newa e = case e of
    DName   ann name -> DName   newa name
    OpName  ann name -> OpName  newa name

instance SynTraverse1 RefName where
  traverse1 e = case e of
    RName ann name -> e 
    OpRName ann name -> OpRName ann (traverse1 name)
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    RName ann name -> ann 
    OpRName ann name -> ann 
  addAnn newa e = case e of
    RName   ann name -> RName   (addAnnToAnnListHelper newa ann) name 
    OpRName ann name -> OpRName (addAnnToAnnListHelper newa ann) name 
  setAnn newa e = case e of
    RName   ann name -> RName   newa name 
    OpRName ann name -> OpRName newa name 

instance SynTraverse1 OpName where
  traverse1 e = case e of
    PrefixOpName ann name -> PrefixOpName ann (traverse1 name)
    PostfixOpName ann name -> PostfixOpName ann (traverse1 name) 
    InfixOpName ann name -> InfixOpName ann (traverse1 name) 
    NofixOpName ann name -> NofixOpName ann (traverse1 name)
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    PrefixOpName ann name -> ann
    PostfixOpName ann name -> ann
    InfixOpName ann name -> ann
    NofixOpName ann name -> ann
  addAnn newa e = case e of
    PrefixOpName    ann name -> PrefixOpName    (addAnnToAnnListHelper newa ann) name
    PostfixOpName   ann name -> PostfixOpName   (addAnnToAnnListHelper newa ann) name
    InfixOpName     ann name -> InfixOpName     (addAnnToAnnListHelper newa ann) name
    NofixOpName     ann name -> NofixOpName     (addAnnToAnnListHelper newa ann) name
  setAnn newa e = case e of
    PrefixOpName    ann name -> PrefixOpName    newa name
    PostfixOpName   ann name -> PostfixOpName   newa name
    InfixOpName     ann name -> InfixOpName     newa name
    NofixOpName     ann name -> NofixOpName     newa name

instance SynTraverse1 PrefixName where
  traverse1 e = case e of
    PrePrefixName ann pre -> e 
    PrePPrefixName ann prep -> e 
    LPrefixName ann l listesss eresre -> LPrefixName ann l (traverse1 listesss) (traverse1 eresre) 
    LPPrefixName ann l listesss erepsrep -> LPPrefixName ann l (traverse1 listesss) (traverse1 erepsrep)  
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    PrePrefixName ann pre -> ann
    PrePPrefixName ann prep -> ann
    LPrefixName ann l listesss eresre -> ann
    LPPrefixName ann l listesss erepsrep -> ann
  addAnn newa e = case e of
    PrePrefixName   ann pre                 -> PrePrefixName   (addAnnToAnnListHelper newa ann) pre                
    PrePPrefixName  ann prep                -> PrePPrefixName  (addAnnToAnnListHelper newa ann) prep               
    LPrefixName     ann l listesss eresre   -> LPrefixName     (addAnnToAnnListHelper newa ann) l listesss eresre  
    LPPrefixName    ann l listesss erepsrep -> LPPrefixName    (addAnnToAnnListHelper newa ann) l listesss erepsrep
  setAnn newa e = case e of
    PrePrefixName   ann pre                 -> PrePrefixName   newa pre                
    PrePPrefixName  ann prep                -> PrePPrefixName  newa prep               
    LPrefixName     ann l listesss eresre   -> LPrefixName     newa l listesss eresre  
    LPPrefixName    ann l listesss erepsrep -> LPPrefixName    newa l listesss erepsrep

instance SynTraverse1 PostfixName where
  traverse1 e = case e of
    PostPostfixName ann post -> e
    PostPPostfixName ann postp -> e
    ELPostfixName ann el listesss ersr -> ELPostfixName ann el (traverse1 listesss) (traverse1 ersr)
    ELPPostfixName ann elp listesss erpsrp -> ELPPostfixName ann elp (traverse1 listesss) (traverse1 erpsrp)
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    PostPostfixName ann post -> ann
    PostPPostfixName ann postp -> ann
    ELPostfixName ann el listesss ersr -> ann
    ELPPostfixName ann elp listesss erpsrp -> ann
  addAnn newa e = case e of
    PostPostfixName     ann post                -> PostPostfixName     (addAnnToAnnListHelper newa ann) post               
    PostPPostfixName    ann postp               -> PostPPostfixName    (addAnnToAnnListHelper newa ann) postp              
    ELPostfixName       ann el listesss ersr    -> ELPostfixName       (addAnnToAnnListHelper newa ann) el listesss ersr   
    ELPPostfixName      ann elp listesss erpsrp -> ELPPostfixName      (addAnnToAnnListHelper newa ann) elp listesss erpsrp
  setAnn newa e = case e of
    PostPostfixName     ann post                -> PostPostfixName     newa post               
    PostPPostfixName    ann postp               -> PostPPostfixName    newa postp              
    ELPostfixName       ann el listesss ersr    -> ELPostfixName       newa el listesss ersr   
    ELPPostfixName      ann elp listesss erpsrp -> ELPPostfixName      newa elp listesss erpsrp

instance SynTraverse1 InfixName where
  traverse1 e = case e of
    InInfixName ann ii -> e
    InPInfixName ann ip -> e
    ELInfixName ann el listesss eresre -> ELInfixName ann el (traverse1 listesss) (traverse1 eresre)
    ELPInfixName ann elp listesss erepsrep -> ELPInfixName ann elp (traverse1 listesss) (traverse1 erepsrep)
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    InInfixName ann ii -> ann
    InPInfixName ann ip -> ann
    ELInfixName ann el listesss eresre -> ann
    ELPInfixName ann elp listesss erepsrep -> ann
  addAnn newa e = case e of
    InInfixName     ann ii                      -> InInfixName     (addAnnToAnnListHelper newa ann) ii                   
    InPInfixName    ann ip                      -> InPInfixName    (addAnnToAnnListHelper newa ann) ip                   
    ELInfixName     ann el listesss eresre      -> ELInfixName     (addAnnToAnnListHelper newa ann) el listesss eresre   
    ELPInfixName    ann elp listesss erepsrep   -> ELPInfixName    (addAnnToAnnListHelper newa ann) elp listesss erepsrep
  setAnn newa e = case e of
    InInfixName     ann ii                      -> InInfixName     newa ii                   
    InPInfixName    ann ip                      -> InPInfixName    newa ip                   
    ELInfixName     ann el listesss eresre      -> ELInfixName     newa el listesss eresre   
    ELPInfixName    ann elp listesss erepsrep   -> ELPInfixName    newa elp listesss erepsrep

instance SynTraverse1 NofixName where
  traverse1 e = case e of
    LNofixName ann l listesss ersr -> LNofixName ann l (traverse1 listesss) (traverse1 ersr)
    LPNofixName ann l listesss erpsrp -> LPNofixName ann l (traverse1 listesss) (traverse1 erpsrp)
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    LNofixName ann l listesss ersr -> ann
    LPNofixName ann l listesss erpsrp -> ann
  addAnn newa e = case e of
    LNofixName  ann l listesss ersr     -> LNofixName  (addAnnToAnnListHelper newa ann) l listesss ersr  
    LPNofixName ann l listesss erpsrp   -> LPNofixName (addAnnToAnnListHelper newa ann) l listesss erpsrp
  setAnn newa e = case e of
    LNofixName  ann l listesss ersr     -> LNofixName  newa l listesss ersr  
    LPNofixName ann l listesss erpsrp   -> LPNofixName newa l listesss erpsrp

instance SynTraverse1 ESSS where
  traverse1 e = case e of
    ES ann es -> ES ann (es) 
    SS ann ss -> SS ann (ss) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    ES ann es -> ann
    SS ann ss -> ann
  addAnn newa e = case e of
    ES ann es -> ES (addAnnToAnnListHelper newa ann) es
    SS ann ss -> SS (addAnnToAnnListHelper newa ann) ss
  setAnn newa e = case e of
    ES ann es -> ES newa es
    SS ann ss -> SS newa ss

instance SynTraverse1 ESSS_N where
  traverse1 e = case e of
    ES_N ann name es -> e 
    SS_N ann name ss -> e 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    ES_N ann name es -> ann
    SS_N ann name ss -> ann
  addAnn newa e = case e of
    ES_N ann name es -> ES_N (addAnnToAnnListHelper newa ann) name es
    SS_N ann name ss -> SS_N (addAnnToAnnListHelper newa ann) name ss
  setAnn newa e = case e of
    ES_N ann name es -> ES_N newa name es
    SS_N ann name ss -> SS_N newa name ss

instance SynTraverse1 ESSS_E where
  traverse1 e = case e of
    ES_E ann expr es -> ES_E ann (traverse1 expr) es 
    SS_E ann expr ss -> SS_E ann (traverse1 expr) ss 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    ES_E ann expr es -> ann
    SS_E ann expr ss -> ann
  addAnn newa e = case e of
    ES_E ann expr es -> ES_E (addAnnToAnnListHelper newa ann) expr es 
    SS_E ann expr ss -> SS_E (addAnnToAnnListHelper newa ann) expr ss 
  setAnn newa e = case e of
    ES_E ann expr es -> ES_E newa expr es 
    SS_E ann expr ss -> SS_E newa expr ss 

instance SynTraverse1 ERESRE where
  traverse1 e = case e of
    ERE ann es -> ERE ann (es) 
    SRE ann ss -> SRE ann (ss) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    ERE ann es -> ann
    SRE ann ss -> ann
  addAnn newa e = case e of
    ERE ann es -> ERE (addAnnToAnnListHelper newa ann) es
    SRE ann ss -> SRE (addAnnToAnnListHelper newa ann) ss
  setAnn newa e = case e of
    ERE ann es -> ERE newa es
    SRE ann ss -> SRE newa ss

instance SynTraverse1 ERESRE_N where
  traverse1 e = case e of
    ERE_N ann name es -> e 
    SRE_N ann name ss -> e 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    ERE_N ann name es -> ann
    SRE_N ann name ss -> ann
  addAnn newa e = case e of
    ERE_N ann name es -> ERE_N (addAnnToAnnListHelper newa ann) name es
    SRE_N ann name ss -> SRE_N (addAnnToAnnListHelper newa ann) name ss
  setAnn newa e = case e of
    ERE_N ann name es -> ERE_N newa name es
    SRE_N ann name ss -> SRE_N newa name ss

instance SynTraverse1 ERESRE_E where
  traverse1 e = case e of
    ERE_E ann expr es -> ERE_E ann (traverse1 expr) es 
    SRE_E ann expr ss -> SRE_E ann (traverse1 expr) ss 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    ERE_E ann expr es -> ann
    SRE_E ann expr ss -> ann
  addAnn newa e = case e of
    ERE_E ann expr es -> ERE_E (addAnnToAnnListHelper newa ann) expr es
    SRE_E ann expr ss -> SRE_E (addAnnToAnnListHelper newa ann) expr ss
  setAnn newa e = case e of
    ERE_E ann expr es -> ERE_E newa expr es
    SRE_E ann expr ss -> SRE_E newa expr ss

instance SynTraverse1 EREPSREP where
  traverse1 e = case e of
    EREP ann es -> EREP ann (es) 
    SREP ann ss -> SREP ann (ss) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    EREP ann es -> ann
    SREP ann ss -> ann
  addAnn newa e = case e of
    EREP ann es -> EREP (addAnnToAnnListHelper newa ann) es
    SREP ann ss -> SREP (addAnnToAnnListHelper newa ann) ss
  setAnn newa e = case e of
    EREP ann es -> EREP newa es
    SREP ann ss -> SREP newa ss

instance SynTraverse1 EREPSREP_E where
  traverse1 e = case e of
    EREP_E ann expr es -> EREP_E ann (traverse1 expr) es 
    SREP_E ann expr ss -> SREP_E ann (traverse1 expr) ss 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    EREP_E ann expr es -> ann
    SREP_E ann expr ss -> ann
  addAnn newa e = case e of
    EREP_E ann expr es -> EREP_E (addAnnToAnnListHelper newa ann) expr es
    SREP_E ann expr ss -> SREP_E (addAnnToAnnListHelper newa ann) expr ss
  setAnn newa e = case e of
    EREP_E ann expr es -> EREP_E newa expr es
    SREP_E ann expr ss -> SREP_E newa expr ss

instance SynTraverse1 ERSR where
  traverse1 e = case e of
    ER ann es -> ER ann (es) 
    SR ann ss -> SR ann (ss) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    ER ann es -> ann
    SR ann ss -> ann
  addAnn newa e = case e of
    ER ann es -> ER (addAnnToAnnListHelper newa ann) es
    SR ann ss -> SR (addAnnToAnnListHelper newa ann) ss
  setAnn newa e = case e of
    ER ann es -> ER newa es
    SR ann ss -> SR newa ss

instance SynTraverse1 ERSR_N where
  traverse1 e = case e of
    ER_N ann name es -> e 
    SR_N ann name ss -> e 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    ER_N ann name es -> ann
    SR_N ann name ss -> ann
  addAnn newa e = case e of
    ER_N ann name es -> ER_N (addAnnToAnnListHelper newa ann) name es
    SR_N ann name ss -> SR_N (addAnnToAnnListHelper newa ann) name ss
  setAnn newa e = case e of
    ER_N ann name es -> ER_N newa name es
    SR_N ann name ss -> SR_N newa name ss

instance SynTraverse1 ERSR_E where
  traverse1 e = case e of
    ER_E ann expr es -> ER_E ann (traverse1 expr) es 
    SR_E ann expr ss -> SR_E ann (traverse1 expr) ss 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    ER_E ann expr es -> ann
    SR_E ann expr ss -> ann
  addAnn newa e = case e of
    ER_E ann expr es -> ER_E (addAnnToAnnListHelper newa ann) expr es
    SR_E ann expr ss -> SR_E (addAnnToAnnListHelper newa ann) expr ss
  setAnn newa e = case e of
    ER_E ann expr es -> ER_E newa expr es
    SR_E ann expr ss -> SR_E newa expr ss

instance SynTraverse1 ERPSRP where
  traverse1 e = case e of
    ERP ann es -> ERP ann (es) 
    SRP ann ss -> SRP ann (ss) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    ERP ann es -> ann
    SRP ann ss -> ann
  addAnn newa e = case e of
    ERP ann es -> ERP (addAnnToAnnListHelper newa ann) es
    SRP ann ss -> SRP (addAnnToAnnListHelper newa ann) ss
  setAnn newa e = case e of
    ERP ann es -> ERP newa es
    SRP ann ss -> SRP newa ss

instance SynTraverse1 ERPSRP_E where
  traverse1 e = case e of
    ERP_E ann expr es -> ERP_E ann (traverse1 expr) es 
    SRP_E ann expr ss -> SRP_E ann (traverse1 expr) ss 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    ERP_E ann expr es -> ann
    SRP_E ann expr ss -> ann
  addAnn newa e = case e of
    ERP_E ann expr es -> ERP_E (addAnnToAnnListHelper newa ann) expr es
    SRP_E ann expr ss -> SRP_E (addAnnToAnnListHelper newa ann) expr ss
  setAnn newa e = case e of
    ERP_E ann expr es -> ERP_E newa expr es
    SRP_E ann expr ss -> SRP_E newa expr ss

instance SynTraverse1 GenName where
  traverse1 e = case e of
    PrefixGenName ann name -> PrefixGenName ann (traverse1 name)
    PostfixGenName ann name -> PostfixGenName ann (traverse1 name)
    InfixGenName ann name -> InfixGenName ann (traverse1 name)
    NofixGenName ann name -> NofixGenName ann (traverse1 name)
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    PrefixGenName ann name -> ann
    PostfixGenName ann name -> ann
    InfixGenName ann name -> ann
    NofixGenName ann name -> ann
  addAnn newa e = case e of
    PrefixGenName   ann name -> PrefixGenName   (addAnnToAnnListHelper newa ann) name
    PostfixGenName  ann name -> PostfixGenName  (addAnnToAnnListHelper newa ann) name
    InfixGenName    ann name -> InfixGenName    (addAnnToAnnListHelper newa ann) name
    NofixGenName    ann name -> NofixGenName    (addAnnToAnnListHelper newa ann) name
  setAnn newa e = case e of
    PrefixGenName   ann name -> PrefixGenName   newa name
    PostfixGenName  ann name -> PostfixGenName  newa name
    InfixGenName    ann name -> InfixGenName    newa name
    NofixGenName    ann name -> NofixGenName    newa name

instance SynTraverse1 PrefixGenName where
  traverse1 e = case e of
    PrePrefixGenName ann pre name -> e 
    LPrefixGenName ann l listesss_n eresre_n name -> LPrefixGenName ann l (traverse1 listesss_n) (traverse1 eresre_n) name 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    PrePrefixGenName ann pre name -> ann
    LPrefixGenName ann l listesss_n eresre_n name -> ann
  addAnn newa e = case e of
    PrePrefixGenName    ann pre name                    -> PrePrefixGenName    (addAnnToAnnListHelper newa ann) pre name                   
    LPrefixGenName      ann l listesss_n eresre_n name  -> LPrefixGenName      (addAnnToAnnListHelper newa ann) l listesss_n eresre_n name 
  setAnn newa e = case e of
    PrePrefixGenName    ann pre name                    -> PrePrefixGenName    newa pre name                   
    LPrefixGenName      ann l listesss_n eresre_n name  -> LPrefixGenName      newa l listesss_n eresre_n name 

instance SynTraverse1 PostfixGenName where
  traverse1 e = case e of
    PostPostfixGenName ann name post -> e 
    ELPostfixGenName ann name el listesss_n ersr_n -> ELPostfixGenName ann name el (traverse1 listesss_n) (traverse1 ersr_n) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    PostPostfixGenName ann name post -> ann
    ELPostfixGenName ann name el listesss_n ersr_n -> ann
  addAnn newa e = case e of
    PostPostfixGenName  ann name post                   -> PostPostfixGenName  (addAnnToAnnListHelper newa ann) name post                 
    ELPostfixGenName    ann name el listesss_n ersr_n   -> ELPostfixGenName    (addAnnToAnnListHelper newa ann) name el listesss_n ersr_n 
  setAnn newa e = case e of
    PostPostfixGenName  ann name post                   -> PostPostfixGenName  newa name post                 
    ELPostfixGenName    ann name el listesss_n ersr_n   -> ELPostfixGenName    newa name el listesss_n ersr_n 

instance SynTraverse1 InfixGenName where
  traverse1 e = case e of
    InInfixGenName ann name1 ii name2 -> e 
    ELInfixGenName ann name1 el listesss_n eresre_n name2 -> ELInfixGenName ann name1 el (traverse1 listesss_n) (traverse1 eresre_n) name2 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    InInfixGenName ann name1 ii name2 -> ann
    ELInfixGenName ann name1 el listesss_n eresre_n name2 -> ann
  addAnn newa e = case e of
    InInfixGenName ann name1 ii name2                       -> InInfixGenName (addAnnToAnnListHelper newa ann) name1 ii name2                    
    ELInfixGenName ann name1 el listesss_n eresre_n name2   -> ELInfixGenName (addAnnToAnnListHelper newa ann) name1 el listesss_n eresre_n name2
  setAnn newa e = case e of
    InInfixGenName ann name1 ii name2                       -> InInfixGenName newa name1 ii name2                    
    ELInfixGenName ann name1 el listesss_n eresre_n name2   -> ELInfixGenName newa name1 el listesss_n eresre_n name2

instance SynTraverse1 NofixGenName where
  traverse1 e = case e of
    LNofixGenName ann l listesss_n ersr_n -> LNofixGenName ann l (traverse1 listesss_n) (traverse1 ersr_n) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    LNofixGenName ann l listesss_n ersr_n -> ann
  addAnn newa e = case e of
    LNofixGenName ann l listesss_n ersr_n -> LNofixGenName (addAnnToAnnListHelper newa ann) l listesss_n ersr_n 
  setAnn newa e = case e of
    LNofixGenName ann l listesss_n ersr_n -> LNofixGenName newa l listesss_n ersr_n 

instance SynTraverse1 Relation where
  traverse1 e = case e of
    PrefixRel ann  rel -> PrefixRel ann (traverse1 rel) 
    PostfixRel ann rel -> PostfixRel ann (traverse1 rel) 
    InfixRel ann   rel -> InfixRel ann (traverse1 rel) 
    NofixRel ann   rel -> NofixRel ann (traverse1 rel) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    PrefixRel ann  rel -> ann
    PostfixRel ann rel -> ann
    InfixRel ann   rel -> ann
    NofixRel ann   rel -> ann
  addAnn newa e = case e of
    PrefixRel   ann rel -> PrefixRel   (addAnnToAnnListHelper newa ann) rel
    PostfixRel  ann rel -> PostfixRel  (addAnnToAnnListHelper newa ann) rel
    InfixRel    ann rel -> InfixRel    (addAnnToAnnListHelper newa ann) rel
    NofixRel    ann rel -> NofixRel    (addAnnToAnnListHelper newa ann) rel
  setAnn newa e = case e of
    PrefixRel   ann rel -> PrefixRel   newa rel
    PostfixRel  ann rel -> PostfixRel  newa rel
    InfixRel    ann rel -> InfixRel    newa rel
    NofixRel    ann rel -> NofixRel    newa rel

instance SynTraverse1 PrefixRel where
  traverse1 e = case e of
    PrePPrefixRel ann prep expr -> PrePPrefixRel ann prep (traverse1 expr)
    LPPrefixRel ann lp listesss_e erepsrep_e expr -> LPPrefixRel ann lp listesss_e erepsrep_e expr 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    PrePPrefixRel ann prep expr -> ann
    LPPrefixRel ann lp listesss_e erepsrep_e expr -> ann
  addAnn newa e = case e of
    PrePPrefixRel   ann prep expr                       -> PrePPrefixRel   (addAnnToAnnListHelper newa ann) prep expr                    
    LPPrefixRel     ann lp listesss_e erepsrep_e expr   -> LPPrefixRel     (addAnnToAnnListHelper newa ann) lp listesss_e erepsrep_e expr
  setAnn newa e = case e of
    PrePPrefixRel   ann prep expr                       -> PrePPrefixRel   newa prep expr                    
    LPPrefixRel     ann lp listesss_e erepsrep_e expr   -> LPPrefixRel     newa lp listesss_e erepsrep_e expr

instance SynTraverse1 PostfixRel where
  traverse1 e = case e of
    PostPPostfixRel ann expr postp -> PostPPostfixRel ann (traverse1 expr) postp 
    ELPPostfixRel ann expr elp listesss_e erpsrp_e -> ELPPostfixRel ann (traverse1 expr) elp (traverse1 listesss_e) (traverse1 erpsrp_e) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    PostPPostfixRel ann expr postp -> ann
    ELPPostfixRel ann expr elp listesss_e erpsrp_e -> ann
  addAnn newa e = case e of
    PostPPostfixRel ann expr postp                      -> PostPPostfixRel (addAnnToAnnListHelper newa ann) expr postp                  
    ELPPostfixRel   ann expr elp listesss_e erpsrp_e    -> ELPPostfixRel   (addAnnToAnnListHelper newa ann) expr elp listesss_e erpsrp_e
  setAnn newa e = case e of
    PostPPostfixRel ann expr postp                      -> PostPPostfixRel newa expr postp                  
    ELPPostfixRel   ann expr elp listesss_e erpsrp_e    -> ELPPostfixRel   newa expr elp listesss_e erpsrp_e

instance SynTraverse1 IP_E where
  traverse1 e = case e of
    IP_E ann str expr -> IP_E ann str (traverse1 expr) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    IP_E ann str expr -> ann
  addAnn newa e = case e of
    IP_E ann str expr -> IP_E (addAnnToAnnListHelper newa ann) str expr 
  setAnn newa e = case e of
    IP_E ann str expr -> IP_E newa str expr 

instance SynTraverse1 InfixRel where
  traverse1 e = case e of
    InInfixRel ann expr listip_e -> InInfixRel ann (traverse1 expr) (traverse1 listip_e) 
    ELPInfixRel ann expr1 elp listesss_e erepsrep_e expr2 -> ELPInfixRel ann (traverse1 expr1) elp (traverse1 listesss_e) (traverse1 erepsrep_e) (traverse1 expr2) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    InInfixRel ann expr listip_e -> ann
    ELPInfixRel ann expr1 elp listesss_e erepsrep_e expr2 -> ann
  addAnn newa e = case e of
    InInfixRel  ann expr listip_e                           -> InInfixRel  (addAnnToAnnListHelper newa ann) expr listip_e                        
    ELPInfixRel ann expr1 elp listesss_e erepsrep_e expr2   -> ELPInfixRel (addAnnToAnnListHelper newa ann) expr1 elp listesss_e erepsrep_e expr2
  setAnn newa e = case e of
    InInfixRel  ann expr listip_e                           -> InInfixRel  newa expr listip_e                        
    ELPInfixRel ann expr1 elp listesss_e erepsrep_e expr2   -> ELPInfixRel newa expr1 elp listesss_e erepsrep_e expr2

instance SynTraverse1 NofixRel where
  traverse1 e = case e of
    LPNofixRel ann lp listesss_e erepserp_e -> LPNofixRel ann lp (traverse1 listesss_e) (traverse1 erepserp_e) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    LPNofixRel ann lp listesss_e erepserp_e -> ann
  addAnn newa e = case e of
    LPNofixRel ann lp listesss_e erepserp_e -> LPNofixRel (addAnnToAnnListHelper newa ann) lp listesss_e erepserp_e 
  setAnn newa e = case e of
    LPNofixRel ann lp listesss_e erepserp_e -> LPNofixRel newa lp listesss_e erepserp_e 

instance SynTraverse1 Application where
  traverse1 e = case e of
    PrefixApp ann  app -> PrefixApp ann  (traverse1 app)
    PostfixApp ann app -> PostfixApp ann (traverse1 app) 
    InfixApp ann   app -> InfixApp ann (traverse1 app) 
    NofixApp ann   app -> NofixApp ann (traverse1 app) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    PrefixApp ann  app -> ann
    PostfixApp ann app -> ann
    InfixApp ann   app -> ann
    NofixApp ann   app -> ann
  addAnn newa e = case e of
    PrefixApp   ann app -> PrefixApp   (addAnnToAnnListHelper newa ann) app
    PostfixApp  ann app -> PostfixApp  (addAnnToAnnListHelper newa ann) app
    InfixApp    ann app -> InfixApp    (addAnnToAnnListHelper newa ann) app
    NofixApp    ann app -> NofixApp    (addAnnToAnnListHelper newa ann) app
  setAnn newa e = case e of
    PrefixApp   ann app -> PrefixApp   newa app
    PostfixApp  ann app -> PostfixApp  newa app
    InfixApp    ann app -> InfixApp    newa app
    NofixApp    ann app -> NofixApp    newa app

instance SynTraverse1 PrefixApp where
  traverse1 e = case e of
    PrePrefixApp ann pre expr -> PrePrefixApp ann pre (traverse1 expr) 
    LPrefixApp ann l listesss_e eresre_e expr -> LPrefixApp ann l (traverse1 listesss_e) (traverse1 eresre_e) (traverse1 expr) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    PrePrefixApp ann pre expr -> ann
    LPrefixApp ann l listesss_e eresre_e expr -> ann
  addAnn newa e = case e of
    PrePrefixApp    ann pre expr                    -> PrePrefixApp    (addAnnToAnnListHelper newa ann) pre expr                  
    LPrefixApp      ann l listesss_e eresre_e expr  -> LPrefixApp      (addAnnToAnnListHelper newa ann) l listesss_e eresre_e expr
  setAnn newa e = case e of
    PrePrefixApp    ann pre expr                    -> PrePrefixApp    newa pre expr                  
    LPrefixApp      ann l listesss_e eresre_e expr  -> LPrefixApp      newa l listesss_e eresre_e expr

instance SynTraverse1 PostfixApp where
  traverse1 e = case e of
    PostPostfixApp ann expr post -> PostPostfixApp ann (traverse1 expr) post 
    ELPostfixApp ann expr el listesss_e ersr_e -> ELPostfixApp ann (traverse1 expr) el (traverse1 listesss_e) (traverse1 ersr_e) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    PostPostfixApp ann expr post -> ann
    ELPostfixApp ann expr el listesss_e ersr_e -> ann
  addAnn newa e = case e of
    PostPostfixApp  ann expr post                   -> PostPostfixApp  (addAnnToAnnListHelper newa ann) expr post                
    ELPostfixApp    ann expr el listesss_e ersr_e   -> ELPostfixApp    (addAnnToAnnListHelper newa ann) expr el listesss_e ersr_e
  setAnn newa e = case e of
    PostPostfixApp  ann expr post                   -> PostPostfixApp  newa expr post                
    ELPostfixApp    ann expr el listesss_e ersr_e   -> ELPostfixApp    newa expr el listesss_e ersr_e

instance SynTraverse1 InfixApp where
  traverse1 e = case e of
    InInfixApp ann expr1 ii expr2 -> InInfixApp ann (traverse1 expr1) ii (traverse1 expr2)
    ELInfixApp ann expr1 el listesss_e eresre_e expr2 -> ELInfixApp ann (traverse1 expr1) el (traverse1 listesss_e) (traverse1 eresre_e) (traverse1 expr2) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    InInfixApp ann expr1 ii expr2 -> ann
    ELInfixApp ann expr1 el listesss_e eresre_e expr2 -> ann
  addAnn newa e = case e of
    InInfixApp ann expr1 ii expr2                       -> InInfixApp (addAnnToAnnListHelper newa ann) expr1 ii expr2                     
    ELInfixApp ann expr1 el listesss_e eresre_e expr2   -> ELInfixApp (addAnnToAnnListHelper newa ann) expr1 el listesss_e eresre_e expr2 
  setAnn newa e = case e of
    InInfixApp ann expr1 ii expr2                       -> InInfixApp newa expr1 ii expr2                     
    ELInfixApp ann expr1 el listesss_e eresre_e expr2   -> ELInfixApp newa expr1 el listesss_e eresre_e expr2 

instance SynTraverse1 NofixApp where
  traverse1 e = case e of
    LNofixApp ann l listesss_e erse_e -> LNofixApp ann l (traverse1 listesss_e) (traverse1 erse_e) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    LNofixApp ann l listesss_e erse_e -> ann
  addAnn newa e = case e of
    LNofixApp ann l listesss_e erse_e -> LNofixApp (addAnnToAnnListHelper newa ann) l listesss_e erse_e
  setAnn newa e = case e of
    LNofixApp ann l listesss_e erse_e -> LNofixApp newa l listesss_e erse_e

instance SynTraverse1 ChannelDecl where
  traverse1 e = case e of
    ChannelDecl ann simplec -> ChannelDecl ann (traverse1 simplec) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    ChannelDecl ann simplec -> ann
  addAnn newa e = case e of
    _   -> e
  setAnn newa e = case e of
    _   -> e

instance SynTraverse1 SimpleCDecl where
  traverse1 e = case e of
    SyncSimpleCDecl ann listname -> e
    TypedSimpleCDecl ann listname expr -> TypedSimpleCDecl ann listname (traverse1 expr) 
    GenericTypedSimpleCDecl ann formals listname expr -> GenericTypedSimpleCDecl ann (traverse1 formals) listname (traverse1 expr)
--    SchemaSimpleCDecl expr -> SchemaSimpleCDecl (traverse1 expr) 
    SimpleCDecl2 ann listname decl2 -> case decl2 of
                            SyncSimpleCDecl2 a1 -> SyncSimpleCDecl ann listname
                            TypedSimpleCDecl2 a1 expr -> TypedSimpleCDecl ann listname (traverse1 expr) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    SyncSimpleCDecl ann listname -> ann
    TypedSimpleCDecl ann listname expr -> ann
    GenericTypedSimpleCDecl ann formals listname expr -> ann
    SimpleCDecl2 ann listname decl2 -> ann
  addAnn newa e = case e of
    _   -> e
  setAnn newa e = case e of
    _   -> e

instance SynTraverse1 ChannelFromDecl where
  traverse1 e = case e of
    ChannelFromDecl ann fromcdecl -> ChannelFromDecl ann (traverse1 fromcdecl) 
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    ChannelFromDecl ann fromcdecl -> ann
  addAnn newa e = case e of
    _   -> e
  setAnn newa e = case e of
    _   -> e

instance SynTraverse1 FromCDecl where
  traverse1 e = case e of
    RefFromCDecl ann expr -> RefFromCDecl ann (traverse1 expr)
    RefDecorFromCDecl ann expr -> RefDecorFromCDecl ann (traverse1 expr)
  traverse1List [] = []
  traverse1List (x:xs) = (:) (traverse1 x) (traverse1 xs)
  getAnn e = case e of
    RefFromCDecl ann expr -> ann
    RefDecorFromCDecl ann expr -> ann
  addAnn newa e = case e of
    _   -> e
  setAnn newa e = case e of
    _   -> e

instance SynTraverse1 ChannelSetDecl where
  traverse1 e = case e of
    ChannelSetDecl ann name expr -> ChannelSetDecl ann name (traverse1 expr) 
  getAnn e = case e of
    ChannelSetDecl ann name expr -> ann 
  addAnn newa e = case e of
    _   -> e
  setAnn newa e = case e of
    _   -> e

instance SynTraverse1 CSExp where
  traverse1 e = case e of
    CSExpEmpty ann  -> e 
    CSExpExt ann exprs -> CSExpExt ann (traverse1 exprs)
    CSExpRef ann expr -> CSExpRef ann (traverse1 expr) 
    CSExpUnion ann csexp1 csexp2  -> CSExpUnion ann (traverse1 csexp1) (traverse1 csexp2)
    CSExpInter ann csexp1 csexp2  -> CSExpInter ann (traverse1 csexp1) (traverse1 csexp2)
    CSExpDiff ann csexp1 csexp2  -> CSExpDiff ann (traverse1 csexp1) (traverse1 csexp2)
  getAnn e = case e of
    CSExpEmpty ann  -> ann 
    CSExpExt ann exprs -> ann 
    CSExpRef ann expr -> ann 
    CSExpUnion ann csexp1 csexp2  -> ann 
    CSExpInter ann csexp1 csexp2  -> ann 
    CSExpDiff ann csexp1 csexp2  -> ann
  addAnn newa e = case e of
    _   -> e
  setAnn newa e = case e of
    _   -> e

instance SynTraverse1 Token where
  traverse1 e = case e of
    (PT pn@(AlexPn o l c) _ _ ) -> e 
    Err pn@(AlexPn o l c)       -> e 
    _                           -> e 
  getAnn e = case e of
    (PT pn@(AlexPn o l c) _ _ ) -> [PosAnn (Pn o l c)]
    Err pn@(AlexPn o l c)       -> [PosAnn (Pn o l c)]
    _                           -> [PosAnn (Pn 0 0 0)]
  addAnn newa e = case e of
    _   -> e
  setAnn newa e = case e of
    _   -> e
