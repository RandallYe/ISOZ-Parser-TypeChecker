{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{- |
Module      : Language.ISOZ.Parser.SynTransformerZero 
Description : 
Copyright   : (C) Kangfeng Ye, 2017
License     : BSD3
Maintainer  : ye.randall@gmail.com 
Stability   : experimental

Syntax transformation for ExprPredicate to Expression or Predicate depending on their context. In addition, check only schema expressions when they are expected.  It provides

- synTraverseZeroTree : traverse the original syntax tree 

-}
module Language.ISOZ.Parser.SynTransformerZero where

import Language.ISOZ.Common.Error
import Language.ISOZ.Common.AbsIsoz
import Language.ISOZ.ZChar 
import Data.Char
import Data.List
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Language.ISOZ.Parser.Mktuple
import Language.ISOZ.Common.Ann
import Language.ISOZ.Lexer.ISOZLexer 
import Language.ISOZ.Common.OpTableMonad 
import Language.ISOZ.Utils.PrintIsoz (printTree)
import Text.Read (readMaybe)
import Language.ISOZ.Parser.SynTransformerOne (getAnnTerm, setAnnTerm, getAnnTermAndRemoveExprInPars, showPos)

-- | Traverse a syntax tree for transformation of ExprPredicate.
synTraverseZeroTree :: SynTraverse0 a => a -> Either String a 
synTraverseZeroTree = \s -> case runOpTableType (traverseZero s) of
                        Left (err) -> Left err
                        Right (Left err) -> Left err
                        Right (Right e) -> Right e

class SynTraverse0 a where
    traverseZero :: a -> OpTableType (Either String a)
    traverseZeroList :: [a] -> OpTableType (Either String [a])
    traverseZeroList [] = return (Right [])
    traverseZeroList (x:xs) = do 
        m <- traverseZero x
        case m of 
            Left err -> return (Left err)
            Right e -> do 
                n <- traverseZero xs
                case n of
                     Left err -> return (Left err)
                     Right es -> return (Right ((:) e es))

instance SynTraverse0 a => SynTraverse0 [a] where
  traverseZero = traverseZeroList

instance SynTraverse0 Specification where
  traverseZero e = case e of
    SpecSect ann sections -> do
            setOpTableType (opmap) 
            setSchNameTable (schmap) 
            n <- (traverseZero sections) ; case n of
                Left err -> return (Left err)
                Right s  -> return (Right (SpecSect ann s))
      where opmap = getOpMapAnnFromListAnns ann 
            schmap = getSchNameMapAnnFromListAnns ann
    SpecAnony ann paragraphs -> do
            setOpTableType (opmap) 
            setSchNameTable (schmap) 
            n <- (traverseZero paragraphs) 
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (SpecAnony ann s)) 
      where opmap = getOpMapAnnFromListAnns ann 
            schmap = getSchNameMapAnnFromListAnns ann
    SpecEmpty ann -> return (Right (SpecEmpty ann))

instance SynTraverse0 Section where
  traverseZero e = case e of
    Section ann zed sectname secparent end paragraphs -> do
        n <- traverseZero paragraphs 
        case n of 
                Left err -> return (Left err)
                Right s  -> return (Right (Section ann zed sectname secparent end s)) 
    BaseSection ann zed sectname end paragraphs -> do
        n <- traverseZero paragraphs 
        case n of 
                Left err -> return (Left err)
                Right s  -> return (Right (BaseSection ann zed sectname end s)) 
    InheritingSection ann zed sectname listname end paragraphs -> do 
        n <- traverseZero paragraphs 
        case n of 
                Left err -> return (Left err)
                Right s  -> return (Right (InheritingSection ann zed sectname listname end s)) 

instance SynTraverse0 ParagraphBody where
  traverseZero e = case e of
    GivenPara2 a1 listname -> return (Right (GivenPara2 a1 listname))
    EPHDefPara2 a4 name expr -> do
        n <- ep2expr expr 
        case n of
            Left err -> return (Left err)
            Right s  -> return (Right (HDefPara2 a4 name s)) 
    EPGenHDefPara2 a5 name formals expr -> do
        n <- ep2expr expr 
        case n of
            Left err -> return (Left err)
            Right s  -> return (Right (GenHDefPara2 a5 name formals s)) 
    EPGenOpDefPara2 a6 genname expr -> do 
        n <- ep2expr expr 
        case n of
            Left err -> return (Left err)
            Right s  -> return (Right (GenOpDefPara2 a6 genname s)) 
    FreetypePara2 a7 listtype -> do 
        n <- traverseZero listtype 
        case n of
            Left err -> return (Left err)
            Right s  -> return (Right (FreetypePara2 a7 s)) 
    OperatorPara2 a9 optemplate -> do 
        n <- traverseZero optemplate 
        case n of
            Left err -> return (Left err)
            Right s  -> return (Right (OperatorPara2 a9 s)) 

instance SynTraverse0 Paragraph where
  traverseZero e = case e of
    AxPara ann zed parabody end -> do
            n <- traverseZero parabody 
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (AxPara ann zed s end )) 
    GivenPara ann listname -> return (Right (GivenPara ann listname))
    AxdefPara ann ax schematext end -> do
            n <- (traverseZero schematext) 
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (AxdefPara ann ax s end)) 
    SchdefPara ann sch name schematext end -> do
            n <- (traverseZero schematext) 
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (SchdefPara ann sch name s end)) 
    GenAxdefPara ann ax formals schematext end -> do
            n <- (traverseZero schematext) 
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (GenAxdefPara ann ax formals s end)) 
    GenSchdefPara ann sch formals name schematext end -> do
            n <- (traverseZero schematext) 
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (GenSchdefPara ann sch formals name s end)) 
    EPGenConjecturePara ann name formals pred -> do
            n <- (ep2pred pred)
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (GenConjecturePara ann name formals s)) 
    EPConjecturePara ann name pred -> do
            n <- (ep2pred pred)
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (ConjecturePara ann name s)) 

instance SynTraverse0 Freetype where
  traverseZero e = case e of
    Freetype ann name listbranches -> do
            n <- (traverseZero listbranches)
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (Freetype ann name s)) 
-- 
instance SynTraverse0 Branch where
  traverseZero e = case e of
    ConstantBranch ann declname -> do
            n <- (traverseZero declname)
            case n of
                Left err -> return (Left err)
                Right s  -> return (Right (ConstantBranch ann s)) 
    EPConstructorBranch ann declname expr -> do
            dl <- (traverseZero declname)
            n <- (ep2expr expr)
            case dl of
                Left err -> return (Left err)
                Right dl'  -> case n of
                    Left err -> return (Left err)
                    Right s  -> return (Right (ConstructorBranch ann dl' s)) 
-- 
instance SynTraverse0 Formals where
  traverseZero e = case e of
    Formals ann listname -> return (Right e)
-- 
instance SynTraverse0 RenamePair where
  traverseZero e = case e of
    RenamePair ann newname oldname -> return (Right e)
--  traverseZeroList [] = []
--  traverseZeroList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverse0 SchemaText where
  traverseZero e = case e of
    SchemaTextEmpty ann -> return (Right e) 
    SchemaTextDecl ann decl -> do
        dl <- traverseZero decl 
        case dl of
            Left err    -> return (Left err)
            Right dl'   -> return (Right (SchemaTextDecl ann dl')) 
    EPSchemaTextPred ann pred -> do
        pr <- ep2pred pred 
        case pr of
            Left err    -> return (Left err)
            Right pr'   -> return (Right (SchemaTextPred ann pr')) 
    EPSchemaText ann decl pred -> do
        dl <- traverseZero decl 
        pr <- ep2pred pred 
        case dl of
            Left err    -> return (Left err)
            Right dl'   -> case pr of
                Left err    -> return (Left err)
                Right pr'   -> return (Right (SchemaText ann dl' pr')) 
-- 
instance SynTraverse0 DeclPart where
  traverseZero e = case e of
    DeclPart ann decls -> do
        dl <- (traverseZero decls) 
        case dl of
            Left err -> return (Left err)
            Right dl' -> return (Right (DeclPart ann dl'))
-- 
instance SynTraverse0 Declaration where
  traverseZero e = case e of
    EPDeclaration ann declnames expr -> do
        dl <- traverseZero declnames 
        er <- ep2expr expr
        case dl of
            Left err -> return (Left err)
            Right dl' -> case er of
                Left err -> return (Left err)
                Right er' -> return (Right (Declaration ann dl' er'))
    EPAbbrDecl ann declname expr -> do 
        dl <- traverseZero declname
        er <- ep2expr expr
        case dl of
            Left err -> return (Left err)
            Right dl' -> case er of
                Left err -> return (Left err)
                Right er' -> return (Right (AbbrDecl ann dl' er'))
    EPExprDecl ann expr -> do 
        er <- ep2expr expr 
        case er of
            Left err -> return (Left err)
            Right er' -> return (Right (ExprDecl ann er'))
-- 
instance SynTraverse0 OperatorTemplate where
  traverseZero e = case e of
    RelationTemplate ann template -> do
        tr <- traverseZero template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (RelationTemplate ann tr'))
    FunctionTemplate ann template -> do
        tr <- traverseZero template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (FunctionTemplate ann tr'))
    GenericTemplate ann template -> do
        tr <- traverseZero template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (GenericTemplate ann tr'))
-- 
instance SynTraverse0 CategoryTemplate where
  traverseZero e = case e of
    PrefixCatTemplate ann template -> do
        tr <- traverseZero template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (PrefixCatTemplate ann tr'))
    PostfixCatTemplate ann template -> do
        tr <- traverseZero template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (PostfixCatTemplate ann tr'))
    InfixCatTemplate ann prec assoc template -> do
        tr <- traverseZero template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (InfixCatTemplate ann prec assoc tr'))
    NofixCatTemplate ann template -> do
        tr <- traverseZero template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (NofixCatTemplate ann tr'))
-- 
instance SynTraverse0 Prec where
  traverseZero e = case e of
    Prec ann _ -> return (Right e)

instance SynTraverse0 Assoc where
  traverseZero e = case e of
    LeftAssoc ann -> return (Right e) 
    RightAssoc ann -> return (Right e) 
-- 
instance SynTraverse0 Template where
  traverseZero e = case e of
    TPrefixTemplate ann template -> do
        tr <- traverseZero template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (TPrefixTemplate ann tr')) 
    TPostfixTemplate ann template -> do
        tr <- traverseZero template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (TPostfixTemplate ann tr')) 
    TInfixTemplate ann template -> do
        tr <- traverseZero template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (TInfixTemplate ann tr')) 
    TNofixTemplate ann template -> do
        tr <- traverseZero template
        case tr of
            Left err    -> return (Left err)
            Right tr'   -> return (Right (TNofixTemplate ann tr')) 
-- 
instance SynTraverse0 PrefixTemplate where
  traverseZero e = case e of
    PrefixTemplate ann prefixname  -> do
        pr <- traverseZero prefixname
        case pr of
            Left err    -> return (Left err)
            Right pr'   -> return (Right (PrefixTemplate ann pr'))
    PowerPrefixTemplate ann        -> return (Right e) 
-- 
instance SynTraverse0 PostfixTemplate where
  traverseZero e = case e of
    PostfixTemplate ann postfixname  -> do
        pr <- traverseZero postfixname
        case pr of
            Left err    -> return (Left err)
            Right pr'   -> return (Right (PostfixTemplate ann pr'))
-- 
instance SynTraverse0 InfixTemplate where
  traverseZero e = case e of
    InfixTemplate ann infixname  -> do
        pr <- traverseZero infixname
        case pr of
            Left err    -> return (Left err)
            Right pr'   -> return (Right (InfixTemplate ann pr'))
-- 
instance SynTraverse0 NofixTemplate where
  traverseZero e = case e of
    NofixTemplate ann nofixname  -> do
        pr <- traverseZero nofixname
        case pr of
            Left err    -> return (Left err)
            Right pr'   -> return (Right (NofixTemplate ann pr'))
-- 
-- 
instance SynTraverse0 DeclName where
  traverseZero e = case e of
    DName ann name -> return (Right e) 
    OpName ann name -> return (Right e)
-- 
instance SynTraverse0 RefName where
  traverseZero e = case e of
    RName ann name -> return (Right e)
    OpRName ann name ->  return (Right e)
-- 
instance SynTraverse0 OpName where
  traverseZero e = case e of
    PrefixOpName ann name -> return (Right e)
    PostfixOpName ann name -> return (Right e)
    InfixOpName ann name -> return (Right e)
    NofixOpName ann name -> return (Right e)
-- 
instance SynTraverse0 PrefixName where
  traverseZero e = case e of
    PrePrefixName ann pre -> return (Right e)
    PrePPrefixName ann prep -> return (Right e) 
    LPrefixName ann l listesss eresre -> return (Right e)
    LPPrefixName ann l listesss erepsrep -> return (Right e)
-- 
instance SynTraverse0 PostfixName where
  traverseZero e = case e of
    PostPostfixName ann post -> return (Right e)
    PostPPostfixName ann postp -> return (Right e)
    ELPostfixName ann el listesss ersr -> return (Right e)
    ELPPostfixName ann elp listesss erpsrp -> return (Right e)
-- 
instance SynTraverse0 InfixName where
  traverseZero e = case e of
    InInfixName ann ii -> return (Right e) 
    InPInfixName ann ip -> return (Right e)
    ELInfixName ann el listesss eresre -> return (Right e)
    ELPInfixName ann elp listesss erepsrep -> return (Right e)
-- 
instance SynTraverse0 NofixName where
  traverseZero e = case e of
    LNofixName ann l listesss ersr -> return (Right e) 
    LPNofixName ann l listesss erpsrp -> return (Right e)
-- 
-- instance SynTraverse0 ESSS where
--   traverseZero e = case e of
--     ES ann es -> ES ann (es) 
--     SS ann ss -> SS ann (ss) 
--   traverseZeroList [] = []
--   traverseZeroList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
-- instance SynTraverse0 ESSS_N where
--   traverseZero e = case e of
--     ES_N ann name es -> e 
--     SS_N ann name ss -> e 
--   traverseZeroList [] = []
--   traverseZeroList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverse0 ESSS_E where
  traverseZero e = case e of
    EPES_E ann expr es -> do
        er <- ep2expr expr 
        case er of
            Left err  -> return (Left err) 
            Right er' -> return (Right (ES_E ann er' es)) 
    EPSS_E ann expr ss -> do
        er <- ep2exprList expr 
        case er of
            Left err  -> return (Left err) 
            Right er' -> return (Right (SS_E ann er' ss)) 
--   traverseZeroList [] = []
--   traverseZeroList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
-- instance SynTraverse0 ERESRE where
--   traverseZero e = case e of
--     ERE ann es -> ERE ann (es) 
--     SRE ann ss -> SRE ann (ss) 
--   traverseZeroList [] = []
--   traverseZeroList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
-- instance SynTraverse0 ERESRE_N where
--   traverseZero e = case e of
--     ERE_N ann name es -> e 
--     SRE_N ann name ss -> e 
--   traverseZeroList [] = []
--   traverseZeroList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverse0 ERESRE_E where
  traverseZero e = case e of
    EPERE_E ann expr es -> do
        er <- ep2expr expr 
        case er of
            Left err  -> return (Left err) 
            Right er' -> return (Right (ERE_E ann er' es)) 
    EPSRE_E ann expr ss -> do
        er <- ep2exprList expr 
        case er of
            Left err  -> return (Left err) 
            Right er' -> return (Right (SRE_E ann er' ss)) 
--   traverseZeroList [] = []
--   traverseZeroList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
-- instance SynTraverse0 EREPSREP where
--   traverseZero e = case e of
--     EREP ann es -> EREP ann (es) 
--     SREP ann ss -> SREP ann (ss) 
--   traverseZeroList [] = []
--   traverseZeroList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverse0 EREPSREP_E where
  traverseZero e = case e of
    EPEREP_E ann expr es -> do
        er <- ep2expr expr 
        case er of
            Left err  -> return (Left err) 
            Right er' -> return (Right (EREP_E ann er' es)) 
    EPSREP_E ann expr ss -> do
        er <- ep2exprList expr 
        case er of
            Left err  -> return (Left err) 
            Right er' -> return (Right (SREP_E ann er' ss)) 
--   traverseZeroList [] = []
--   traverseZeroList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
-- instance SynTraverse0 ERSR where
--   traverseZero e = case e of
--     ER ann es -> ER ann (es) 
--     SR ann ss -> SR ann (ss) 
--   traverseZeroList [] = []
--   traverseZeroList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
-- instance SynTraverse0 ERSR_N where
--   traverseZero e = case e of
--     ER_N ann name es -> e 
--     SR_N ann name ss -> e 
--   traverseZeroList [] = []
--   traverseZeroList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverse0 ERSR_E where
  traverseZero e = case e of
    EPER_E ann expr es -> do
        er <- ep2expr expr 
        case er of
            Left err  -> return (Left err) 
            Right er' -> return (Right (ER_E ann er' es)) 
    EPSR_E ann expr ss -> do
        er <- ep2exprList expr 
        case er of
            Left err  -> return (Left err) 
            Right er' -> return (Right (SR_E ann er' ss)) 
--   traverseZeroList [] = []
--   traverseZeroList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
-- instance SynTraverse0 ERPSRP where
--   traverseZero e = case e of
--     ERP ann es -> ERP ann (es) 
--     SRP ann ss -> SRP ann (ss) 
--   traverseZeroList [] = []
--   traverseZeroList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverse0 ERPSRP_E where
  traverseZero e = case e of
    EPERP_E ann expr es -> do
        er <- ep2expr expr 
        case er of
            Left err  -> return (Left err) 
            Right er' -> return (Right (ERP_E ann er' es)) 
    EPSRP_E ann expr ss -> do
        er <- ep2exprList expr 
        case er of
            Left err  -> return (Left err) 
            Right er' -> return (Right (SRP_E ann er' ss)) 
--   traverseZeroList [] = []
--   traverseZeroList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverse0 GenName where
  traverseZero e = case e of
    PrefixGenName ann name -> return (Right e)
    PostfixGenName ann name -> return (Right e)
    InfixGenName ann name -> return (Right e)
    NofixGenName ann name -> return (Right e) 
--   traverseZeroList [] = []
--   traverseZeroList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverse0 PrefixGenName where
  traverseZero e = case e of
    PrePrefixGenName ann pre name -> return (Right e) 
    LPrefixGenName ann l listesss_n eresre_n name -> return (Right e)
--   traverseZeroList [] = []
--   traverseZeroList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverse0 PostfixGenName where
  traverseZero e = case e of
    PostPostfixGenName ann name post -> return (Right e)
    ELPostfixGenName ann name el listesss_n ersr_n -> return (Right e) 
--   traverseZeroList [] = []
--   traverseZeroList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverse0 InfixGenName where
  traverseZero e = case e of
    InInfixGenName ann name1 ii name2 -> return (Right e)
    ELInfixGenName ann name1 el listesss_n eresre_n name2 -> return (Right e) 
--   traverseZeroList [] = []
--   traverseZeroList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverse0 NofixGenName where
  traverseZero e = case e of
    LNofixGenName ann l listesss_n ersr_n -> return (Right e) 
--   traverseZeroList [] = []
--   traverseZeroList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverse0 Relation where
  traverseZero e = case e of
    PrefixRel ann  rel -> do
        rr <- traverseZero rel
        case rr of
            Left err -> return (Left err)
            Right rr' -> return (Right (PrefixRel ann rr')) 
    PostfixRel ann rel -> do
        rr <- traverseZero rel
        case rr of
            Left err -> return (Left err)
            Right rr' -> return (Right (PostfixRel ann rr')) 
    InfixRel ann   rel -> do
        rr <- traverseZero rel
        case rr of
            Left err -> return (Left err)
            Right rr' -> return (Right (InfixRel ann rr')) 
    NofixRel ann   rel -> do
        rr <- traverseZero rel
        case rr of
            Left err -> return (Left err)
            Right rr' -> return (Right (NofixRel ann rr')) 
-- 
instance SynTraverse0 PrefixRel where
  traverseZero e = case e of
    EPPrePPrefixRel ann prep expr -> do
        er <- ep2expr expr 
        case er of
            Left err -> return (Left err)
            Right er' -> return (Right (PrePPrefixRel ann prep er')) 
    EPLPPrefixRel ann lp listesss_e erepsrep_e expr -> do
        esr <- traverseZero listesss_e 
        erpr <- traverseZero erepsrep_e 
        er <- ep2expr expr 
        case esr of
            Left err    -> return (Left err)
            Right esr'  -> case erpr of
                Left err    -> return (Left err)
                Right erpr' -> case er of
                    Left err  -> return (Left err)
                    Right er' -> return (Right (LPPrefixRel ann lp esr' erpr' er')) 
-- 
instance SynTraverse0 PostfixRel where
  traverseZero e = case e of
    EPPostPPostfixRel ann expr postp -> do
        er <- ep2expr expr 
        case er of
            Left err -> return (Left err)
            Right er' -> return (Right (PostPPostfixRel ann er' postp)) 
    EPELPPostfixRel ann expr elp listesss_e erpsrp_e -> do
        er <- ep2expr expr 
        esr <- traverseZero listesss_e 
        erpr <- traverseZero erpsrp_e 
        case esr of
            Left err    -> return (Left err)
            Right esr'  -> case erpr of
                Left err    -> return (Left err)
                Right erpr' -> case er of
                    Left err  -> return (Left err)
                    Right er' -> return (Right (ELPPostfixRel ann er' elp esr' erpr')) 
-- 
instance SynTraverse0 IP_E where
  traverseZero e = case e of
    EPIP_E ann str expr -> do
        er <- ep2expr expr
        case er of
            Left err    -> return (Left err)
            Right er'   -> return (Right (IP_E ann str er')) 
-- 
instance SynTraverse0 InfixRel where
  traverseZero e = case e of
    EPInInfixRel ann expr listip_e -> do
        er <- ep2expr expr
        ipr <- traverseZero listip_e 
        case er of
            Left err    -> return (Left err)
            Right er'   -> case ipr of
                Left err    -> return (Left err)
                Right ipr'  -> return (Right (InInfixRel ann er' ipr')) 
    EPELPInfixRel ann expr1 elp listesss_e erepsrep_e expr2 -> do
        er1 <- ep2expr expr1
        er2 <- ep2expr expr2
        esr <- traverseZero listesss_e 
        epr <- traverseZero erepsrep_e 
        case er1 of
            Left err    -> return (Left err)
            Right er1'   -> case er2 of
                Left err    -> return (Left err)
                Right er2'   ->  case esr of
                    Left err    -> return (Left err)
                    Right esr'  -> case epr of
                        Left err    -> return (Left err)
                        Right epr'  -> return (Right (ELPInfixRel ann er1' elp esr' epr' er2')) 
-- 
instance SynTraverse0 NofixRel where
  traverseZero e = case e of
    LPNofixRel ann lp listesss_e erepserp_e -> do
        esr <- traverseZero listesss_e 
        epr <- traverseZero erepserp_e 
        case esr of
            Left err    -> return (Left err)
            Right esr'   -> case epr of
                Left err    -> return (Left err)
                Right epr'  -> return (Right (LPNofixRel ann lp esr' epr')) 
-- 
instance SynTraverse0 Application where
  traverseZero e = case e of
    PrefixApp ann  app -> do
        ar <- traverseZero app
        case ar of
            Left err -> return (Left err)
            Right ar'-> return (Right (PrefixApp ann  ar'))
    PostfixApp ann app -> do
        ar <- traverseZero app
        case ar of
            Left err -> return (Left err)
            Right ar'-> return (Right (PostfixApp ann ar'))
    InfixApp ann   app -> do
        ar <- traverseZero app
        case ar of
            Left err -> return (Left err)
            Right ar'-> return (Right (InfixApp ann ar'))
    NofixApp ann   app -> do
        ar <- traverseZero app
        case ar of
            Left err -> return (Left err)
            Right ar'-> return (Right (NofixApp ann ar'))
-- 
instance SynTraverse0 PrefixApp where
  traverseZero e = case e of
    EPPrePrefixApp ann pre expr -> do
        er <- ep2expr expr
        case er of
            Left err    -> return (Left err)
            Right er'   -> return (Right (PrePrefixApp ann pre er'))
    EPLPrefixApp ann l listesss_e eresre_e expr -> do
        er <- ep2expr expr
        esr <- traverseZero listesss_e 
        ersr <- traverseZero eresre_e 
        case er of
            Left err    -> return (Left err)
            Right er'   -> case esr of 
                Left err    -> return (Left err)
                Right esr'  -> case ersr of
                    Left err    -> return (Left err)
                    Right ersr' -> return (Right (LPrefixApp ann l esr' ersr' er')) 
-- 
instance SynTraverse0 PostfixApp where
  traverseZero e = case e of
    EPPostPostfixApp ann expr post -> do
        er <- ep2expr expr
        case er of
            Left err    -> return (Left err)
            Right er'   -> return (Right (PostPostfixApp ann er' post))
    EPELPostfixApp ann expr el listesss_e ersr_e -> do
        er <- ep2expr expr
        esr <- traverseZero listesss_e 
        ersr <- traverseZero ersr_e 
        case er of
            Left err    -> return (Left err)
            Right er'   -> case esr of 
                Left err    -> return (Left err)
                Right esr'  -> case ersr of
                    Left err    -> return (Left err)
                    Right ersr' -> return (Right (ELPostfixApp ann er' el esr' ersr')) 
-- 
instance SynTraverse0 InfixApp where
  traverseZero e = case e of
    EPInInfixApp ann expr1 ii expr2 -> do
        er1 <- ep2expr expr1 
        er2 <- ep2expr expr2 
        case er1 of
            Left err    -> return (Left err)
            Right er1'   -> case er2 of
                Left err    -> return (Left err)
                Right er2'  -> return (Right (InInfixApp ann er1' ii er2')) 
    EPELInfixApp ann expr1 el listesss_e eresre_e expr2 -> do
        er1 <- ep2expr expr1
        esr <- traverseZero listesss_e 
        epr <- traverseZero eresre_e 
        er2 <- ep2expr expr2
        case er1 of
            Left err    -> return (Left err)
            Right er1'   -> case er2 of
                Left err    -> return (Left err)
                Right er2'   ->  case esr of
                    Left err    -> return (Left err)
                    Right esr'  -> case epr of
                        Left err    -> return (Left err)
                        Right epr'  -> return (Right (ELInfixApp ann er1' el esr' epr' er2')) 
--   traverseZeroList [] = []
--   traverseZeroList (x:xs) = (:) (traverseExpr x) (traverseExpr xs)
-- 
instance SynTraverse0 NofixApp where
  traverseZero e = case e of
    EPLNofixApp ann l listesss_e erse_e -> do
        er1 <- traverseZero listesss_e
        er2 <- traverseZero erse_e 
        case er1 of
            Left err    -> return (Left err)
            Right er1'   -> case er2 of
                Left err    -> return (Left err)
                Right er2'  -> return (Right (LNofixApp ann l er1' er2')) 

-- | Transformation from ExprPredicate to Expression.
ep2expr :: ExprPredicate -> OpTableType (Either String Expression)
ep2expr (EPForall anns schtext ep) = do
    m <- traverseZero schtext; case m of
        Left err -> return (Left err)
        Right schtext' -> do 
            n <- ep2exprSchema ep; case n of
                Left err -> return (Left err)
                Right ep' -> return (Right (ForallExpr anns schtext' ep'))
ep2expr (EPExists anns schtext ep) = do 
    m <- traverseZero schtext; case m of
        Left err -> return (Left err)
        Right schtext' -> do 
            n <- ep2exprSchema ep; case n of
                Left err -> return (Left err)
                Right ep' -> return (Right (ExistsExpr anns schtext' ep'))
ep2expr (EPExists1 anns schtext ep) = do
    m <- traverseZero schtext; case m of
        Left err -> return (Left err)
        Right schtext' -> do 
            n <- ep2exprSchema ep; case n of
                Left err -> return (Left err)
                Right ep' -> return (Right (Exists1Expr anns schtext' ep'))
ep2expr (EPEquiv anns ep1 ep2) = do
    m <- ep2exprSchema ep1; case m of
        Left err -> return (Left err)
        Right ep1' -> do 
            n <- ep2exprSchema ep2; case n of
                Left err -> return (Left err)
                Right ep2' -> return (Right (EquivExpr anns ep1' ep2'))
ep2expr (EPImpl anns ep1 ep2) = do
    m <- ep2exprSchema ep1; case m of
        Left err -> return (Left err)
        Right ep1' -> do 
            n <- ep2exprSchema ep2; case n of
                Left err -> return (Left err)
                Right ep2' -> return (Right (ImplExpr anns ep1' ep2'))
ep2expr (EPAnd anns ep1 ep2) = do 
    m <- ep2exprSchema ep1; case m of
        Left err -> return (Left err)
        Right ep1' -> do 
            n <- ep2exprSchema ep2; case n of
                Left err -> return (Left err)
                Right ep2' -> return (Right (AndExpr anns ep1' ep2'))
ep2expr (EPOr anns ep1 ep2) = do 
    m <- ep2exprSchema ep1; case m of
        Left err -> return (Left err)
        Right ep1' -> do 
            n <- ep2exprSchema ep2; case n of
                Left err -> return (Left err)
                Right ep2' -> return (Right (OrExpr anns ep1' ep2'))
ep2expr (EPNeg anns ep) = do
    n <- ep2exprSchema ep; case n of
        Left err -> return (Left err)
        Right ep' -> return (Right (NegExpr anns ep'))
ep2expr (EPLambda anns schtext ep) = do 
    m <- traverseZero schtext; case m of
        Left err -> return (Left err)
        Right schtext' -> do 
            n <- ep2expr ep; case n of
                Left err -> return (Left err)
                Right ep' -> return (Right (LambdaExpr anns schtext' ep'))
ep2expr (EPMu anns schtext ep) = do 
    m <- traverseZero schtext; case m of
        Left err -> return (Left err)
        Right schtext' -> do 
            n <- ep2expr ep; case n of
                Left err -> return (Left err)
                Right ep' -> return (Right (MuExpr anns schtext' ep'))
ep2expr (EPLocalDef anns decls ep) = do
    m <- traverseZero decls; case m of
        Left err -> return (Left err)
        Right decls' -> do 
            n <- ep2expr ep; case n of
                Left err -> return (Left err)
                Right ep' -> return (Right (LocalDefExpr anns decls' ep'))
ep2expr (EPCond anns ep1 ep2 ep3) = do 
    m <- ep2pred ep1; case m of
        Left err -> return (Left err)
        Right ep1' -> do 
            n <- ep2expr ep2; case n of
                Left err -> return (Left err)
                Right ep2' -> do
                    p <- ep2expr ep3; case p of
                        Left err -> return (Left err)
                        Right ep3' -> return (Right (CondExpr anns ep1' ep2' ep3'))
ep2expr (EPComp anns ep1 ep2) = do
    m <- ep2exprSchema ep1; case m of
        Left err -> return (Left err)
        Right ep1' -> do 
            n <- ep2exprSchema ep2; case n of
                Left err -> return (Left err)
                Right ep2' -> return (Right (CompExpr anns ep1' ep2'))
ep2expr (EPPipe anns ep1 ep2) = do  
    m <- ep2exprSchema ep1; case m of
        Left err -> return (Left err)
        Right ep1' -> do 
            n <- ep2exprSchema ep2; case n of
                Left err -> return (Left err)
                Right ep2' -> return (Right (PipeExpr anns ep1' ep2'))
ep2expr (EPHide anns ep dnames) = do 
    m <- traverseZero dnames; case m of
        Left err -> return (Left err)
        Right dnames' -> do 
            n <- ep2exprSchema ep; case n of
                Left err -> return (Left err)
                Right ep' -> return (Right (HideExpr anns ep' dnames'))
ep2expr (EPProj anns ep1 ep2) = do
    m <- ep2exprSchema ep1; case m of
        Left err -> return (Left err)
        Right ep1' -> do 
            n <- ep2exprSchema ep2; case n of
                Left err -> return (Left err)
                Right ep2' -> return (Right (ProjExpr anns ep1' ep2'))
ep2expr (EPPre anns ep) = do
    n <- ep2exprSchema ep; case n of
        Left err -> return (Left err)
        Right ep' -> return (Right (PreExpr anns ep'))
ep2expr (EPProd anns eps) = do
    n <- ep2exprList eps; case n of 
        Left err -> return (Left err)
        Right eps' -> return (Right (ProdExpr anns eps'))
ep2expr (EPPower anns ep) = do
    n <- ep2expr ep; case n of
        Left err -> return (Left err)
        Right ep' -> return (Right (PowerExpr anns ep'))
ep2expr (EPFuncAppl anns app) = do
    n <- traverseZero app; case n of
        Left err -> return (Left err)
        Right app' -> return (Right (FuncApplExpr anns app'))
ep2expr (EPAppl anns ep1 ep2) = do
    m <- ep2expr ep1; case m of
        Left err -> return (Left err)
        Right ep1' -> do 
            n <- ep2expr ep2; case n of
                Left err -> return (Left err)
                Right ep2' -> return (Right (ApplExpr anns ep1' ep2'))
ep2expr (EPDecor anns ep decor) = do 
    n <- ep2exprSchema ep; case n of
        Left err -> return (Left err)
        Right ep' -> return (Right (DecorExpr anns ep' decor))
ep2expr (EPRename anns ep rps) = do 
    m <- traverseZero rps; case m of
        Left err -> return (Left err)
        Right rps' -> do 
            n <- ep2exprSchema ep; case n of
                Left err -> return (Left err)
                Right ep' -> return (Right (RenameExpr anns ep' rps'))
ep2expr (EPBindSel anns ep refname) = do 
    m <- traverseZero refname; case m of
        Left err -> return (Left err)
        Right r' -> do 
            n <- ep2expr ep; case n of
                Left err -> return (Left err)
                Right ep' -> return (Right (BindSelExpr anns ep' r'))
ep2expr (EPTupleSel anns ep num) = do 
    n <- ep2expr ep; case n of
        Left err -> return (Left err)
        Right ep' -> return (Right (TupleSelExpr anns ep' num))
ep2expr (EPTheta anns ep ns) = do 
    n <- ep2exprSchema ep; case n of
        Left err -> return (Left err)
        Right ep' -> return (Right (ThetaExpr anns ep' ns))
ep2expr (EPRef anns refname) = do 
    m <- traverseZero refname; case m of
        Left err -> return (Left err)
        Right r' -> return (Right (RefExpr anns r'))
ep2expr (EPGenRef anns refname eps) = do 
    m <- traverseZero refname; case m of
        Left err -> return (Left err)
        Right r' -> do 
            n <- ep2exprList eps; case n of
                Left err -> return (Left err)
                Right eps' -> return (Right (GenRefExpr anns r' eps'))
ep2expr (EPEmptySet anns) = return (Right (EmptySetExpr anns))
ep2expr (EPSet anns eps) = do 
    n <- ep2exprList eps; case n of 
        Left err -> return (Left err)
        Right eps' -> return (Right (SetExpr anns eps'))
ep2expr (EPSetComp anns schtext ep) = do 
    m <- traverseZero schtext; case m of
        Left err -> return (Left err)
        Right schtext' -> do 
            n <- ep2expr ep; case n of
                Left err -> return (Left err)
                Right ep' -> return (Right (SetCompExpr anns schtext' ep'))
ep2expr (EPCharSetComp anns schtext) = do
    m <- traverseZero schtext; case m of
        Left err -> return (Left err)
        Right schtext' -> return (Right (CharSetCompExpr anns schtext'))
ep2expr (EPSchema anns schtext) = do
    m <- traverseZero schtext; case m of
        Left err -> return (Left err)
        Right schtext' -> return (Right (SchemaExpr anns schtext'))
ep2expr (EPVarCons anns name ep) = do
    n <- ep2expr ep; case n of
        Left err -> return (Left err)
        Right ep' -> return (Right (VarConsExpr anns name ep'))
ep2expr (EPSchemaCons anns ep1 ep2) = do 
    m <- ep2expr ep1; case m of
        Left err -> return (Left err)
        Right ep1' -> do 
            n <- ep2pred ep2; case n of
                Left err -> return (Left err)
                Right ep2' -> return (Right (SchemaConsExpr anns ep1' ep2'))
ep2expr (EPBindExt anns decls) = do 
    m <- traverseZero decls; case m of
        Left err -> return (Left err)
        Right decls' -> return (Right (BindExtExpr anns decls'))
ep2expr (EPTupleExt anns ep eps) = do  
    n <- ep2expr ep; case n of
        Left err -> return (Left err)
        Right ep' -> do  
            n <- ep2exprList eps; case n of 
                Left err -> return (Left err)
                Right eps' -> return (Right (TupleExtExpr anns ep' eps'))
ep2expr (EPCharMu anns schtext) = do  
    m <- traverseZero schtext; case m of
        Left err -> return (Left err)
        Right schtext' -> return (Right (CharMuExpr anns schtext'))
ep2expr (EPNum anns n) = return (Right (NumExpr anns n))
ep2expr (EPRel anns rel) = return (Left (err2Msg (ErrParser (EParseEPNotExpr ("Relation [" ++ (show rel) ++ "] " ++ (showPos anns) ++ " cannot be expression!" )))))
-- ep2expr (EPExprPred anns ep) = return (Left ("Relation [" ++ (show rel) ++ "] " ++ (showPos anns) ++ " cannot be expression!" ))
ep2expr (EPTrue anns) = return (Left (err2Msg (ErrParser (EParseEPNotExpr ("true " ++ (showPos anns) ++ " cannot be expression!" )))))
ep2expr (EPFalse anns) = return (Left (err2Msg (ErrParser (EParseEPNotExpr ("false " ++ (showPos anns) ++ " cannot be expression!" )))))

-- | Transformation from a list of ExprPredicates to a list of Expressions.
ep2exprList :: [ExprPredicate] -> OpTableType (Either String [Expression])
ep2exprList [] = return (Right [])
ep2exprList (x:xs) = do
    n <- ep2expr x; case n of
        Left err -> return (Left err)
        Right x' -> do
            m <- ep2exprList xs; case m of
                Left err -> return (Left err)
                Right xs' -> return (Right (x':xs'))

-- | Transformation from ExprPredicate to Expression and check if the transformed expression is a schema expression.
ep2exprSchema :: ExprPredicate -> OpTableType (Either String Expression)
ep2exprSchema ep = do
    n <- ep2expr ep ; case n of
        Left err -> return (Left err)
        Right ep' -> do
            r <- isSchemaExpr ep' ; case r of
                True -> return (Right ep')
                False -> return (Left (notASchemaErrorExpr ep')) 

-- | Transformation from ExprPredicate to predicate.
ep2pred :: ExprPredicate -> OpTableType (Either String Predicate)
ep2pred (EPForall anns schtext ep) = do
    m <- traverseZero schtext; case m of
        Left err -> return (Left err)
        Right schtext' -> do 
            n <- ep2pred ep; case n of
                Left err -> return (Left err)
                Right ep' -> return (Right (ForallPred anns schtext' ep'))
ep2pred (EPExists anns schtext ep) = do 
    m <- traverseZero schtext; case m of
        Left err -> return (Left err)
        Right schtext' -> do 
            n <- ep2pred ep; case n of
                Left err -> return (Left err)
                Right ep' -> return (Right (ExistsPred anns schtext' ep'))
ep2pred (EPExists1 anns schtext ep) = do
    m <- traverseZero schtext; case m of
        Left err -> return (Left err)
        Right schtext' -> do 
            n <- ep2pred ep; case n of
                Left err -> return (Left err)
                Right ep' -> return (Right (Exists1Pred anns schtext' ep'))
ep2pred (EPEquiv anns ep1 ep2) = do
    m <- ep2pred ep1; case m of
        Left err -> return (Left err)
        Right ep1' -> do 
            n <- ep2pred ep2; case n of
                Left err -> return (Left err)
                Right ep2' -> return (Right (EquivPred anns ep1' ep2'))
ep2pred (EPImpl anns ep1 ep2) = do
    m <- ep2pred ep1; case m of
        Left err -> return (Left err)
        Right ep1' -> do 
            n <- ep2pred ep2; case n of
                Left err -> return (Left err)
                Right ep2' -> return (Right (ImplPred anns ep1' ep2'))
ep2pred (EPAnd anns ep1 ep2) = do 
    m <- ep2pred ep1; case m of
        Left err -> return (Left err)
        Right ep1' -> do 
            n <- ep2pred ep2; case n of
                Left err -> return (Left err)
                Right ep2' -> return (Right (AndPred anns ep1' ep2'))
ep2pred (EPOr anns ep1 ep2) = do 
    m <- ep2pred ep1; case m of
        Left err -> return (Left err)
        Right ep1' -> do 
            n <- ep2pred ep2; case n of
                Left err -> return (Left err)
                Right ep2' -> return (Right (OrPred anns ep1' ep2'))
ep2pred (EPNeg anns ep) = do
    n <- ep2pred ep; case n of
        Left err -> return (Left err)
        Right ep' -> return (Right (NegPred anns ep'))
ep2pred (EPLambda anns schtext ep) = return (Left (err2Msg (ErrParser (EParseEPNotPred ("Lambda expression [" ++ (show (EPLambda anns schtext ep)) ++ "] " ++ (showPos anns) ++ " cannot be predicate!" )))))

-- TODO: can a mu expression be a predicate
ep2pred (EPMu anns schtext ep) = do 
    m <- traverseZero schtext; case m of
        Left err -> return (Left err)
        Right schtext' -> do 
            n <- ep2expr ep; case n of
                Left err -> return (Left err)
                Right ep' -> return (Right (ExprPred anns (MuExpr anns schtext' ep')))
-- TODO: can a local definition expression be a predicate
-- if ep is a schema expression
ep2pred (EPLocalDef anns decls ep) = do
    m <- traverseZero decls; case m of
        Left err -> return (Left err)
        Right decls' -> do 
            n <- ep2expr ep; case n of
                Left err -> return (Left err)
                Right ep' -> return (Right (ExprPred anns (LocalDefExpr anns decls' ep')))
ep2pred (EPCond anns ep1 ep2 ep3) = do 
    m <- ep2pred ep1; case m of
        Left err -> return (Left err)
        Right ep1' -> do 
            n <- ep2expr ep2; case n of
                Left err -> return (Left err)
                Right ep2' -> do
                    p <- ep2expr ep3; case p of
                        Left err -> return (Left err)
                        Right ep3' -> return (Right (ExprPred anns (CondExpr anns ep1' ep2' ep3')))
ep2pred (EPComp anns ep1 ep2) = do
    m <- ep2expr ep1; case m of
        Left err -> return (Left err)
        Right ep1' -> do 
            n <- ep2expr ep2; case n of
                Left err -> return (Left err)
                Right ep2' -> return (Right (ExprPred anns (CompExpr anns ep1' ep2')))
ep2pred (EPPipe anns ep1 ep2) = do  
    m <- ep2expr ep1; case m of
        Left err -> return (Left err)
        Right ep1' -> do 
            n <- ep2expr ep2; case n of
                Left err -> return (Left err)
                Right ep2' -> return (Right (ExprPred anns (PipeExpr anns ep1' ep2')))
ep2pred (EPHide anns ep dnames) = do 
    m <- traverseZero dnames; case m of
        Left err -> return (Left err)
        Right dnames' -> do 
            n <- ep2expr ep; case n of
                Left err -> return (Left err)
                Right ep' -> return (Right (ExprPred anns (HideExpr anns ep' dnames')))
ep2pred (EPProj anns ep1 ep2) = do
    m <- ep2expr ep1; case m of
        Left err -> return (Left err)
        Right ep1' -> do 
            n <- ep2expr ep2; case n of
                Left err -> return (Left err)
                Right ep2' -> return (Right (ExprPred anns (ProjExpr anns ep1' ep2')))
ep2pred (EPPre anns ep) = do
    n <- ep2expr ep; case n of
        Left err -> return (Left err)
        Right ep' -> return (Right (ExprPred anns (PreExpr anns ep')))
ep2pred (EPProd anns eps) = return (Left (err2Msg (ErrParser (EParseEPNotPred ("Cartesian product expression [" ++ (show (EPProd anns eps)) ++ "] " ++ (showPos anns) ++ " cannot be predicate!" )))))
ep2pred (EPPower anns ep) = return (Left (err2Msg (ErrParser (EParseEPNotPred ("Power expression [" ++ (show (EPPower anns ep)) ++ "] " ++ (showPos anns) ++ " cannot be predicate!" )))))
ep2pred (EPFuncAppl anns app) = return (Left (err2Msg (ErrParser (EParseEPNotPred ("Function application expression [" ++ (show (EPFuncAppl anns app)) ++ "] " ++ (showPos anns) ++ " cannot be predicate!" )))))
ep2pred (EPAppl anns ep1 ep2) = return (Left (err2Msg (ErrParser (EParseEPNotPred ("Application expression [" ++ (show (EPAppl anns ep1 ep2)) ++ "] " ++ (showPos anns) ++ " cannot be predicate!" )))))
ep2pred (EPDecor anns ep decor) = do 
    n <- ep2expr ep; case n of
        Left err -> return (Left err)
        Right ep' -> return (Right (ExprPred anns (DecorExpr anns ep' decor)))
ep2pred (EPRename anns ep rps) = do 
    m <- traverseZero rps; case m of
        Left err -> return (Left err)
        Right rps' -> do 
            n <- ep2expr ep; case n of
                Left err -> return (Left err)
                Right ep' -> return (Right (ExprPred anns (RenameExpr anns ep' rps')))
ep2pred (EPBindSel anns ep refname) = return (Left (err2Msg (ErrParser (EParseEPNotPred ("Binding selection expression [" ++ (show (EPBindSel anns ep refname)) ++ "] " ++ (showPos anns) ++ " cannot be predicate!" )))))
--    m <- traverseZero refname; case m of
--        Left err -> return (Left err)
--        Right r' -> do 
--            n <- ep2pred ep; case n of
--                Left err -> return (Left err)
--                Right ep' -> return (Right (BindSelExpr anns ep' r'))
ep2pred (EPTupleSel anns ep n) = return (Left (err2Msg (ErrParser (EParseEPNotPred ("Tuple selection expression [" ++ (show (EPTupleSel anns ep n)) ++ "] " ++ (showPos anns) ++ " cannot be predicate!" )))))
    --n <- ep2pred ep; case n of
    --    Left err -> return (Left err)
    --    Right ep' -> return (Right (TupleSelExpr anns ep' n))
ep2pred (EPTheta anns ep ns) = do 
    n <- ep2expr ep; case n of
        Left err -> return (Left err)
        Right ep' -> return (Right (ExprPred anns (ThetaExpr anns ep' ns)))
--ep2pred (EPRef anns refname) = do 
--    m <- traverseZero refname; case m of
--        Left err -> return (Left err)
--        Right r' -> return (Right (RefExpr anns r'))
--ep2pred (EPGenRef anns refname eps) = do 
--    m <- traverseZero refname; case m of
--        Left err -> return (Left err)
--        Right r' -> do 
--            n <- ep2predList eps; case n of
--                Left err -> return (Left err)
--                Right eps' -> return (Right (GenRefExpr anns r' eps'))
--ep2pred (EPEmptySet anns) = return (Right (EmptySetExpr anns))
--ep2pred (EPSet anns eps) = do 
--    n <- ep2predList eps; case n of 
--        Left err -> return (Left err)
--        Right eps' -> return (Right (SetExpr anns eps'))
--ep2pred (EPSetComp anns schtext ep) = do 
--    m <- traverseZero schtext; case m of
--        Left err -> return (Left err)
--        Right schtext' -> do 
--            n <- ep2pred ep; case n of
--                Left err -> return (Left err)
--                Right ep' -> return (Right (SetCompExpr anns schtext' ep'))
--ep2pred (EPCharSetComp anns schtext) = do
--    m <- traverseZero schtext; case m of
--        Left err -> return (Left err)
--        Right schtext' -> return (Right (CharSetCompExpr anns schtext'))
ep2pred (EPSchema anns schtext) = do
    m <- traverseZero schtext; case m of
        Left err -> return (Left err)
        Right schtext' -> return (Right (ExprPred anns (SchemaExpr anns schtext')))
--ep2pred (EPVarCons anns name ep) = do
--    n <- ep2pred ep; case n of
--        Left err -> return (Left err)
--        Right ep' -> return (Right (VarConsExpr anns name ep'))
--ep2pred (EPSchemaCons anns ep1 ep2) = do 
--    m <- ep2pred ep1; case m of
--        Left err -> return (Left err)
--        Right ep1' -> do 
--            n <- ep2pred ep2; case n of
--                Left err -> return (Left err)
--                Right ep2' -> return (Right (SchemaConsExpr anns ep1' ep2'))
ep2pred (EPBindExt anns decls) = do 
    m <- traverseZero decls; case m of
        Left err -> return (Left err)
        Right decls' -> return (Right (ExprPred anns (BindExtExpr anns decls')))
--ep2pred (EPTupleExt anns ep eps) = do  
--    n <- ep2pred ep; case n of
--        Left err -> return (Left err)
--        Right ep' -> do  
--            n <- ep2predList eps; case n of 
--                Left err -> return (Left err)
--                Right eps' -> return (Right (TupleExtExpr anns ep' eps'))
-- ep2pred (EPCharMu anns schtext) = do  
--     m <- traverseZero schtext; case m of
--         Left err -> return (Left err)
--         Right schtext' -> return (Right (CharMuExpr anns schtext'))
-- ep2pred (EPNum anns n) = return (NumExpr anns n)
ep2pred (EPRel anns rel) = do
    n <- traverseZero rel; case n of
        Left err -> return (Left err) 
        Right rel' -> return (Right (RelPred anns rel')) 
-- ep2pred (EPExprPred anns ep) = return (Left ("Relation [" ++ (show rel) ++ "] " ++ (showPos anns) ++ " cannot be expression!" ))
ep2pred (EPTrue anns) = return (Right (TruePred anns)) 
ep2pred (EPFalse anns) = return (Right (FalsePred anns)) 
ep2pred ep = do
    n <- ep2expr ep; case n of 
        Left err -> return (Left err)
        Right ep' -> do
            b <- isSchemaExpr ep'; case b of
                True -> return (Right (ExprPred (getAnnTerm ep') (ep')))
                False -> return (Left (err2Msg (ErrParser (EParseEPNotPred ("Expression [" ++ (show ep) ++ "] " ++ (showPos (getAnnTerm ep')) ++ " cannot be predicate!" )))))  

-- | Transform from a list of ExprPredicate to a list of Predicate.
ep2predList :: [ExprPredicate] -> OpTableType (Either String [Predicate] )
ep2predList [] = return (Right [])
ep2predList (x:xs) = do
    n <- ep2pred x; case n of
        Left err -> return (Left err)
        Right x' -> do
            m <- ep2predList xs; case m of
                Left err -> return (Left err)
                Right xs' -> return (Right (x':xs'))


-- | Check if is a name is a schema.
isASchema :: String -> OpTableType Bool
isASchema name = do
        m <- getSchNameTable
        case Map.lookup (realName name) m of 
            Just True -> return True 
            _         -> return False 
  -- remove prefixing Xi or Delta
  where realName ('\x0394': xs) = xs
        realName ('\x039E': xs) = xs
        realName s = s

-- | Check if both expressions are schema expressions.
andSchemaExprTrue :: Expression -> Expression -> OpTableType Bool
andSchemaExprTrue e1 e2 = do r1 <- isSchemaExpr e1
                             r2 <- isSchemaExpr e2
                             case r1 of
                                True -> case r2 of
                                        True -> return True 
                                        False -> return False
                                False -> return False

-- | Check if an expression is a schema expression.
isSchemaExpr :: Expression -> OpTableType Bool
isSchemaExpr (ForallExpr _ _ e) = ( isSchemaExpr e )
isSchemaExpr (ExistsExpr _ _ e) = ( isSchemaExpr e )
isSchemaExpr (Exists1Expr _ _ e) = ( isSchemaExpr e ) 
isSchemaExpr (EquivExpr _ e1 e2) = andSchemaExprTrue e1 e2 
isSchemaExpr (ImplExpr _ e1 e2) = andSchemaExprTrue e1 e2
isSchemaExpr (AndExpr _ e1 e2) = andSchemaExprTrue e1 e2
isSchemaExpr (OrExpr _ e1 e2) = andSchemaExprTrue e1 e2
isSchemaExpr (NegExpr _ e) = ( isSchemaExpr e)
isSchemaExpr (MuExpr _ _ e) = ( isSchemaExpr e)
isSchemaExpr (LocalDefExpr _ _ e) = isSchemaExpr e
isSchemaExpr (CondExpr _ _ e1 e2 ) = andSchemaExprTrue e1 e2
isSchemaExpr (CompExpr _ e1 e2  ) = andSchemaExprTrue e1 e2
isSchemaExpr (PipeExpr _ e1 e2  ) = andSchemaExprTrue e1 e2
isSchemaExpr (HideExpr _ e _) = ( isSchemaExpr e )
isSchemaExpr (ProjExpr _ e1 e2  ) = andSchemaExprTrue e1 e2
isSchemaExpr (PreExpr _ e) = ( isSchemaExpr e)
isSchemaExpr (DecorExpr _ e _) = ( isSchemaExpr e )
isSchemaExpr (RenameExpr _ e _) = ( isSchemaExpr e ) 
isSchemaExpr (ThetaExpr _ e _) = ( isSchemaExpr e )
isSchemaExpr (RefExpr _ (RName _ r)) = isASchema r 
isSchemaExpr (GenRefExpr _ (RName _ r) _) = isASchema r 
isSchemaExpr (CharSetCompExpr _ _) = return ( True) 
isSchemaExpr (SchemaExpr _ _) = return ( True)
isSchemaExpr (BindExtExpr _ _) = return ( True)  
isSchemaExpr _ = return ( False )

-- | Return an error message to indicate it is not a schema expression error.
notASchemaErrorExpr :: Expression -> String
notASchemaErrorExpr e = (err2Msg (ErrParser (EParseNotASchema ("Expression [" ++ printTree e ++ ", " ++ show e ++ "] " ++ (showPos (getAnnTerm e)) ++ " is not a schema expression!"))))
