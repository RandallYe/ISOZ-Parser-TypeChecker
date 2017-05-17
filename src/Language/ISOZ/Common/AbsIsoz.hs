{- |
Module      : Language.ISOZ.Common.AbsIsoz
Description : 
Copyright   : (C) Kangfeng Ye, 2017
License     : BSD3
Maintainer  : ye.randall@gmail.com 
Stability   : experimental

Concrete and annotated syntax types and constructors (see clause 8 and clause 10 of ISO Z Standard)

-}
module Language.ISOZ.Common.AbsIsoz where

import Language.ISOZ.Common.Ann
import Language.ISOZ.ZChar

-- | NAME is string
type NAME = String

-- | Not used 
data Name = Name [Ann] String
  deriving (Eq, Ord, Show, Read)

-- | Specification category 
data Specification = SpecSect [Ann] [Section]   -- ^ sectioned specification
                   | SpecAnony [Ann] [Paragraph]-- ^ anonymous specification  
                   | SpecEmpty [Ann]            -- ^ empty specification
                   | SpecSectA [Ann] [Section]  -- ^ transformed sectioned specification 
  deriving (Eq, Ord, Show, Read)

-- | Section
data Section = Section [Ann] String String SectionParents String [Paragraph]    -- ^ Section for parsing, including Base Section and Inheriting Section  . It is used in *.y and later it will be transformed to BaseSection or InheritingSection by synTraverse1Tree 
             | BaseSection [Ann] String String String [Paragraph]               -- ^ Base section without parents
             | InheritingSection [Ann] String String [String] String [Paragraph]-- ^ Inheriting section before syntactical transformation (synTraverse2Tree)
             | InheritingSectionA [Ann] NAME [NAME] [Paragraph]                 -- ^ Inheriting section after syntactical transformation (synTraverse2Tree)
  deriving (Eq, Ord, Show, Read)

data SectionParents = BaseSection2 [Ann]                                        -- ^ Used in parsing (.y) and it will be transformed to BaseSection later
                    | InheritingSection2 [Ann] [String]                         -- ^ Used in parsing (.y) and it will be transformed to InheritingSection later
  deriving (Eq, Ord, Show, Read)

-- | Paragraphs
data Paragraph = AxPara [Ann] String [ParagraphBody] String                     -- ^ Paragraphs starting with ZED
               | AxParaA [Ann] String [Paragraph] String                        -- ^ Temporary use after synTraverse1Tree which transforms a list of paragraph bodies to a list of paragraphs
               | GivenPara [Ann] [String]                                       -- ^ Given paragraph 
               | GivenParaA [Ann] [NAME]                                        -- ^ Transformed given paragraph
               | AxdefPara [Ann] String SchemaText String                       -- ^ Axiomatic definition paragraph 
               | SchdefPara [Ann] String String SchemaText String               -- ^ Schema definition paragraph
               | GenAxdefPara [Ann] String Formals SchemaText String            -- ^ Generic axiomatic definition paragraph
               | GenSchdefPara [Ann] String Formals String SchemaText String    -- ^ Generic schema definition paragraph
               | GenConjecturePara [Ann] String Formals Predicate               -- ^ Generic conjecture paragraph
               | EPGenConjecturePara [Ann] String Formals ExprPredicate         -- ^ Generic conjecture paragraph in the parsing stage where Predicate is parsed ExprPredicate
               | GenConjectureParaA [Ann] String [NAME] Predicate               -- ^ Transformed Generic conjecture paragraph by synTraverse2Tree
               | HDefPara [Ann] DeclName Expression                             -- ^ Horizontal definition paragraph
--               | GenHDefPara [Ann] String Formals Expression
-- In ISO Z, it is NAME. However, it will not make "< ,, >" be parsed correctly
-- So DeclName would be right here
               | GenHDefPara [Ann] DeclName Formals Expression                  -- ^ Generic horizontal definition paragraph
               | GenOpDefPara [Ann] GenName Expression                          -- ^ Generic operator definition paragraph
               | FreetypePara [Ann] [Freetype]                                  -- ^ Free type paragraph 
               | FreetypeParaA [Ann] [Freetype]                                 -- ^ Transformed free type paragraph by synTraverse2Tree   
               | ConjecturePara [Ann] String Predicate                          -- ^ Conjecture paragraph 
               | EPConjecturePara [Ann] String ExprPredicate                    -- ^ Conjecture paragraph in the parsing stage where Predicate is parsed ExprPredicate 
               | ConjectureParaA [Ann] String Predicate                         -- ^ Transformed conjecture paragraph by synTraverse2Tree   
               | OperatorPara [Ann] OperatorTemplate                            -- ^ operator template paragraph 
               | ChannelPara [Ann] [ChannelDecl]                                -- ^ Circus channel paragraph
               | ChannelFromPara [Ann] [ChannelFromDecl]                        -- ^ Channel from paragraph 
               | ChannelSetPara [Ann] [ChannelSetDecl]                          -- ^ Channel se paragraphs
-- annotated syntax
               | AxdefParaA [Ann] Expression                                    -- Transformed axiomatic definition paragraphs by synTraverse2Tree
               | GenAxdefParaA [Ann] [NAME] Expression                          -- Transformed generic axiomatic definition paragraphs by synTraverse2Tree
  deriving (Eq, Ord, Show, Read)

-- | ParagraphBody: just used in the parsing stage to minimise more shift/reduce conflicts
data ParagraphBody = GivenPara2 [Ann] [String]
                   | HDefPara2 [Ann] DeclName Expression
                   | EPHDefPara2 [Ann] DeclName ExprPredicate
--                   | GenHDefPara2 [Ann] String Formals Expression
-- In ISO Z, it is NAME. However, it will not make "< ,, >" be parsed correctly
-- So DeclName would be right here
                   | GenHDefPara2 [Ann] DeclName Formals Expression
                   | EPGenHDefPara2 [Ann] DeclName Formals ExprPredicate
                   | GenOpDefPara2 [Ann] GenName Expression
                   | EPGenOpDefPara2 [Ann] GenName ExprPredicate
                   | FreetypePara2 [Ann] [Freetype] 
--                   | ConjecturePara2 [Ann] Predicate 
                   | OperatorPara2 [Ann] OperatorTemplate 
                   | ChannelPara2 [Ann] [ChannelDecl]
                   | ChannelFromPara2 [Ann] [ChannelFromDecl]
                   | ChannelSetPara2 [Ann] [ChannelSetDecl]
                   | ProcDeclPara2 [Ann] NAME ProcDef
                   | GenProcDeclPara2 [Ann] [String] NAME ProcDef
  deriving (Eq, Ord, Show, Read)

-- | Rename pairf from oldname to newname
data RenamePair = RenamePair [Ann] DeclName DeclName 
                | RenamePairA [Ann] NAME NAME 
  deriving (Eq, Ord, Show, Read)

-- | A merge of Expression and Predicate. It will be transformed to Predicate and Expression shortly depending on the context
data ExprPredicate = EPForall [Ann] SchemaText ExprPredicate -- confused Expression or Predicate 
                   | EPExists [Ann] SchemaText ExprPredicate 
                   | EPExists1 [Ann] SchemaText ExprPredicate 
                   | EPEquiv [Ann] ExprPredicate ExprPredicate 
                   | EPImpl [Ann] ExprPredicate ExprPredicate 
                   | EPAnd [Ann] ExprPredicate ExprPredicate 
                   | EPOr [Ann] ExprPredicate ExprPredicate 
                   | EPNeg [Ann] ExprPredicate
                   | EPLambda [Ann] SchemaText ExprPredicate 
                   | EPMu [Ann] SchemaText ExprPredicate 
                   | EPLocalDef [Ann] [Declaration] ExprPredicate 
                   | EPCond [Ann] ExprPredicate ExprPredicate ExprPredicate 
                   | EPComp [Ann] ExprPredicate ExprPredicate 
                   | EPPipe [Ann] ExprPredicate ExprPredicate 
                   | EPHide [Ann] ExprPredicate [DeclName] 
                   | EPProj [Ann] ExprPredicate ExprPredicate
                   | EPPre [Ann] ExprPredicate
                   | EPProd [Ann] [ExprPredicate]
                   | EPPower [Ann] ExprPredicate
                   | EPFuncAppl [Ann] Application 
                   | EPAppl [Ann] ExprPredicate ExprPredicate
                   | EPDecor [Ann] ExprPredicate String 
                   | EPRename [Ann] ExprPredicate [RenamePair] 
                   | EPBindSel [Ann] ExprPredicate RefName 
                   | EPTupleSel [Ann] ExprPredicate String 
                   | EPTheta [Ann] ExprPredicate [String] 
                   | EPRef [Ann] RefName
                   | EPGenRef [Ann] RefName [ExprPredicate]
                   | EPEmptySet [Ann]
                   | EPSet [Ann] [ExprPredicate]
                   | EPSetComp [Ann] SchemaText ExprPredicate 
                   | EPCharSetComp [Ann] SchemaText
                   | EPSchema [Ann] SchemaText
                   | EPVarCons [Ann] NAME ExprPredicate
                   | EPSchemaCons [Ann] ExprPredicate ExprPredicate 
                   | EPBindExt [Ann] [Declaration]  
                   | EPTupleExt [Ann] ExprPredicate [ExprPredicate]  
                   | EPCharMu [Ann] SchemaText  
                   | EPNum [Ann] String
                   | EPRel [Ann] Relation
                   | EPExprPred [Ann] ExprPredicate
                   | EPTrue [Ann]
                   | EPFalse [Ann]
  deriving (Eq, Ord, Show, Read)

-- | Expression
data Expression = NumExpr [Ann] String 
                | ForallExpr [Ann] SchemaText Expression
                | ForallExprA [Ann] Expression Expression
                | ExistsExpr [Ann] SchemaText Expression
                | Exists1Expr [Ann] SchemaText Expression
                | Exists1ExprA [Ann] Expression Expression
                | LambdaExpr [Ann] SchemaText Expression
                | MuExpr [Ann] SchemaText Expression
                | MuExprA [Ann] Expression Expression
                | LocalDefExpr [Ann] [Declaration] Expression
                | EquivExpr [Ann] Expression Expression
                | ImplExpr [Ann] Expression Expression
                | AndExpr [Ann] Expression Expression
                | OrExpr [Ann] Expression Expression
                | NegExpr [Ann] Expression
                | CondExpr [Ann] Predicate Expression Expression  
                | CompExpr [Ann] Expression Expression  
                | PipeExpr [Ann] Expression Expression  
                | HideExpr [Ann] Expression [DeclName] 
                | HideExprA [Ann] Expression [NAME] 
                | ProjExpr [Ann] Expression Expression  
                | PreExpr [Ann] Expression
                | ProdExpr [Ann] [Expression]
                | PowerExpr [Ann] Expression
                | FuncApplExpr [Ann] Application 
                | ApplExpr [Ann] Expression Expression 
                | DecorExpr [Ann] Expression String 
                | RenameExpr [Ann] Expression [RenamePair] 
                | BindSelExpr [Ann] Expression RefName 
                | BindSelExprA [Ann] Expression NAME 
                | TupleSelExpr [Ann] Expression String 
                | ThetaExpr [Ann] Expression [String] 
--                | RefExprB [Ann] RefName RefExprBody  -- (body of RefExpr: empty)
                | RefExpr [Ann] RefName
                | RefExprA [Ann] NAME 
                | GenRefExpr [Ann] RefName [Expression]
                | GenRefExprA [Ann] NAME [Expression]
                | EmptySetExpr [Ann]
                | SetExpr [Ann] [Expression]
                | SetCompExpr [Ann] SchemaText Expression 
                | SetCompExprA [Ann] Expression Expression 
                | CharSetCompExpr [Ann] SchemaText
                | SchemaExpr [Ann] SchemaText
                | VarConsExpr [Ann] NAME Expression  
                | SchemaConsExpr [Ann] Expression Predicate 
                | BindExtExpr [Ann] [Declaration]  
                | BindExtExprA [Ann] [(NAME, Expression)]  
                | TupleExtExpr [Ann] Expression [Expression]  
                | CharMuExpr [Ann] SchemaText  
                | NullExpr [Ann]   -- an invalid NULL expression
  deriving (Eq, Ord, Show, Read)

-- | DeclName 
data DeclName = DName [Ann] String
              | OpName [Ann] OpName 
  deriving (Eq, Ord, Show, Read)

-- | Schema text 
data SchemaText = SchemaTextEmpty [Ann]                     -- ^  Empty Schema text
                | SchemaTextDecl [Ann] DeclPart
                | SchemaTextPred [Ann] Predicate 
                | EPSchemaTextPred [Ann] ExprPredicate 
                | SchemaText [Ann] DeclPart Predicate
                | EPSchemaText [Ann] DeclPart ExprPredicate
  deriving (Eq, Ord, Show, Read)

-- | DeclPart
data DeclPart = DeclPart [Ann] [Declaration] 
  deriving (Eq, Ord, Show, Read)

-- | Declaration
data Declaration = Declaration [Ann] [DeclName] Expression  
                 | EPDeclaration [Ann] [DeclName] ExprPredicate
                 | AbbrDecl [Ann] DeclName Expression  
                 | EPAbbrDecl [Ann] DeclName ExprPredicate  
                 | ExprDecl [Ann] Expression
                 | EPExprDecl [Ann] ExprPredicate
                 | Declaration1 [Ann] String Decl1 
                 | Declaration2 [Ann] OpName Decl1 
  deriving (Eq, Ord, Show, Read)

-- | Decl1
data Decl1 = BasicDecl1 [Ann] [DeclName] Expression  
           | EPBasicDecl1 [Ann] [DeclName] ExprPredicate
           | AbbrDecl1 [Ann] Expression  
           | EPAbbrDecl1 [Ann] ExprPredicate  
           | ExprDecl1 [Ann] 
  deriving (Eq, Ord, Show, Read)

-- ^ Predicate
data Predicate = AndPred [Ann] Predicate Predicate
               | ForallPred [Ann] SchemaText Predicate
               | ForallPredA [Ann] Expression Predicate
               | ExistsPred [Ann] SchemaText Predicate
               | Exists1Pred [Ann] SchemaText Predicate
               | Exists1PredA [Ann] Expression Predicate
               | EquivPred [Ann] Predicate Predicate
               | ImplPred [Ann] Predicate Predicate
               | OrPred [Ann] Predicate Predicate
               | NegPred [Ann] Predicate
               | RelPred [Ann] Relation
               | ExprPred [Ann] Expression 
               | TruePred [Ann]
               | FalsePred [Ann]
-- annotated syntax
               | MemPred [Ann] Expression Expression
--               | PPredicate Predicate
  deriving (Eq, Ord, Show, Read)

data Formals = Formals [Ann] [String] 
  deriving (Eq, Ord, Show, Read)

data Freetype = Freetype [Ann] NAME [Branch]
  deriving (Eq, Ord, Show, Read)

data Branch = ConstantBranch [Ann] DeclName 
            | ConstructorBranch [Ann] DeclName Expression
            | EPConstructorBranch [Ann] DeclName ExprPredicate
            | ConstantBranchA [Ann] NAME 
            | ConstructorBranchA [Ann] NAME Expression
  deriving (Eq, Ord, Show, Read)

data OperatorTemplate = RelationTemplate [Ann] Template
                      | FunctionTemplate [Ann] CategoryTemplate  
                      | GenericTemplate [Ann] CategoryTemplate  
  deriving (Eq, Ord, Show, Read)

data CategoryTemplate = PrefixCatTemplate [Ann] PrefixTemplate 
                      | PostfixCatTemplate [Ann] PostfixTemplate 
                      | InfixCatTemplate [Ann] Prec Assoc InfixTemplate 
                      | NofixCatTemplate [Ann] NofixTemplate
  deriving (Eq, Ord, Show, Read)

data Prec = Prec [Ann] String
  deriving (Eq, Ord, Show, Read)

data Assoc = LeftAssoc [Ann] | RightAssoc [Ann]
  deriving (Eq, Ord, Show, Read)

data Template = TPrefixTemplate [Ann] PrefixTemplate
              | TPostfixTemplate [Ann] PostfixTemplate 
              | TInfixTemplate [Ann] InfixTemplate 
              | TNofixTemplate [Ann] NofixTemplate
  deriving (Eq, Ord, Show, Read)

data PrefixTemplate = PrefixTemplate [Ann] PrefixName
                    | PowerPrefixTemplate [Ann]
  deriving (Eq, Ord, Show, Read)

data PostfixTemplate = PostfixTemplate [Ann] PostfixName
  deriving (Eq, Ord, Show, Read)

data InfixTemplate = InfixTemplate [Ann] InfixName 
  deriving (Eq, Ord, Show, Read)

data NofixTemplate = NofixTemplate [Ann] NofixName
  deriving (Eq, Ord, Show, Read)

data RefName = RName [Ann] String
             | OpRName [Ann] OpName 
  deriving (Eq, Ord, Show, Read)

data OpName = PrefixOpName [Ann] PrefixName
            | PostfixOpName [Ann] PostfixName  
            | InfixOpName [Ann] InfixName  
            | NofixOpName [Ann] NofixName  
  deriving (Eq, Ord, Show, Read)

data PrefixName = PrePrefixName [Ann] String
                | PrePPrefixName [Ann] String
                | LPrefixName [Ann] String [ESSS] ERESRE
                | LPPrefixName [Ann] String [ESSS] EREPSREP
  deriving (Eq, Ord, Show, Read)

data PostfixName = PostPostfixName [Ann] String 
                 | PostPPostfixName [Ann] String
                 | ELPostfixName [Ann] String [ESSS] ERSR
                 | ELPPostfixName [Ann] String [ESSS] ERPSRP
  deriving (Eq, Ord, Show, Read)

data InfixName = InInfixName [Ann] String
               | InPInfixName [Ann] String
               | ELInfixName [Ann] String [ESSS] ERESRE
               | ELPInfixName [Ann] String [ESSS] EREPSREP
  deriving (Eq, Ord, Show, Read)

data NofixName = LNofixName [Ann] String [ESSS] ERSR
               | LPNofixName [Ann] String [ESSS] ERPSRP
  deriving (Eq, Ord, Show, Read)

-- | ES or SS operator name
--
-- For example, "function  1 leftassoc (_ el _ es1 _ es2 _ ere _)" contains 
--
-- - ES ann es1 
-- - ES ann es2
--
data ESSS = ES [Ann] String
          | SS [Ann] String
  deriving (Eq, Ord, Show, Read)

-- | ES or SS generic name
--
-- For example, "(X el Y es1 Z es2 U ere V)" contains 
--
-- - ES_N ann Y es1 
-- - ES_N ann Z es2
--
data ESSS_N = ES_N [Ann] String String
            | SS_N [Ann] String String
  deriving (Eq, Ord, Show, Read)

-- | ES or SS expression 
--
-- For example, "(e1 el e2 es1 e3 es2 e4 ere e5)" contains 
--
-- - ES_E ann e2 es1 
-- - ES_E ann e3 es2
--
data ESSS_E = ES_E [Ann] Expression String
            | EPES_E [Ann] ExprPredicate String
            | SS_E [Ann] [Expression] String
            | EPSS_E [Ann] [ExprPredicate] String
  deriving (Eq, Ord, Show, Read)

data ERESRE = ERE [Ann] String
            | SRE [Ann] String
  deriving (Eq, Ord, Show, Read)

data ERESRE_N = ERE_N [Ann] String String
              | SRE_N [Ann] String String
  deriving (Eq, Ord, Show, Read)

data ERESRE_E = ERE_E [Ann] Expression String
              | EPERE_E [Ann] ExprPredicate String
              | SRE_E [Ann] [Expression] String
              | EPSRE_E [Ann] [ExprPredicate] String
  deriving (Eq, Ord, Show, Read)

data EREPSREP = EREP [Ann] String
              | SREP [Ann] String
  deriving (Eq, Ord, Show, Read)

data EREPSREP_E = EREP_E [Ann] Expression String
                | EPEREP_E [Ann] ExprPredicate String
                | SREP_E [Ann] [Expression] String
                | EPSREP_E [Ann] [ExprPredicate] String
  deriving (Eq, Ord, Show, Read)

data ERSR = ER [Ann] String
          | SR [Ann] String
  deriving (Eq, Ord, Show, Read)

data ERSR_N = ER_N [Ann] String String
            | SR_N [Ann] String String
  deriving (Eq, Ord, Show, Read)

data ERSR_E = ER_E [Ann] Expression String
            | EPER_E [Ann] ExprPredicate String
            | SR_E [Ann] [Expression] String
            | EPSR_E [Ann] [ExprPredicate] String
  deriving (Eq, Ord, Show, Read)

data ERPSRP = ERP [Ann] String
            | SRP [Ann] String
  deriving (Eq, Ord, Show, Read)

data ERPSRP_E = ERP_E [Ann] Expression String
              | EPERP_E [Ann] ExprPredicate String
              | SRP_E [Ann] [Expression] String
              | EPSRP_E [Ann] [ExprPredicate] String
  deriving (Eq, Ord, Show, Read)

data GenName = PrefixGenName [Ann] PrefixGenName 
             | PostfixGenName [Ann] PostfixGenName 
             | InfixGenName [Ann] InfixGenName 
             | NofixGenName [Ann] NofixGenName 
  deriving (Eq, Ord, Show, Read)

data PrefixGenName = PrePrefixGenName [Ann] String String 
                   | LPrefixGenName [Ann] String [ESSS_N] ERESRE_N String
  deriving (Eq, Ord, Show, Read)

data PostfixGenName = PostPostfixGenName [Ann] String String 
                    | ELPostfixGenName [Ann] String String [ESSS_N] ERSR_N
  deriving (Eq, Ord, Show, Read)

data InfixGenName = InInfixGenName [Ann] String String String
                  | ELInfixGenName [Ann] String String [ESSS_N] ERESRE_N String
  deriving (Eq, Ord, Show, Read)

data NofixGenName = LNofixGenName [Ann] String [ESSS_N] ERSR_N 
  deriving (Eq, Ord, Show, Read)

data Relation = PrefixRel [Ann] PrefixRel 
              | PostfixRel [Ann] PostfixRel 
              | InfixRel [Ann] InfixRel 
              | NofixRel [Ann] NofixRel 
  deriving (Eq, Ord, Show, Read)

data PrefixRel = PrePPrefixRel [Ann] String Expression
               | EPPrePPrefixRel [Ann] String ExprPredicate
               | LPPrefixRel [Ann] String [ESSS_E] EREPSREP_E Expression 
               | EPLPPrefixRel [Ann] String [ESSS_E] EREPSREP_E ExprPredicate 
  deriving (Eq, Ord, Show, Read)

data PostfixRel = PostPPostfixRel [Ann] Expression String 
                | EPPostPPostfixRel [Ann] ExprPredicate String 
                | ELPPostfixRel [Ann] Expression String [ESSS_E] ERPSRP_E
                | EPELPPostfixRel [Ann] ExprPredicate String [ESSS_E] ERPSRP_E
  deriving (Eq, Ord, Show, Read)

data InfixRel = InInfixRel [Ann] Expression [IP_E] 
              | EPInInfixRel [Ann] ExprPredicate [IP_E] 
              | ELPInfixRel [Ann] Expression String [ESSS_E] EREPSREP_E Expression 
              | EPELPInfixRel [Ann] ExprPredicate String [ESSS_E] EREPSREP_E ExprPredicate
  deriving (Eq, Ord, Show, Read)

data NofixRel = LPNofixRel [Ann] String [ESSS_E] ERPSRP_E 
  deriving (Eq, Ord, Show, Read)

data IP_E = IP_E [Ann] String Expression 
          | EPIP_E [Ann] String ExprPredicate 
  deriving (Eq, Ord, Show, Read)

data Application = PrefixApp [Ann] PrefixApp 
                 | PostfixApp [Ann] PostfixApp 
                 | InfixApp [Ann] InfixApp 
                 | NofixApp [Ann] NofixApp 
  deriving (Eq, Ord, Show, Read)

data PrefixApp = PrePrefixApp [Ann] String Expression
               | EPPrePrefixApp [Ann] String ExprPredicate
               | LPrefixApp [Ann] String [ESSS_E] ERESRE_E Expression 
               | EPLPrefixApp [Ann] String [ESSS_E] ERESRE_E ExprPredicate
  deriving (Eq, Ord, Show, Read)

data PostfixApp = PostPostfixApp [Ann] Expression String 
                | EPPostPostfixApp [Ann] ExprPredicate String 
                | ELPostfixApp [Ann] Expression String [ESSS_E] ERSR_E
                | EPELPostfixApp [Ann] ExprPredicate String [ESSS_E] ERSR_E
  deriving (Eq, Ord, Show, Read)

data InfixApp = InInfixApp [Ann] Expression String Expression 
              | EPInInfixApp [Ann] ExprPredicate String ExprPredicate
              | ELInfixApp [Ann] Expression String [ESSS_E] ERESRE_E Expression 
              | EPELInfixApp [Ann] ExprPredicate String [ESSS_E] ERESRE_E ExprPredicate 
  deriving (Eq, Ord, Show, Read)

data NofixApp = LNofixApp [Ann] String [ESSS_E] ERSR_E 
              | EPLNofixApp [Ann] String [ESSS_E] ERSR_E 
  deriving (Eq, Ord, Show, Read)

data ChannelDecl = ChannelDecl [Ann] SimpleCDecl 
  deriving (Eq, Ord, Show, Read)

data SimpleCDecl = SyncSimpleCDecl [Ann] [String] 
                 | TypedSimpleCDecl [Ann] [String] Expression
                 | GenericTypedSimpleCDecl [Ann] Formals [String] Expression
--                 | SchemaSimpleCDecl Expression
                 | SimpleCDecl2 [Ann] [String] SimpleCDecl2
  deriving (Eq, Ord, Show, Read)

data SimpleCDecl2 = SyncSimpleCDecl2 [Ann]
                  | TypedSimpleCDecl2 [Ann] Expression
  deriving (Eq, Ord, Show, Read)

data ChannelFromDecl = ChannelFromDecl [Ann] FromCDecl
  deriving (Eq, Ord, Show, Read)

data FromCDecl = RefFromCDecl [Ann] Expression 
               | RefDecorFromCDecl [Ann] Expression  
  deriving (Eq, Ord, Show, Read)

data ChannelSetDecl = ChannelSetDecl [Ann] NAME CSExp 
  deriving (Eq, Ord, Show, Read)

-- {||} | {| N+ |} | N | CSExp∪CSExp |  CSExp∩CSExp | CSExp \ CSExp
data CSExp = CSExpEmpty [Ann]  
           | CSExpExt [Ann] [Expression]    -- A list of references to channels
           | CSExpRef [Ann] Expression      -- RefExpr
           | CSExpUnion [Ann] CSExp CSExp 
           | CSExpInter [Ann] CSExp CSExp 
           | CSExpDiff [Ann] CSExp CSExp 
  deriving (Eq, Ord, Show, Read)

data ProcDef = ParamProc [Ann] ProcDef 
             | IndexedProc [Ann] ProcDef
--             | Proc [Ann] Proc
  deriving (Eq, Ord, Show, Read)

getPrec :: CategoryTemplate -> String
getPrec (PrefixCatTemplate _ t) = "" 
getPrec (PostfixCatTemplate _ t) = "" 
getPrec (InfixCatTemplate _ (Prec _ prec) _ t) = prec 
getPrec (NofixCatTemplate _ t) = "" 

getAssoc :: CategoryTemplate -> String
getAssoc (PrefixCatTemplate _ t) = "" 
getAssoc (PostfixCatTemplate _ t) = "" 
getAssoc (InfixCatTemplate _ _ (LeftAssoc _) t) = "leftassoc"
getAssoc (InfixCatTemplate _ _ (RightAssoc _) t) = "rightassoc"
getAssoc (NofixCatTemplate _ t) = "" 

{-
 - Get operator name from a category template
 -}
getOpNameFromCatTemplate :: CategoryTemplate -> String
getOpNameFromCatTemplate (PrefixCatTemplate _ t) = getOpNameFromPrefixTemplate t
getOpNameFromCatTemplate (PostfixCatTemplate _ t) = getOpNameFromPostfixTemplate t
getOpNameFromCatTemplate (InfixCatTemplate _ _ _ t) = getOpNameFromInfixTemplate t
getOpNameFromCatTemplate (NofixCatTemplate _ t) = getOpNameFromNofixTemplate t

{-
 - Get operator name from a template
 -}
getOpNameFromTemplate :: Template -> String
getOpNameFromTemplate (TPrefixTemplate _ (PrefixTemplate _ (PrePrefixName _ name))) = name
getOpNameFromTemplate (TPrefixTemplate _ (PrefixTemplate _ (PrePPrefixName _ name))) = name
getOpNameFromTemplate (TPrefixTemplate _ (PrefixTemplate _ (LPrefixName _ name _ _))) = name
getOpNameFromTemplate (TPrefixTemplate _ (PrefixTemplate _ (LPPrefixName _ name _ _))) = name
getOpNameFromTemplate (TPrefixTemplate _ (PowerPrefixTemplate _ )) = zChar_power
getOpNameFromTemplate (TPostfixTemplate _ (PostfixTemplate _ (PostPostfixName _ name))) = name
getOpNameFromTemplate (TPostfixTemplate _ (PostfixTemplate _ (PostPPostfixName _ name))) = name
getOpNameFromTemplate (TPostfixTemplate _ (PostfixTemplate _ (ELPostfixName _ name _ _))) = name
getOpNameFromTemplate (TPostfixTemplate _ (PostfixTemplate _ (ELPPostfixName _ name _ _))) = name
getOpNameFromTemplate (TInfixTemplate _ (InfixTemplate _ (InInfixName _ name))) = name
getOpNameFromTemplate (TInfixTemplate _ (InfixTemplate _ (InPInfixName _ name))) = name
getOpNameFromTemplate (TInfixTemplate _ (InfixTemplate _ (ELInfixName _ name _ _))) = name
getOpNameFromTemplate (TInfixTemplate _ (InfixTemplate _ (ELPInfixName _ name _ _))) = name
getOpNameFromTemplate (TNofixTemplate _ (NofixTemplate _ (LNofixName _ name _ _))) = name
getOpNameFromTemplate (TNofixTemplate _ (NofixTemplate _ (LPNofixName _ name _ _))) = name

getOpNameFromPrefixTemplate :: PrefixTemplate -> String
getOpNameFromPrefixTemplate (PrefixTemplate _ (PrePrefixName _ name)) = name
getOpNameFromPrefixTemplate (PrefixTemplate _ (PrePPrefixName _ name)) = name
getOpNameFromPrefixTemplate (PrefixTemplate _ (LPrefixName _ name _ _)) = name
getOpNameFromPrefixTemplate (PrefixTemplate _ (LPPrefixName _ name _ _)) = name
getOpNameFromPrefixTemplate (PowerPrefixTemplate _ ) = zChar_power

getOpNameFromPostfixTemplate :: PostfixTemplate -> String
getOpNameFromPostfixTemplate (PostfixTemplate _ (PostPostfixName _ name)) = name
getOpNameFromPostfixTemplate (PostfixTemplate _ (PostPPostfixName _ name)) = name
getOpNameFromPostfixTemplate (PostfixTemplate _ (ELPostfixName _ name _ _)) = name
getOpNameFromPostfixTemplate (PostfixTemplate _ (ELPPostfixName _ name _ _)) = name

getOpNameFromInfixTemplate :: InfixTemplate -> String
getOpNameFromInfixTemplate (InfixTemplate _ (InInfixName _ name)) = name
getOpNameFromInfixTemplate (InfixTemplate _ (InPInfixName _ name)) = name
getOpNameFromInfixTemplate (InfixTemplate _ (ELInfixName _ name _ _)) = name
getOpNameFromInfixTemplate (InfixTemplate _ (ELPInfixName _ name _ _)) = name

getOpNameFromNofixTemplate :: NofixTemplate -> String
getOpNameFromNofixTemplate (NofixTemplate _ (LNofixName _ name _ _)) = name
getOpNameFromNofixTemplate (NofixTemplate _ (LPNofixName _ name _ _)) = name
