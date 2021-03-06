{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Language.ISOZ.Utils.PrintIsoz where

-- pretty-printer generated by the BNF converter

import Language.ISOZ.Common.AbsIsoz
import Data.Char
import Language.ISOZ.ZChar
import Language.Circus.CircusChar


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id

instance Print Specification where
  prt i e = case e of
    SpecSect ann sections -> prPrec i 0 (concatD [prt 0 sections])
    SpecAnony ann paragraphs -> prPrec i 0 (concatD [prt 0 paragraphs])
    SpecEmpty ann -> prPrec i 0 (concatD [])

instance Print Section where
  prt i e = case e of
    Section ann zed sectname listname end paragraphs -> prPrec i 0 (concatD [prt 0 zed, doc (showString "section"), prt 0 sectname, prt 0 listname, prt 0 end, doc (showString "\n"), prt 0 paragraphs])
    -- transformed BaseSection
    BaseSection ann zed sectname end paragraphs -> prPrec i 0 (concatD [prt 0 zed, doc (showString "section"), prt 0 sectname, prt 0 end, doc (showString "\n"), prt 0 paragraphs])
    -- transformed InheritingSection 
    InheritingSection ann zed sectname listname end paragraphs -> prPrec i 0 (concatD [prt 0 zed, doc (showString "section"), prt 0 sectname, doc (showString "section"), prt 0 listname, prt 0 end, doc (showString "\n"), prt 0 paragraphs])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString "\n"), prt 0 xs])

instance Print SectionParents where
  prt i e = case e of
    BaseSection2 ann -> prPrec i 0 (concatD ([]))
    InheritingSection2 ann listname -> prPrec i 0 (concatD [doc (showString "parents"), prt 0 listname])

instance Print Paragraph where
  prt i e = case e of
    AxPara ann zed parabody end -> prPrec i 0 (concatD [prt 0 zed, prt 0 parabody, prt 0 end])
    AxdefPara ann ax schematext end -> prPrec i 0 (concatD [prt 0 ax, prt 0 schematext, prt 0 end])
    SchdefPara ann sch name schematext end -> prPrec i 0 (concatD [prt 0 sch, prt 0 name, prt 0 schematext, prt 0 end])
    GenAxdefPara ann ax formals schematext end -> prPrec i 0 (concatD [prt 0 ax, doc (showString "["), prt 0 formals, doc (showString "]"), prt 0 schematext, prt 0 end])
    GenSchdefPara ann sch formals name schematext end -> prPrec i 0 (concatD [prt 0 sch, doc (showString "["), prt 0 formals, doc (showString "]"), prt 0 name, prt 0 schematext, prt 0 end])
    -- transformed
    GivenPara ann listname -> prPrec i 0 (concatD [doc (showString "["), prt 0 listname, doc (showString "]")])
    GivenParaA ann listname -> prPrec i 0 (concatD [doc (showString "["), prt 0 listname, doc (showString "]")])
    HDefPara ann name expr -> prPrec i 0 (concatD [prt 0 name, doc (showString "=="), prt 0 expr])
    GenHDefPara ann name formals expr -> prPrec i 0 (concatD [prt 0 name, doc (showString "["), prt 0 formals, doc (showString "]"), doc (showString "=="), prt 0 expr])
    GenOpDefPara ann genname expr -> prPrec i 0 (concatD [prt 0 genname, doc (showString "=="), prt 0 expr])
    FreetypePara ann listtype -> prPrec i 0 (concatD [prt 0 listtype])
    ConjecturePara ann name pred -> prPrec i 0 (concatD [prt 0 name, doc (showString "\8866?"), prt 0 pred])
    GenConjecturePara ann name formals pred -> prPrec i 0 (concatD [prt 0 name, doc (showString "["), prt 0 formals, doc (showString "]"), doc (showString "\8866?"), prt 0 pred])
    OperatorPara ann optemplate -> prPrec i 0 (concatD [prt 0 optemplate])
    ChannelPara ann cdecl -> prPrec i 0 (concatD [prt 0 cdecl])
    ChannelFromPara ann cdecl -> prPrec i 0 (concatD [prt 0 cdecl])
    ChannelSetPara ann cdecl -> prPrec i 0 (concatD [prt 0 cdecl])
    _                        -> error ("Not supported " ++ (show e))
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString "\n"), prt 0 xs])

instance Print ParagraphBody where
  prt i e = case e of
    GivenPara2 ann listname -> prPrec i 0 (concatD [doc (showString "["), prt 0 listname, doc (showString "]")])
    HDefPara2 ann name expr -> prPrec i 0 (concatD [prt 0 name, doc (showString "=="), prt 0 expr])
    GenHDefPara2 ann name formals expr -> prPrec i 0 (concatD [prt 0 name, doc (showString "["), prt 0 formals, doc (showString "]"), doc (showString "=="), prt 0 expr])
    GenOpDefPara2 ann genname expr -> prPrec i 0 (concatD [prt 0 genname, doc (showString "=="), prt 0 expr])
    FreetypePara2 ann listtype -> prPrec i 0 (concatD [prt 0 listtype])
    OperatorPara2 ann optemplate -> prPrec i 0 (concatD [prt 0 optemplate])
    ChannelPara2 ann cdecl -> prPrec i 0 (concatD [doc (showString "channel"), prt 0 cdecl])

instance Print Freetype where
  prt i e = case e of
    Freetype ann name listbranches -> prPrec i 0 (concatD [prt 0 name, doc (showString "::="), prt 0 listbranches]) 
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString "&"), prt 0 xs])

instance Print Branch where
  prt i e = case e of
    ConstantBranch ann declname -> prPrec i 0 (concatD [prt 0 declname]) 
    ConstructorBranch ann declname expr -> prPrec i 0 (concatD [prt 0 declname, doc (showString "\x27EA"), prt 0 expr, doc (showString "\x27EB")]) 
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString "|"), prt 0 xs])

instance Print Formals where
  prt i e = case e of
    Formals ann listname -> prPrec i 0 (concatD [prt 0 listname]) 

instance Print Predicate where
  prt i e =  case e of
    AndPred ann p1 p2 ->  prPrec i 0 (concatD [prt 0 p1, doc (showString "\x2227"), doc (showString "\n"), prt 0 p2])
    OrPred ann p1 p2 ->  prPrec i 0 (concatD [prt 0 p1, doc (showString "\8743"), doc (showString "\n"), prt 0 p2])
    ForallPred ann schtext p -> prPrec i 0 (concatD [doc (showString "\x2200"), prt 0 schtext, prt 0 p])
    ExistsPred ann schtext p -> prPrec i 0 (concatD [doc (showString "\x2203"), prt 0 schtext, prt 0 p])
    Exists1Pred ann schtext p -> prPrec i 0 (concatD [doc (showString "\x2203\x2198\&1\x2196"), prt 0 schtext, prt 0 p])
    EquivPred ann p1 p2 -> prPrec i 0 (concatD [prt 0 p1, doc (showString "\8660"), prt 0 p2])
    ImplPred ann p1 p2 -> prPrec i 0 (concatD [prt 0 p1, doc (showString "\8658"), prt 0 p2])
    NegPred ann p -> prPrec i 0 (concatD [doc (showString "\172"), prt 0 p])
    RelPred ann relation -> prPrec i 0 (concatD [prt 0 relation])
    ExprPred ann p -> prPrec i 0 (concatD [prt 0 p])
    TruePred ann ->  prPrec i 0 (concatD [doc (showString "True")])
    FalsePred ann ->  prPrec i 0 (concatD [doc (showString "False")])
--    PPredicate p -> prPrec i 0 (concatD [doc (showString "("), prt 0 p, doc (showString ")")])

instance Print RenamePair where
  prt i e = case e of 
    RenamePair ann newname oldname -> prPrec i 0 (concatD [prt 0 newname, doc (showString "/"), prt 0 oldname])  
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])

instance Print Expression where
  prt i e = case e of
    NumExpr ann name -> prPrec i 0 (concatD [prt 0 name])
    ForallExpr ann schtext expr -> prPrec i 0 (concatD [doc (showString "\8704"), prt 0 schtext, doc (showString "\10625"), prt 0 expr]) 
    ExistsExpr ann schtext expr -> prPrec i 0 (concatD [doc (showString "\8707"), prt 0 schtext, doc (showString "\10625"), prt 0 expr]) 
    Exists1Expr ann schtext expr -> prPrec i 0 (concatD [doc (showString "\8707\8600\&1\8598"), prt 0 schtext, doc (showString "\10625"), prt 0 expr]) 
    LambdaExpr ann schtext expr -> prPrec i 0 (concatD [doc (showString "\955"), prt 0 schtext, doc (showString "\10625"), prt 0 expr]) 
    MuExpr ann schtext expr -> prPrec i 0 (concatD [doc (showString "\956"), prt 0 schtext, doc (showString "\10625"), prt 0 expr]) 
    LocalDefExpr ann decllist expr -> prPrec i 0 (concatD [doc (showString "let"), prt 0 decllist, doc (showString "\10625"), prt 0 expr]) 
    EquivExpr ann expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "\8660"), prt 0 expr2]) 
    ImplExpr ann expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "\8658"), prt 0 expr2]) 
    AndExpr ann expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "\8743"), prt 0 expr2]) 
    OrExpr ann expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "\8744"), prt 0 expr2]) 
    NegExpr ann expr -> prPrec i 0 (concatD [doc (showString "\172"), prt 0 expr]) 
    CondExpr ann pred expr1 expr2 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 pred, doc (showString "then"), prt 0 expr1, doc (showString "else"), prt 0 expr2]) 
    CompExpr ann expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "\10783"), prt 0 expr2]) 
    PipeExpr ann expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "\10784"), prt 0 expr2]) 
    HideExpr ann expr namelist -> prPrec i 0 (concatD [prt 0 expr, doc (showString "\10745"), doc (showString "("), prt 0 namelist, doc (showString ")")]) 
    ProjExpr ann expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "\10785"), prt 0 expr2]) 
    PreExpr ann expr -> prPrec i 0 (concatD [doc (showString "pre"), prt 0 expr]) 
    ProdExpr ann exprlist -> prPrec i 0 (concatD [prtExprList 0 exprlist]) 
            where prtExprList _ [x] = (concatD [prt 0 x])
                  prtExprList _ (x:xs) = (concatD [doc (showString "("), prt 0 x, doc (showString "\215"), prtExprList 0 xs, doc (showString ") ")])
    PowerExpr ann expr -> prPrec i 0 (concatD [doc (showString "\8473"), prt 0 expr]) 
    FuncApplExpr ann app -> prPrec i 0 (concatD [prt 0 app]) 
    ApplExpr ann expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, prt 0 expr2]) 
    DecorExpr ann expr decor -> prPrec i 0 (concatD [prt 0 expr, prt 0 decor]) 
    RenameExpr ann expr namepairlist -> prPrec i 0 (concatD [prt 0 expr, doc (showString "["), prt 0 namepairlist, doc (showString "]")]) 
    BindSelExpr ann expr refname -> prPrec i 0 (concatD [prt 0 expr, doc (showString "."), prt 0 refname]) 
    TupleSelExpr ann expr numeral -> prPrec i 0 (concatD [prt 0 expr, doc (showString "."), prt 0 numeral]) 
    ThetaExpr ann expr strokelist -> prPrec i 0 (concatD [doc (showString "\952"), prt 0 expr, prt 0 strokelist]) 
    RefExpr ann refname -> prPrec i 0 (concatD [prt 0 refname]) 
    GenRefExpr ann refname exprlist -> prPrec i 0 (concatD [prt 0 refname, doc (showString "["), prt 0 exprlist, doc (showString "]")]) 
    SetExpr ann exprlist -> prPrec i 0 (concatD [doc (showString "{"), prt 0 exprlist, doc (showString "}")]) 
    SetCompExpr ann schematext expr -> prPrec i 0 (concatD [doc (showString "{"), prt 0 schematext, doc (showString "\10625"), prt 0 expr, doc (showString "}")]) 
    CharSetCompExpr ann schematext -> prPrec i 0 (concatD [doc (showString "{"), prt 0 schematext, doc (showString "}")]) 
    SchemaExpr ann schematext -> prPrec i 0 (concatD [doc (showString "["), prt 0 schematext, doc (showString "]")]) 
    BindExtExpr ann decllist -> prPrec i 0 (concatD [doc (showString "\x2989"), prtDeclList 0 decllist, doc (showString "\x298A")]) 
            where prtDeclList _ [] = (concatD [])
                  prtDeclList _ [x] = (concatD [prt 0 x])
                  prtDeclList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
    TupleExtExpr ann expr1 exprlist -> prPrec i 0 (concatD [doc (showString "("), prt 0 expr1, prtexprList 0 exprlist, doc (showString ")")])  
            where prtexprList _ [] = (concatD [])
                  prtexprList _ [x] = (concatD [doc (showString ","), prt 0 x])
                  prtexprList _ (x:xs) = (concatD [doc (showString ","), prt 0 x, prt 0 xs])
    CharMuExpr ann schematext -> prPrec i 0 (concatD [doc (showString "("), doc (showString "\956"), prt 0 schematext, doc (showString ")")]) 
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])

instance Print SchemaText where
  prt i e = case e of 
    SchemaTextEmpty ann -> prPrec i 0 (concatD [doc (showString "")])
    SchemaTextDecl ann decl -> prPrec i 0 (concatD [prt 0 decl])
    SchemaTextPred ann pred -> prPrec i 0 (concatD [prt 0 pred])
    SchemaText ann decl predicate -> prPrec i 0 (concatD [prt 0 decl, doc (showString "|"), prt 0 predicate])

instance Print DeclPart where
  prt i e = case e of 
    DeclPart ann decls -> prPrec i 0 (concatD [prt 0 decls])

instance Print DeclName where
  prt i e = case e of
    DName ann name -> prPrec i 0 (concatD [prt 0 name])
    OpName ann name -> prPrec i 0 (concatD [prt 0 name])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])

instance Print Declaration where
  prt i e = case e of 
    Declaration ann declnames expr -> prPrec i 0 (concatD [prt 0 declnames,  doc (showString ":"), prt 0 expr])
    AbbrDecl ann declname expr -> prPrec i 0 (concatD [prt 0 declname,  doc (showString "=="), prt 0 expr])
    ExprDecl ann expr -> prPrec i 0 (concatD [prt 0 expr])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString "; "), prt 0 xs])

instance Print OperatorTemplate where
  prt i e = case e of 
    RelationTemplate ann template -> prPrec i 0 (concatD [doc (showString "relation"), prt 0 template])
    FunctionTemplate ann template -> prPrec i 0 (concatD [doc (showString "function"), prt 0 template])
    GenericTemplate ann template -> prPrec i 0 (concatD [doc (showString "generic"), prt 0 template])

instance Print CategoryTemplate where
  prt i e = case e of 
    PrefixCatTemplate ann template -> prPrec i 0 (concatD [prt 0 template])
    PostfixCatTemplate ann template -> prPrec i 0 (concatD [prt 0 template])
    InfixCatTemplate ann prec assoc template -> prPrec i 0 (concatD [prt 0 prec, prt 0 assoc, prt 0 template])
    NofixCatTemplate ann template -> prPrec i 0 (concatD [prt 0 template])

instance Print Prec where 
  prt i e = case e of 
    Prec ann prec -> prPrec i 0 (concatD [prt 0 prec])

instance Print Assoc where 
  prt i e = case e of 
    LeftAssoc ann -> prPrec i 0 (concatD [doc (showString "leftassoc")])
    RightAssoc ann -> prPrec i 0 (concatD [doc (showString "rightassoc")])

instance Print Template where
  prt i e = case e of 
    TPrefixTemplate ann template -> prPrec i 0 (concatD [prt 0 template])
    TPostfixTemplate ann template -> prPrec i 0 (concatD [prt 0 template])
    TInfixTemplate ann template -> prPrec i 0 (concatD [prt 0 template])
    TNofixTemplate ann template -> prPrec i 0 (concatD [prt 0 template])

instance Print PrefixTemplate where
  prt i e = case e of 
    PrefixTemplate ann prefixname  -> prPrec i 0 (concatD [doc (showString "("), prt 0 prefixname, doc (showString ")")])
    PowerPrefixTemplate ann        -> prPrec i 0 (concatD [doc (showString "("), doc (showString "\8473"), doc (showString " _ )")])

instance Print PostfixTemplate where
  prt i e = case e of 
    PostfixTemplate ann postfixname  -> prPrec i 0 (concatD [doc (showString "("), prt 0 postfixname, doc (showString ")")])

instance Print InfixTemplate where
  prt i e = case e of 
    InfixTemplate ann infixname  -> prPrec i 0 (concatD [doc (showString "("), prt 0 infixname, doc (showString ")")])

instance Print NofixTemplate where
  prt i e = case e of 
    NofixTemplate ann nofixname  -> prPrec i 0 (concatD [doc (showString "("), prt 0 nofixname, doc (showString ")")])

instance Print RefName where
  prt i e = case e of
    RName ann name -> prPrec i 0 (concatD [prt 0 name])
    OpRName ann name -> prPrec i 0 (concatD [doc (showString "("), prt 0 name, doc (showString ")")])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])

instance Print OpName where
  prt i e = case e of
    PrefixOpName ann name -> prPrec i 0 (concatD [prt 0 name])
    PostfixOpName ann name -> prPrec i 0 (concatD [prt 0 name])
    InfixOpName ann name -> prPrec i 0 (concatD [prt 0 name])
    NofixOpName ann name -> prPrec i 0 (concatD [prt 0 name])

instance Print PrefixName where
  prt i e = case e of
    PrePrefixName ann pre -> prPrec i 0 (concatD [prt 0 pre, doc (showString "_")])
    PrePPrefixName ann prep -> prPrec i 0 (concatD [prt 0 prep, doc (showString "_")])
    LPrefixName ann l listesss eresre -> prPrec i 0 (concatD [prt 0 l, prt 0 listesss, prt 0 eresre, doc (showString "_")])
    LPPrefixName ann l listesss erepsrep -> prPrec i 0 (concatD [prt 0 l, prt 0 listesss, prt 0 erepsrep, doc (showString "_")])

instance Print PostfixName where
  prt i e = case e of
    PostPostfixName ann post -> prPrec i 0 (concatD [doc (showString "_"), prt 0 post])
    PostPPostfixName ann postp -> prPrec i 0 (concatD [doc (showString "_"), prt 0 postp])
    ELPostfixName ann el listesss ersr -> prPrec i 0 (concatD [doc (showString "_"), prt 0 el, prt 0 listesss, prt 0 ersr])
    ELPPostfixName ann elp listesss erpsrp -> prPrec i 0 (concatD [doc (showString "_"), prt 0 elp, prt 0 listesss, prt 0 erpsrp])

instance Print InfixName where
  prt i e = case e of
    InInfixName ann ii -> prPrec i 0 (concatD [doc (showString "_"), prt 0 ii, doc (showString "_")])
    InPInfixName ann ip -> prPrec i 0 (concatD [doc (showString "_"), prt 0 ip, doc (showString "_")])
    ELInfixName ann el listesss eresre -> prPrec i 0 (concatD [doc (showString "_"), prt 0 el, prt 0 listesss, prt 0 eresre, doc (showString "_")])
    ELPInfixName ann elp listesss erepsrep -> prPrec i 0 (concatD [doc (showString "_"), prt 0 elp, prt 0 listesss, prt 0 erepsrep, doc (showString "_")])

instance Print NofixName where
  prt i e = case e of
    LNofixName ann l listesss ersr -> prPrec i 0 (concatD [prt 0 l, prt 0 listesss, prt 0 ersr])
    LPNofixName ann l listesss erpsrp -> prPrec i 0 (concatD [prt 0 l, prt 0 listesss, prt 0 erpsrp])

instance Print ESSS where
  prt i e = case e of
    ES ann es -> prPrec i 0 (concatD [doc (showString "_"), prt 0 es])
    SS ann ss -> prPrec i 0 (concatD [doc (showString ",,"), prt 0 ss])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])

instance Print ESSS_N where
  prt i e = case e of
    ES_N ann name es -> prPrec i 0 (concatD [prt 0 name, prt 0 es])
    SS_N ann name ss -> prPrec i 0 (concatD [prt 0 name, prt 0 ss])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])

instance Print ESSS_E where
  prt i e = case e of
    ES_E ann expr es -> prPrec i 0 (concatD [prt 0 expr, prt 0 es])
    SS_E ann expr ss -> prPrec i 0 (concatD [prt 0 expr, prt 0 ss])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])

instance Print ERESRE where
  prt i e = case e of
    ERE ann es -> prPrec i 0 (concatD [doc (showString "_"), prt 0 es])
    SRE ann ss -> prPrec i 0 (concatD [doc (showString ",,"), prt 0 ss])

instance Print ERESRE_N where
  prt i e = case e of
    ERE_N ann name es -> prPrec i 0 (concatD [prt 0 name, prt 0 es])
    SRE_N ann name ss -> prPrec i 0 (concatD [prt 0 name, prt 0 ss])

instance Print ERESRE_E where
  prt i e = case e of
    ERE_E ann expr es -> prPrec i 0 (concatD [prt 0 expr, prt 0 es])
    SRE_E ann exprlist ss -> prPrec i 0 (concatD [prt 0 exprlist, prt 0 ss])

instance Print EREPSREP where
  prt i e = case e of
    EREP ann es -> prPrec i 0 (concatD [doc (showString "_"), prt 0 es])
    SREP ann ss -> prPrec i 0 (concatD [doc (showString ",,"), prt 0 ss])

instance Print EREPSREP_E where
  prt i e = case e of
    EREP_E ann expr es -> prPrec i 0 (concatD [prt 0 expr, prt 0 es])
    SREP_E ann exprlist ss -> prPrec i 0 (concatD [prt 0 exprlist, prt 0 ss])

instance Print ERSR where
  prt i e = case e of
    ER ann es -> prPrec i 0 (concatD [doc (showString "_"), prt 0 es])
    SR ann ss -> prPrec i 0 (concatD [doc (showString ",,"), prt 0 ss])

instance Print ERSR_N where
  prt i e = case e of
    ER_N ann name er -> prPrec i 0 (concatD [prt 0 name, prt 0 er])
    SR_N ann name sr -> prPrec i 0 (concatD [prt 0 name, prt 0 sr])

instance Print ERSR_E where
  prt i e = case e of
    ER_E ann expr er -> prPrec i 0 (concatD [prt 0 expr, prt 0 er])
    SR_E ann exprlist sr -> prPrec i 0 (concatD [prt 0 exprlist, prt 0 sr])

instance Print ERPSRP where
  prt i e = case e of
    ERP ann es -> prPrec i 0 (concatD [doc (showString "_"), prt 0 es])
    SRP ann ss -> prPrec i 0 (concatD [doc (showString ",,"), prt 0 ss])

instance Print ERPSRP_E where
  prt i e = case e of
    ERP_E ann expr er -> prPrec i 0 (concatD [prt 0 expr, prt 0 er])
    SRP_E ann exprlist sr -> prPrec i 0 (concatD [prt 0 exprlist, prt 0 sr])

instance Print GenName where
  prt i e = case e of
    PrefixGenName ann name -> prPrec i 0 (concatD [prt 0 name]) 
    PostfixGenName ann name -> prPrec i 0 (concatD [prt 0 name]) 
    InfixGenName ann name -> prPrec i 0 (concatD [prt 0 name]) 
    NofixGenName ann name -> prPrec i 0 (concatD [prt 0 name]) 

instance Print PrefixGenName where
  prt i e = case e of
    PrePrefixGenName ann pre name -> prPrec i 0 (concatD [prt 0 pre, prt 0 name])
    LPrefixGenName ann l listesss_n eresre_n name -> prPrec i 0 (concatD [prt 0 l, prt 0 listesss_n, prt 0 eresre_n, prt 0 name])

instance Print PostfixGenName where
  prt i e = case e of
    PostPostfixGenName ann name post -> prPrec i 0 (concatD [prt 0 name, prt 0 post])
    ELPostfixGenName ann name el listesss_n ersr_n -> prPrec i 0 (concatD [prt 0 name, prt 0 el, prt 0 listesss_n, prt 0 ersr_n])

instance Print InfixGenName where
  prt i e = case e of
    InInfixGenName ann name1 ii name2 -> prPrec i 0 (concatD [prt 0 name1, prt 0 ii, prt 0 name2])
    ELInfixGenName ann name1 el listesss_n eresre_n name2 -> prPrec i 0 (concatD [prt 0 name1, prt 0 el, prt 0 listesss_n, prt 0 eresre_n, prt 0 name2])

instance Print NofixGenName where
  prt i e = case e of
    LNofixGenName ann l listesss_n ersr_n -> prPrec i 0 (concatD [prt 0 l, prt 0 listesss_n, prt 0 ersr_n])

instance Print Relation where
  prt i e = case e of
    PrefixRel ann  rel -> prPrec i 0 (concatD [prt 0 rel]) 
    PostfixRel ann rel -> prPrec i 0 (concatD [prt 0 rel]) 
    InfixRel ann   rel -> prPrec i 0 (concatD [prt 0 rel]) 
    NofixRel ann   rel -> prPrec i 0 (concatD [prt 0 rel]) 

instance Print PrefixRel where
  prt i e = case e of
    PrePPrefixRel ann prep expr -> prPrec i 0 (concatD [prt 0 prep, prt 0 expr])
    LPPrefixRel ann lp listesss_e erepsrep_e expr -> prPrec i 0 (concatD [prt 0 lp, prt 0 listesss_e, prt 0 erepsrep_e, prt 0 expr])

instance Print PostfixRel where
  prt i e = case e of
    PostPPostfixRel ann expr postp -> prPrec i 0 (concatD [prt 0 expr, prt 0 postp])
    ELPPostfixRel ann expr elp listesss_e erpsrp_e -> prPrec i 0 (concatD [prt 0 expr, prt 0 listesss_e, prt 0 erpsrp_e])

instance Print IP_E where
  prt i e = case e of
    IP_E ann str expr -> prPrec i 0 (concatD [prt 0 str, prt 0 expr])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])

instance Print InfixRel where
  prt i e = case e of
    InInfixRel ann expr listip_e -> prPrec i 0 (concatD [prt 0 expr, prt 0 listip_e])
    ELPInfixRel ann expr1 elp listesss_e erepsrep_e expr2 -> prPrec i 0 (concatD [prt 0 expr1, prt 0 elp, prt 0 listesss_e, prt 0 erepsrep_e, prt 0 expr2])

instance Print NofixRel where
  prt i e = case e of
    LPNofixRel ann lp listesss_e erepserp_e -> prPrec i 0 (concatD [prt 0 lp, prt 0 listesss_e, prt 0 erepserp_e])

instance Print Application where
  prt i e = case e of
    PrefixApp ann  app -> prPrec i 0 (concatD [prt 0 app]) 
    PostfixApp ann app -> prPrec i 0 (concatD [prt 0 app]) 
    InfixApp ann   app -> prPrec i 0 (concatD [prt 0 app]) 
    NofixApp ann   app -> prPrec i 0 (concatD [prt 0 app]) 

instance Print PrefixApp where
  prt i e = case e of
    PrePrefixApp ann pre expr -> prPrec i 0 (concatD [doc (showString "("), prt 0 pre, prt 0 expr, doc (showString ") ")])
    LPrefixApp ann l listesss_e eresre_e expr -> prPrec i 0 (concatD [doc (showString "("), prt 0 l, prt 0 listesss_e, prt 0 eresre_e, prt 0 expr, doc (showString ") ")])

instance Print PostfixApp where
  prt i e = case e of
    PostPostfixApp ann expr post -> prPrec i 0 (concatD [doc (showString "("), prt 0 expr, prt 0 post, doc (showString ") ")])
    ELPostfixApp ann expr el listesss_e ersr_e -> prPrec i 0 (concatD [doc (showString "("), prt 0 expr, prt 0 el, prt 0 listesss_e, prt 0 ersr_e, doc (showString ") ")])

instance Print InfixApp where
  prt i e = case e of
    InInfixApp ann expr1 ii expr2 -> prPrec i 0 (concatD [doc (showString "("), prt 0 expr1, prt 0 ii, prt 0 expr2, doc (showString ") ")])
    ELInfixApp ann expr1 el listesss_e eresre_e expr2 -> prPrec i 0 (concatD [doc (showString "("), prt 0 expr1, prt 0 el, prt 0 listesss_e, prt 0 eresre_e, prt 0 expr2, doc (showString ") ")])

instance Print NofixApp where
  prt i e = case e of
    LNofixApp ann l listesss_e erse_e -> prPrec i 0 (concatD [doc (showString "("), prt 0 l, prt 0 listesss_e, prt 0 erse_e, doc (showString ")")])

instance Print ChannelDecl where
  prt i e = case e of
    ChannelDecl ann simplec -> prPrec i 0 (concatD [doc (showString "channel"), prt 0 simplec])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString "\n"), prt 0 xs])

instance Print SimpleCDecl where
  prt i e = case e of
    SyncSimpleCDecl ann listname -> prPrec i 0 (concatD [prt 0 listname])
    TypedSimpleCDecl ann listname expr -> prPrec i 0 (concatD [prt 0 listname, doc (showString ":"), prt  0 expr]) 
    GenericTypedSimpleCDecl ann formals listname expr -> prPrec i 0 (concatD [doc (showString "["), prt 0 formals, doc (showString "]"), prt 0 listname, doc (showString ":"), prt 0 expr]) 
--    SchemaSimpleCDecl expr -> prPrec i 0 (concatD [prt 0 expr]) 
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])

instance Print ChannelFromDecl where
  prt i e = case e of
    ChannelFromDecl ann fromcdecl -> prPrec i 0 (concatD [doc (showString "channelfrom"), prt  0 fromcdecl]) 
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString "\n"), prt 0 xs])

instance Print FromCDecl where
  prt i e = case e of
    RefFromCDecl ann expr -> prPrec i 0 (concatD [prt  0 expr])
    RefDecorFromCDecl ann expr -> prPrec i 0 (concatD [prt  0 expr])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])

instance Print ChannelSetDecl where
  prt i e = case e of
    ChannelSetDecl ann name expr -> prPrec i 0 (concatD [doc (showString "channelset"), prt  0 expr])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])

instance Print CSExp where
  prt i e = case e of
    CSExpEmpty ann  -> prPrec i 0 (concatD [doc (showString cChar_lchanset), doc (showString cChar_rchanset)])
    CSExpExt ann exprs -> prPrec i 0 (concatD [doc (showString cChar_lchanset), prt 0 exprs, doc (showString cChar_rchanset)]) 
    CSExpRef ann expr -> prPrec i 0 (concatD [prt 0 expr])
    CSExpUnion ann csexp1 csexp2  -> prPrec i 0 (concatD [doc (showString "("), prt 0 csexp1, doc (showString zChar_union), prt  0 csexp2, doc (showString ") ")])
    CSExpInter ann csexp1 csexp2  -> prPrec i 0 (concatD [doc (showString "("), prt 0 csexp1, doc (showString zChar_inter), prt  0 csexp2, doc (showString ") ")])
    CSExpDiff ann csexp1 csexp2  -> prPrec i 0 (concatD [doc (showString "("), prt 0 csexp1, doc (showString zChar_diff), prt  0 csexp2, doc (showString ") ")])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
