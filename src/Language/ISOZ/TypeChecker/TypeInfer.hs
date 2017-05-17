{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{- |
Module      : Language.ISOZ.Parser.TypeInfer
Description : 
Copyright   : (C) Kangfeng Ye, 2017
License     : BSD3
Maintainer  : ye.randall@gmail.com 
Stability   : experimental

Annotate syntax sentence and typecheck by inference rules in Clause 13 "Type inference rules"

It provides

     - typeInferTraverseTree: traverse the syntax tree
-}
module Language.ISOZ.TypeChecker.TypeInfer where

import Language.ISOZ.ZChar
import Language.ISOZ.Common.Error
import Language.ISOZ.Common.AbsIsoz
import Data.Char
import Language.ISOZ.Parser.Mktuple
import Language.ISOZ.Lexer.ISOZLexer (lexerError)
import Language.ISOZ.Common.TypeCMonad 
import Language.ISOZ.Common.Type
import Language.ISOZ.Common.Ann 
import Control.Monad
import qualified Data.Map as Map
import Data.List as List
import Language.ISOZ.Parser.SynTransformerOne
import Language.ISOZ.Parser.SynTransformerTwo (synTraverse2Tree, transformSchema)
import Language.ISOZ.Parser.SynTransformerTwo (appToExpr)

{- | traverse the syntax tree to type check it
 
- if there is an error, then return Left error
- otherwise, return a transformed and annotated syntax tree
 -}
typeInferTraverseTree :: TypeInferTraverse a => a -> Either String a 
typeInferTraverseTree = \s -> case runTypeC (traverseT s) of
                        Left (err) -> Left err
                        Right (Left err) -> Left err
                        Right (Right e) -> Right e

-- | Traverse for typechecking
class TypeInferTraverse a where
    traverseT :: a -> TypeC (Either String a)
    traverseTList :: [a] -> TypeC (Either String [a])
    traverseTList [] = do return (Right [])
    traverseTList (x:xs) = do m <- traverseT x;case m of
                                Left err -> return (Left err)
                                Right e -> do 
                                    n <- traverseT xs; case n of
                                         Left err -> return (Left err)
                                         Right es -> return (Right ((:) e es))
    -- | (13.2.3.3) Implicit Instantiation
    traverseTImplInst :: a -> TypeC a 
    traverseTImplInstList :: [a] -> TypeC [a] 
    traverseTImplInstList [] = do return []
    traverseTImplInstList (x:xs) = do m <- traverseTImplInst x
                                      n <- traverseTImplInstList xs
                                      return ((:) m n)

-- | Traverse a list
instance TypeInferTraverse a => TypeInferTraverse [a] where
  traverseT = traverseTList
  traverseTImplInst = traverseTImplInstList

-- | Traverse specification
instance TypeInferTraverse Specification where
  traverseT e = case e of
    SpecSectA ann sections -> do
                    -- the prelude section is presumably loaded at first
                    -- setSectEnvTypeC (Map.insert "prelude" preludeSectTypeEnv (Map.empty))
                    n <- traverseT sections;  case n of
                        Left err -> return (Left err)
                        Right s -> return (Right (SpecSectA ann s))
    SpecEmpty ann -> return (Right (SpecEmpty ann))
  -- 
  traverseTImplInst e = return e

instance TypeInferTraverse Section where
  traverseT e = case e of
    --  C.3.2
    InheritingSectionA ann sectname listname paragraphs -> do 
        -- 13.2.2.1
        -- 1. check if the section name is unique
        sectenv <- getSectEnvTypeC
        if sectname `elem` (Map.keys sectenv) && sectname /= "prelude"
        then return (Left (err2Msg (ErrTypeCheck (ETypeCDupSect (assembleError ("The section name [" ++ sectname ++ "] has been used before. The same name sections are not allowed") ann)))))
        else if not (null (listname \\ (Map.keys sectenv)))
        -- 2. check if the parent sections are loaded
             then return (Left (err2Msg (ErrTypeCheck (ETypeCUndefSect (assembleError ("The parent sections [" ++ show (listname \\ (Map.keys sectenv)) ++ "] are unknown") ann)))))
        -- 3. include prelude section.
        -- if the section is "prelude", then it is empty. Otherwise, load prelude and other parent sections into type environment
             else do setEnvTypeC (beta_0 sectname listname sectenv)
                     setCurSectName sectname 
                     m <- traverseT paragraphs; case m of 
                         Left err -> return (Left err)
        -- 4. check names in the signatures of all paragraphs are disjoint 
                         Right s -> case disjointNames s of 
                                Left err -> return (Left err)
        -- 5. check no conflicts in this section's built SectTypeEnv and all its parents' SectTypeEnv
                                Right True -> case not (null (conflictNames sectname listname sectenv s)) of
                                            True -> return (Left (err2Msg (ErrTypeCheck (ETypeCDupName (assembleError ("Names [" ++ show (conflictNames sectname listname sectenv s) ++ "] are used in more than one section [this section: " ++ sectname ++ "]!") ann)))))
--                                            False -> return (Right (InheritingSectionA (SectTypeEnvAnn (SectTypeEnv (gamma sectname listname sectenv s)): ann) sectname listname (removeOpTempParas s)))
                                            False -> do
                                                setSectEnvTypeC (Map.insert sectname newSectEnvType sectenv)
                                                return (Right (InheritingSectionA (SectTypeEnvAnn (newSectEnvType): ann) sectname listname (removeOpTempParas s)))
                                        where newSectEnvType = SectTypeEnv (gamma sectname listname sectenv s)
                     -- γ-1
                     where gamma_1 "prelude" sectenv = Map.empty 
                           gamma_1 sectname  sectenv = getSectTypeEnvMap preludeSectTypeEnv 
                           -- gamma_1 sectname  sectenv = if "prelude" `elem` (Map.keys sectenv) 
                           --                             then Map.empty   -- prelude has been loaded (It is different from the description in 13.2.2.1)
                           --                             else getSectTypeEnvMap preludeSectTypeEnv 
                           gamma_0 sectname psects sectenv = Map.unionWith withFunc (gamma_1 sectname sectenv) (combineSectEnv psects sectenv)
                           beta_0 sectname psects sectenv = Map.fromList(sectTypeEnvToTypeEnv (SectTypeEnv (gamma_0 sectname psects sectenv)))
                           gamma sectname psects sectenv paras = (Map.unionWith withFunc (gamma_0 sectname psects sectenv) (signToSectTypeEnv sectname (combineSignsFromParas paras)))
                           -- conflictNames sectname psects sectenv paras = Map.keys (Map.filter filterConflictFunc (gamma sectname listname sectenv paras))
                           -- duplicate preludes are allowed
                           conflictNames sectname psects sectenv paras = Map.keys (Map.filter filterConflictFunc (Map.difference (gamma sectname listname sectenv paras) (getSectTypeEnvMap preludeSectTypeEnv)))
                           removeOpTempParas [] = []
                           removeOpTempParas ((OperatorPara _ _) : ps) = removeOpTempParas ps
                           removeOpTempParas (p : ps) = p : (removeOpTempParas ps)
  -- 
  traverseTImplInst e = return e

instance TypeInferTraverse Paragraph where
  traverseT e = case e of
    -- C.4.2
    GivenParaA ann listname -> do 
        clearTypeVarList 
        case returnDuplicates listname of
          -- attach signature annotation with this paragraph
          [] -> do sectname <- getCurSectName
                   env <- getEnvTypeC
                   -- 13.2.2.1 i) add new signature to type environment, then paragraphs after can refer to this GIVEN sets.
                   setEnvTypeC (Map.union (genSig Map.empty listname) env)
                   r <- isAllGenInsted 
                   case r of 
                        True -> do 
                            -- attached signature
                            return (Right (GivenParaA ((SigAnn (genSig Map.empty listname)) : ann) listname))
                        False -> return (Left (err2Msg (ErrTypeCheck (ETypeCGenericUninst (assembleError ("some references are not instantiated") ann)))))
              where genSig m [x] = (Map.insert x (Type (PowerType (GIVEN x))) m)
                    genSig m (x:xs) = genSig (Map.insert x (Type (PowerType (GIVEN x))) m) xs
          s@(x:xs) -> return (Left (err2Msg (ErrTypeCheck (ETypeCDupName (assembleError ("[" ++ show s ++ "] found in Given Paragraph") ann)))))
    -- C.4.3
    AxdefParaA ann expr -> do 
        clearTypeVarList
        p <- traverseT expr
        case p of
            Left err -> return (Left err)
            Right s -> do 
                    r <- isAllGenInsted 
                    case r of 
                         True -> do 
                            s' <- traverseTImplInst s 
                            case (extractType (getAnnTerm s')) of
                                Type (PowerType (SchType sig)) -> do 
                                    env <- getEnvTypeC 
                                    setEnvTypeC (Map.union env sig)
                                    return (Right (AxdefParaA ((SigAnn sig) : ann) s'))
                                _                              -> return (Left (err2Msg (ErrTypeCheck (ETypeCNotSchType (assembleError ("The expression in the Axdef paragraph is not a schema type") ann)))))
                         False -> return (Left (err2Msg (ErrTypeCheck (ETypeCGenericUninst (assembleError ("some references are not instantiated") ann)))))
    -- 13.2.4.3 Generic axiomatic description paragraph
    GenAxdefParaA ann formals expr -> do
        clearTypeVarList
        if not (null (returnDuplicates formals))
        then return (Left (err2Msg (ErrTypeCheck (ETypeCDupName (assembleError ("[" ++ show (returnDuplicates formals) ++ "] are found in the formal parameters of a generic axdef paragraph") ann)))))
        else do 
            env <- getEnvTypeC
            setEnvTypeC (Map.union (Map.fromList (formalsToSig formals)) env)
            p <- traverseT expr; case p of
                Left err -> return (Left err)
                Right s -> do
                        r <- isAllGenInsted 
                        case r of 
                             True -> do 
                                s' <- traverseTImplInst s 
                                case (extractType (getAnnTerm s')) of
                                    Type (PowerType (SchType sig)) -> do 
                                        setEnvTypeC (Map.union env (transSig sig formals))
                                        return (Right (GenAxdefParaA ((SigAnn (transSig sig formals)) : ann) formals s'))
                                    _                              -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The expression in the generic Axdef paragraph is not powerset of a schema type") ann)))))
                             False -> return (Left (err2Msg (ErrTypeCheck (ETypeCGenericUninst (assembleError ("some references are not instantiated") ann)))))
            where -- build a signature from a list of formal parameters
                  formalsToSig [] = []
                  formalsToSig [x] = [(x, (Type (PowerType (GENTYPE x))))]
                  formalsToSig (x:xs) = (formalsToSig [x]) ++ (formalsToSig xs)
                  transSig sig formals = Map.fromList (convertSig (Map.toList sig) formals)
                  convertSig [] formals = []
                  convertSig ((i, Type t2) : xs) formals = (i, (GenType formals t2)) : (convertSig xs formals)

    -- 13.2.4.4 
    FreetypeParaA ann listtype -> do
        clearTypeVarList
        case returnDuplicates (getAllNamesInFreetypes listtype) of
            (x:xs)  -> return (Left (err2Msg (ErrTypeCheck (ETypeCDupName (assembleError ("[" ++ show xs ++ "] in the definition of a free type ") ann)))))
            []      -> do
                env <- getEnvTypeC  
                -- a new type environment with all freetypes' name related GIVEN types
                setEnvTypeC (Map.union env (Map.fromList (genFreetypeNamesSig listtype)))
                rl <- typeInferFreetypes listtype 
                case rl of
                    Left err -> return (Left err)
                    Right (listtype', sig) -> do
                        newenv <- getEnvTypeC  
                        setEnvTypeC (Map.union newenv sig)
                        r <- isAllGenInsted 
                        case r of 
                             True -> do 
                                return (Right (FreetypeParaA ((SigAnn (Map.union sig (Map.fromList (genFreetypeNamesSig listtype)))) : ann) listtype'))
                             False -> return (Left (err2Msg (ErrTypeCheck (ETypeCGenericUninst (assembleError ("some references are not instantiated") ann)))))
              -- get all names declared in a list of free types
        where getAllNamesInFreetypes :: [Freetype] -> [String]
              getAllNamesInFreetypes [] = [] 
              getAllNamesInFreetypes (f:fs) = (getAllNamesInAFreetype f) ++ (getAllNamesInFreetypes fs)
              --
              getAllNamesInAFreetype :: Freetype -> [String]
              getAllNamesInAFreetype (Freetype _ name branches) = name : (getAllNamesInBranches branches)
              --
              getAllNamesInBranches :: [Branch] -> [String]
              getAllNamesInBranches [] = []  
              getAllNamesInBranches ((ConstantBranchA _ n) : xs)  = n : (getAllNamesInBranches xs)  
              getAllNamesInBranches ((ConstructorBranchA _ n e) : xs)  = n : (getAllNamesInBranches xs)  
              -- a new type environment introduced only by the name of free types 
              genFreetypeNamesSig :: [Freetype] -> [(String, Type)] 
              genFreetypeNamesSig [] = []
              genFreetypeNamesSig ((Freetype _ name _):fs) = (name, Type $ PowerType $ GIVEN name) : (genFreetypeNamesSig fs)
              -- type inference of a list of free types 
              -- return error by (Left err), or a pair from a new list of type checked free types to the signatures buit
              typeInferFreetypes :: [Freetype] -> TypeC (Either String ([Freetype], Sig))
              typeInferFreetypes [] = return (Right ([], Map.empty))
              typeInferFreetypes (f:fs) = do
                    r <- typeInferAFreetype f
                    case r of
                        Left err -> return (Left err)
                        Right (f', sig1) -> do
                            rs <- typeInferFreetypes fs
                            case rs of
                                Left err -> return (Left err)
                                Right (fs', sig2) -> return (Right ((f' : fs'), (Map.union sig1 sig2)))
              -- 
              typeInferAFreetype :: Freetype -> TypeC (Either String (Freetype, Sig))
              typeInferAFreetype (Freetype ann name branches) = do
                    r <- typeInferBranches branches (GIVEN name)
                    case r of 
                        Left err    -> return (Left err)
                        Right (branches', sig) -> return (Right ((Freetype ann name branches'), sig))
              -- 
              typeInferBranches :: [Branch] -> Type2 -> TypeC (Either String ([Branch], Sig))
              typeInferBranches [] (GIVEN f) = return (Right ([], Map.empty))
              typeInferBranches ((ConstantBranchA ann h):bs) (GIVEN f) = do
                    r <- typeInferBranches bs (GIVEN f)
                    case r of
                        Left err    -> return (Left err)
                        Right (bs', ms) -> return (Right ( (ConstantBranchA ann h) : bs', (Map.union (Map.fromList [(h, Type (GIVEN f))]) ms)))
              typeInferBranches ((ConstructorBranchA ann g eb):bs) (GIVEN f) = do
                    e1 <- traverseT eb
                    case e1 of
                        Left err  -> return (Left err)
                        Right e1' -> case getType2FromTerm e1' of 
                                        (PowerType t)   -> do
                                            rs <- typeInferBranches bs (GIVEN f)
                                            case rs of
                                                Left err    -> return (Left err)
                                                Right (bs', ms) -> return (Right ((ConstructorBranchA ann g e1') : bs', (Map.union (Map.fromList [(g, Type (PowerType (ProdType [t, (GIVEN f)])))]) ms)))
                                        _               -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The type of the expression in a branch of a free type definition should be powerset") ann)))))
              
              
    -- 13.2.4.5 Conjecture paragraph (empty signature)
    ConjectureParaA ann name pred -> do 
        clearTypeVarList
        p <- traverseT pred
        case p of
            Left err -> return (Left err)
            Right s -> do 
                r <- isAllGenInsted 
                case r of 
                     True -> do 
                        s' <- traverseTImplInst s 
                        return (Right (ConjectureParaA ((SigAnn Map.empty) : ann) name s'))
                     False -> return (Left (err2Msg (ErrTypeCheck (ETypeCGenericUninst (assembleError ("some references are not instantiated") ann)))))
    -- 13.2.4.6 
    GenConjectureParaA ann name formals pred -> do 
        clearTypeVarList
        if not (null (returnDuplicates formals))
        then return (Left (err2Msg (ErrTypeCheck (ETypeCDupName (assembleError ("[" ++ show (returnDuplicates formals) ++ "] are found in the formal parameters of a generic conjecture paragraph") ann)))))
        else do 
            env <- getEnvTypeC
            setEnvTypeC (Map.union (Map.fromList (formalsToSig formals)) env)
            p <- traverseT pred; case p of
                Left err -> return (Left err)
                Right s -> do
                        setEnvTypeC (env) -- restore
                        r <- isAllGenInsted 
                        case r of 
                             True -> do 
                                s' <- traverseTImplInst s 
                                return (Right (GenConjectureParaA ((SigAnn Map.empty) : ann) name formals s'))
                             False -> return (Left (err2Msg (ErrTypeCheck (ETypeCGenericUninst (assembleError ("some references are not instantiated") ann)))))
            where -- build a signature from a list of formal parameters
                  formalsToSig [] = []
                  formalsToSig [x] = [(x, (Type (PowerType (GENTYPE x))))]
                  formalsToSig (x:xs) = (formalsToSig [x]) ++ (formalsToSig xs)
{-
 - OperatorPara should not be in annotated syntax and it should be removed in SynTransformerTwo
 - However, since in SynTransformerTwo we don't have the information about whether it is a function or generic
 - application when transforming application expression, we delay the transformation to here. 
 - The reason is we have the state monad now and we can keep this information in a state,
 - and then used later to transform it
-}
    OperatorPara ann optemplate -> case optemplate of
                FunctionTemplate ann cattemp  -> do
                    m <- getOpNameFuncGen
                    setOpNameFuncGen (Map.insert (getOpNameFromCatTemplate cattemp) True m)
                    return (Right e)
                GenericTemplate ann cattemp  -> do
                    m <- getOpNameFuncGen
                    setOpNameFuncGen (Map.insert (getOpNameFromCatTemplate cattemp) False m)
                    return (Right e)
                -- for Relation, maybe just return it.
                RelationTemplate ann temp    -> do
                    return (Right e)
    ChannelPara ann cdecl -> do
        clearTypeVarList
        r <- isAllGenInsted 
        case r of 
             True -> do 
                return (Right e)
             False -> return (Left (err2Msg (ErrTypeCheck (ETypeCGenericUninst (assembleError ("some references are not instantiated") ann)))))
    ChannelFromPara ann cdecl -> do
        clearTypeVarList
        r <- isAllGenInsted 
        case r of 
             True -> do 
                return (Right e)
             False -> return (Left (err2Msg (ErrTypeCheck (ETypeCGenericUninst (assembleError ("some references are not instantiated") ann)))))
    ChannelSetPara ann cdecl -> do
        clearTypeVarList
        r <- isAllGenInsted 
        case r of 
             True -> do 
                return (Right e)
             False -> return (Left (err2Msg (ErrTypeCheck (ETypeCGenericUninst (assembleError ("some references are not instantiated") ann)))))

  -- 
  traverseTImplInst e = return e 
--instance TypeInferTraverse Freetype where
--  traverseT e = case e of
--    Freetype name listbranches -> Freetype (name) (traverseT listbranches) 
--  traverseTList [] = []
--  traverseTList (x:xs) = (:) (traverseT x) (traverseT xs)
--
--instance TypeInferTraverse Branch where
--  traverseT e = case e of
--    ConstantBranch declname -> ConstantBranch (traverseT declname)
--    ConstructorBranch declname expr -> ConstructorBranch (traverseT declname) (traverseT expr) 
--  traverseTList [] = []
--  traverseTList (x:xs) = (:) (traverseT x) (traverseT xs)
--
--instance TypeInferTraverse Formals where
--  traverseT e = case e of
--    Formals listname -> e 
--
instance TypeInferTraverse Predicate where
  traverseT e = case e of
    -- 13.2.5.1
    MemPred ann expr1 expr2 -> do
        e1 <- traverseT expr1
        e2 <- traverseT expr2
        case e1 of  
            Left err -> return (Left err)
            Right e1' -> case e2 of
                Left err -> return (Left err)
                Right e2' -> case (getType2FromTerm e2') of
                    (PowerType t)  -> do 
                        uni <- unify t (getType2FromTerm e1')
                        case uni of                
                            True -> return (Right (MemPred ann e1' e2'))
                            False -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch2 (assembleError ("The type of two expressions in a member predicate doesn't match. The type \n\ttype2: " ++ (show (getType2FromTerm e2')) ++ "\nof the second expression \n\te2:" ++ (show e2') ++ " \nshould be the powerset of that \n\ttype1: " ++ (show (getType2FromTerm e1')) ++ "\nof the first one \n\te1: " ++ (show e1') ++ "\n") ann)))))
--                    (GenType2 formals t21 t22) -> return (Left (assembleError ("The type of the second expression should be powerset") ann))
                    _              -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The type of the second expression: " ++ (show (getType2FromTerm e2')) ++ "\nshould be powerset") ann)))))
    -- 13.2.5.2
    TruePred ann -> return (Right (TruePred ann))
    -- 13.2.5.3
    NegPred ann p -> do 
        p1 <- traverseT p; case p1 of
            Left err -> return (Left err)
            Right p2 -> return (Right (NegPred ann p2))
    -- 13.2.5.4
    AndPred ann p1 p2 -> do 
        if not (isAChainedRel ann)
        then do 
            p1' <- traverseT p1
            p2' <- traverseT p2
            case p1' of
                Left err -> return (Left err)
                Right p1'' -> case p2' of
                    Left err -> return (Left err)
                    Right p2'' -> return (Right (AndPred ann p1'' p2''))
        else do -- Chained Relation (It's also the top of chained relation)
            -- e1 ip1 e2 ip2 e3 ip3 e4 => e1 ip1 (e2 : a1) /\ (e2 : a1) ip2 (e3 : a2) /\ (e3 : a2) ip3 (e4 : a3)
            -- here AndPred denotes the first /\, so 
            --      p1 = e1 ip1 (e2 : a1) 
            --      p2 = (e2 : a1) ip2 (e3 : a2) /\ (e3 : a2) ip3 (e4 : a3)
            p1' <- traverseT p1
            p2' <- traverseT p2
            case p1' of
                Left err -> return (Left err)
                Right p1'' -> case p2' of
                    Left err -> return (Left err)
                    Right p2'' -> do
                        r <- checkTypeUnify p1'' p2''
                        case r of 
                            True -> return (Right (AndPred (removeExtraTypeAdd ann) (removeExtraTypeFromPred p1'') (removeExtraTypeFromPred p2'')))
                            False -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The types in a chained relation does not match.\n\t" ++ show p1'' ++ "\n\t" ++ show p2'' ++ "\n") ann)))))
      where isAChainedRel [] = False
            isAChainedRel (x:xs) = if (x == (TypeAnn (Type $ TypeVar "Chain")))
                                   then True
                                   else isAChainedRel xs
            -- remove the extra TypeAnn ("a") for chained relation only, since it has been type-checked, they can be removed now
            removeExtraTypeAdd [] = []
            removeExtraTypeAdd (x:xs) = case x of
                                            TypeAnn (Type (TypeVar v))  -> if (v == "Chain") || (isPrefixOf "a" v)
                                                                            then removeExtraTypeAdd xs 
                                                                            else x:(removeExtraTypeAdd xs) 
                                            _                           -> x:(removeExtraTypeAdd xs) 
            -- remove the extra TypeAnn ("a") from expr
            removeExtraTypeFromOneExpr expr = setAnnTerm (removeExtraTypeAdd (getAnnTerm expr)) expr
            removeExtraTypeFromExprList [] = [] 
            removeExtraTypeFromExprList (x:xs) = (removeExtraTypeFromOneExpr x): (removeExtraTypeFromExprList xs) 
---------------- removeExtraTypeFromExpr 
            removeExtraTypeFromExpr (ForallExprA ann e' expr) = removeExtraTypeFromOneExpr (ForallExprA ann (removeExtraTypeFromOneExpr e') (removeExtraTypeFromOneExpr expr))
            removeExtraTypeFromExpr (Exists1ExprA ann e' expr) = removeExtraTypeFromOneExpr (Exists1ExprA ann (removeExtraTypeFromOneExpr e') (removeExtraTypeFromOneExpr expr))
            removeExtraTypeFromExpr (MuExprA ann e' expr) = removeExtraTypeFromOneExpr (MuExprA ann (removeExtraTypeFromOneExpr e') (removeExtraTypeFromOneExpr expr))
            removeExtraTypeFromExpr (LocalDefExpr ann decllist expr) = removeExtraTypeFromOneExpr (LocalDefExpr ann decllist (removeExtraTypeFromOneExpr expr))
            removeExtraTypeFromExpr (EquivExpr ann expr1 expr2) = removeExtraTypeFromOneExpr (EquivExpr ann (removeExtraTypeFromOneExpr expr1) (removeExtraTypeFromOneExpr expr2))
            removeExtraTypeFromExpr (ImplExpr ann expr1 expr2) = removeExtraTypeFromOneExpr (ImplExpr ann (removeExtraTypeFromOneExpr expr1) (removeExtraTypeFromOneExpr expr2))
            removeExtraTypeFromExpr (AndExpr ann expr1 expr2) = removeExtraTypeFromOneExpr (AndExpr ann (removeExtraTypeFromOneExpr expr1) (removeExtraTypeFromOneExpr expr2))
            removeExtraTypeFromExpr (OrExpr ann expr1 expr2) = removeExtraTypeFromOneExpr (OrExpr ann (removeExtraTypeFromOneExpr expr1) (removeExtraTypeFromOneExpr expr2))
            removeExtraTypeFromExpr (NegExpr ann expr) = removeExtraTypeFromOneExpr (NegExpr ann (removeExtraTypeFromOneExpr expr))
            removeExtraTypeFromExpr (CondExpr ann pred expr1 expr2) = removeExtraTypeFromOneExpr (CondExpr ann pred (removeExtraTypeFromOneExpr expr1) (removeExtraTypeFromOneExpr expr2))
            removeExtraTypeFromExpr (CompExpr ann expr1 expr2) = removeExtraTypeFromOneExpr (CompExpr ann (removeExtraTypeFromOneExpr expr1) (removeExtraTypeFromOneExpr expr2))
            removeExtraTypeFromExpr (PipeExpr ann expr1 expr2) = removeExtraTypeFromOneExpr (PipeExpr ann (removeExtraTypeFromOneExpr expr1) (removeExtraTypeFromOneExpr expr2))
            removeExtraTypeFromExpr (HideExpr ann expr namelist) = removeExtraTypeFromOneExpr (HideExpr ann (removeExtraTypeFromOneExpr expr) namelist)
            removeExtraTypeFromExpr (ProjExpr ann expr1 expr2) = removeExtraTypeFromOneExpr (ProjExpr ann (removeExtraTypeFromOneExpr expr1) (removeExtraTypeFromOneExpr expr2))
            removeExtraTypeFromExpr (PreExpr ann expr) = removeExtraTypeFromOneExpr (PreExpr ann (removeExtraTypeFromOneExpr expr))
            removeExtraTypeFromExpr (ProdExpr ann exprlist) = removeExtraTypeFromOneExpr (ProdExpr ann (removeExtraTypeFromExprList exprlist))
            removeExtraTypeFromExpr (PowerExpr ann expr) = removeExtraTypeFromOneExpr (PowerExpr ann (removeExtraTypeFromOneExpr expr))
--            removeExtraTypeFromExpr (FuncApplExpr ann app) = FuncApplExpr ann app
            removeExtraTypeFromExpr (ApplExpr ann expr1 expr2) = removeExtraTypeFromOneExpr (ApplExpr ann (removeExtraTypeFromOneExpr expr1) (removeExtraTypeFromOneExpr expr2))
            removeExtraTypeFromExpr (DecorExpr ann expr decor) = removeExtraTypeFromOneExpr (DecorExpr ann (removeExtraTypeFromOneExpr expr) decor)
            removeExtraTypeFromExpr (RenameExpr ann expr namepairlist) = removeExtraTypeFromOneExpr (RenameExpr ann (removeExtraTypeFromOneExpr expr) namepairlist)
            removeExtraTypeFromExpr (BindSelExpr ann expr refname) = removeExtraTypeFromOneExpr (BindSelExpr ann (removeExtraTypeFromOneExpr expr) refname)
            removeExtraTypeFromExpr (BindSelExprA ann expr name) = removeExtraTypeFromOneExpr (BindSelExprA ann (removeExtraTypeFromOneExpr expr) name)
            removeExtraTypeFromExpr (TupleSelExpr ann expr numeral) = removeExtraTypeFromOneExpr (TupleSelExpr ann (removeExtraTypeFromOneExpr expr) numeral)
            removeExtraTypeFromExpr (ThetaExpr ann expr strokelist) = removeExtraTypeFromOneExpr (ThetaExpr ann (removeExtraTypeFromOneExpr expr) strokelist)
            removeExtraTypeFromExpr (GenRefExpr ann refname exprlist) = removeExtraTypeFromOneExpr (GenRefExpr ann refname (removeExtraTypeFromExprList exprlist))
            removeExtraTypeFromExpr (GenRefExprA ann name exprlist) = removeExtraTypeFromOneExpr (GenRefExprA ann name (removeExtraTypeFromExprList exprlist))
            removeExtraTypeFromExpr (SetExpr ann exprlist) = removeExtraTypeFromOneExpr (SetExpr ann (removeExtraTypeFromExprList exprlist))
            removeExtraTypeFromExpr (SetCompExpr ann schematext expr) = removeExtraTypeFromOneExpr (SetCompExpr ann schematext (removeExtraTypeFromOneExpr expr))
            removeExtraTypeFromExpr (SetCompExprA ann e' expr) = removeExtraTypeFromOneExpr (SetCompExprA ann (removeExtraTypeFromOneExpr e') (removeExtraTypeFromOneExpr expr))
            removeExtraTypeFromExpr (VarConsExpr ann name e) = removeExtraTypeFromOneExpr (VarConsExpr ann name (removeExtraTypeFromOneExpr e))
            removeExtraTypeFromExpr (SchemaConsExpr ann ex p) = removeExtraTypeFromOneExpr (SchemaConsExpr ann (removeExtraTypeFromOneExpr ex) p)
-- ???
--            removeExtraTypeFromExpr (BindExtExprA ann e') = removeExtraTypeFromOneExpr (BindExtExprA ann (removeExtraTypeFromOneExpr e'))
            removeExtraTypeFromExpr (TupleExtExpr ann expr1 exprlist) = removeExtraTypeFromOneExpr (TupleExtExpr ann (removeExtraTypeFromOneExpr expr1) (removeExtraTypeFromExprList exprlist))
            removeExtraTypeFromExpr expr = removeExtraTypeFromOneExpr expr 
---------------- removeExtraTypeFromPred 
            -- remove the extra TypeAnn ("a") from predicate 
            removeExtraTypeFromPred (MemPred ap e1 e2) = MemPred ap (removeExtraTypeFromExpr e1) (removeExtraTypeFromExpr e2)
            removeExtraTypeFromPred (AndPred ap p1 p2) = AndPred ap (removeExtraTypeFromPred p1) (removeExtraTypeFromPred p2)
            -- the second expr (e2) of MemPred1 shall have the same type as the 
            --  first expr (e2) of MemPred2
            -- e1 ip1 e2 ip2 e3 => e1 ip1 (e2 : a1) /\ (e2 : a1) ip2 (e3 : a2)
            checkTypeUnify m1@(MemPred a1 e11 e12) m2@(MemPred a2 e21 e22) = do
                case extractSecondConstrainedType m1 of
                    ("", _) -> return False 
                    (v, t2) -> case extractFirstConstrainedType m2 of 
                                    ("", _) -> return False 
                                    (v, t1) -> unify t2 t1
            checkTypeUnify m1@(MemPred a1 e1 e2) (AndPred a2 p1 p2) = do
                r <- checkTypeUnify m1 p1
                case r of
                    True -> checkTypeUnify p1 p2
                    False -> return False 
            extractFirstConstrainedType (MemPred a e1 (SetExpr sa [e2])) = extractConstrained (getAnnTerm e1)    
            extractFirstConstrainedType (MemPred a (TupleExtExpr ta e1 [e2]) (RefExprA a1 op)) = extractConstrained (getAnnTerm e1)    
            extractFirstConstrainedType (MemPred a e1 e2) = extractConstrained (getAnnTerm e1)
            extractSecondConstrainedType (MemPred a e1 (SetExpr sa [e2])) = extractConstrained (getAnnTerm e2)    
            extractSecondConstrainedType (MemPred a (TupleExtExpr ta e1 [e2]) (RefExprA a1 op)) = extractConstrained (getAnnTerm e2)    
            extractSecondConstrainedType (MemPred a e1 e2) = extractConstrained (getAnnTerm e2)
            -- extract constrained type variable ("a") and its real type 
            extractConstrained :: [Ann] -> (String, Type2)
            extractConstrained ann = case isAConstrainedType ann of
                                        (True, v) -> (v, getTypeAnn ann)
                                        (False, _) -> ("", TypeVar "")
                where getTypeAnn [] = TypeVar ""
                      getTypeAnn (x:xs) = case x of
                                            TypeAnn (Type (TypeVar v)) -> if isPrefixOf "a" v
                                                                          then getTypeAnn xs 
                                                                          else TypeVar v
                                            TypeAnn t -> typeToType2 t
                                            _         -> getTypeAnn xs
            -- a constrained type having (TypeAnn (Type (TypeVar v))) where v is "a[1-9]" (such as a1, a2, etc.)
            isAConstrainedType :: [Ann] -> (Bool, String)
            isAConstrainedType [] = (False, "")
            isAConstrainedType (x:xs) = case x of
                                            (TypeAnn (Type (TypeVar v))) -> if isPrefixOf "a" v
                                                                            then (True, v)
                                                                            else isAConstrainedType xs
                                            _                            -> isAConstrainedType xs
    -- 13.2.5.5
    ForallPredA ann expr p -> do 
        e1 <- traverseT expr; case e1 of
            Left err -> return (Left err)
            Right e1' -> case (extractType (getAnnTerm e1')) of -- check if e1's type annotation is powerset of schema type
                  Type (PowerType (SchType sig)) -> do 
                        env <- getEnvTypeC
                        setEnvTypeC (Map.union env sig)
                        p1 <- traverseT p; case p1 of
                            Left err -> return (Left err)
                            Right p1' -> do 
                                setEnvTypeC env -- restore env
                                return (Right (ForallPredA ann e1' p1'))
                  _                              -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The expression in the universal quantification predicate is not a powerset of a schema type") ann)))))
    -- 13.2.5.6
    Exists1PredA ann expr p -> do 
        e1 <- traverseT expr; case e1 of
            Left err -> return (Left err)
            Right e1' -> case (extractType (getAnnTerm e1')) of -- check if e1's type annotation is powerset of schema type
                  Type (PowerType (SchType sig)) -> do 
                        env <- getEnvTypeC
                        setEnvTypeC (Map.union env sig)
                        p1 <- traverseT p; case p1 of
                            Left err -> return (Left err)
                            Right p1' -> do 
                                setEnvTypeC env -- restore env
                                return (Right (Exists1PredA ann e1' p1'))
                  _                              -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The expression in the unique existential quantification predicate is not a powerset of a schema type") ann)))))
--
  traverseTImplInst e = case e of
    MemPred ann expr1 expr2 -> do
        e1 <- traverseTImplInst expr1 
        e2 <- traverseTImplInst expr2
        return (MemPred ann e1 e2)
    TruePred ann -> return e 
    NegPred ann p -> do 
        p' <- traverseTImplInst p 
        return (NegPred ann p')
    AndPred ann p1 p2 -> do 
        p1' <- traverseTImplInst p1
        p2' <- traverseTImplInst p2
        return (AndPred ann p1' p2')
    ForallPredA ann expr p -> do 
        e' <- traverseTImplInst expr
        p' <- traverseTImplInst p
        return (ForallPredA ann e' p')
    Exists1PredA ann expr p -> do 
        e' <- traverseTImplInst expr
        p' <- traverseTImplInst p
        return (Exists1PredA ann e' p')

instance TypeInferTraverse Expression where
 traverseT e = case e of
    -- according to 12.2.6.9, it should be transformed to number_literal_0 and number_literal_1,
    -- but we use number system to represent it directly
    -- the type of number can be derived from prelude, here we just specify it explicitly
    NumExpr ann name -> return (Right (NumExpr ((TypeAnn (Type (GIVEN "\x01D538"))) : ann) name))
    -- 13.2.6.1
    RefExprA ann name -> do env <- getEnvTypeC 
                            if isPrefixOf zChar_delta name -- Delta i
                            then do
                                e' <- traverseT (transformSchema (synTraverse2Tree (SchemaTextDecl ann (DeclPart ann [(ExprDecl ann (RefExpr ann (RName ann (tail name)))), (ExprDecl ann (DecorExpr ann (RefExpr ann (RName ann (tail name))) zChar_next))]))));
                                 case e' of
                                    Left err -> return (Left err)
                                    Right s -> return (Right (RefExprA (TypeAnn (extractType (getAnnTerm s)): ann) name))  
--                            then if Map.member (tail name) env
--                                 -- the name i has been in type environment
--                                 then return (Right e) -- ???
--                                 -- the name i is not in type environment 
--                                 -- then it is transformed to a schema construction expression 
--                                 else  do e' <- traverseT (transformSchema (synTraverse2Tree (SchemaTextDecl ann (DeclPart ann [(ExprDecl ann (RefExpr ann (RName ann (tail name)))), (ExprDecl ann (DecorExpr ann (RefExpr ann (RName ann (tail name))) "ʹ"))]))))
--                                          case e' of
--                                            Left err -> return (Left err)
--                                            Right s -> return (Right (RefExprA (TypeAnn (extractType (getAnnTerm s)): ann) name))  
                            else if isPrefixOf zChar_xi name    -- Xi i
                                 then do e' <- traverseT (transformSchema (synTraverse2Tree (SchemaText ann (DeclPart ann [(ExprDecl ann (RefExpr ann (RName ann (tail name)))), (ExprDecl ann (DecorExpr ann (RefExpr ann (RName ann (tail name))) zChar_next))]) (RelPred ann (InfixRel ann (InInfixRel ann (ThetaExpr ann (RefExpr ann (RName ann (tail name))) []) [(IP_E ann "=" (ThetaExpr ann (RefExpr ann (RName ann (tail name))) [zChar_next]))])))))); 
                                         case e' of
                                            Left err -> return (Left err)
                                            Right s -> return (Right (RefExprA (TypeAnn (extractType (getAnnTerm s)): ann) name))  
--                                 then if Map.member (tail name) env
--                                 -- the name i has been in type environment
--                                      then return (Right e) -- ???
--                                 -- the name i is not in type environment 
--                                 -- then it is transformed to a schema construction expression 
--                                      else  do e' <- traverseT (transformSchema (synTraverse2Tree (SchemaText ann (DeclPart ann [(ExprDecl ann (RefExpr ann (RName ann (tail name)))), (ExprDecl ann (DecorExpr ann (RefExpr ann (RName ann (tail name))) "ʹ"))]) (RelPred ann (InfixRel ann (InInfixRel ann (ThetaExpr ann (RefExpr ann (RName ann (tail name))) []) [(IP_E ann "=" (ThetaExpr ann (RefExpr ann (RName ann (tail name))) ["ʹ"]))])))))); 
--                                                 case e' of
--                                                    Left err -> return (Left err)
--                                                    Right s -> return (Right (RefExprA (TypeAnn (extractType (getAnnTerm s)): ann) name))  
                                         
                                 else case Map.lookup name env of
                                        Nothing -> return (Left (err2Msg (ErrTypeCheck (ETypeCUndefName (assembleError ("The reference to the name [" ++ name ++ "] can not be found") ann)))))
                                        Just t -> case t of
                                                    -- generic type
                                                    (GenType listname t2)       -> do       
                                                        -- allocate a list of type variables to correspond to the list of generic names
                                                        -- for example: "name [X, Y]" will get two type variables (such as "α1" and "α2")
                                                        (tv, tl) <- genDistinctTypeVar listname 
                                                        -- uninstantiated generic type, instantiated type
                                                        -- GenType2 [X] (P(GENTYPE X)), (([X] P(GENTYPE X)) ["α1"])
                                                        -- after determination of all type variables via constraints, this RefExprA should be transformed to
                                                        -- a generic instantiation expression by the rule in 13.2.3.3, but this new generic
                                                        -- instantiation expression has been annotated with carrier in 13.2.3.2, and so it's
                                                        -- not necessary to visit it again 
                                                        return (Right (RefExprA (TypeVarAnn tv : (TypeAnn (GenType2 listname t2 (genTypeInst t tl)): ann)) name)) 
                                                    _                           -> return (Right (RefExprA (TypeAnn t: ann) name)) 
        where genDistinctTypeVar [x] = do v <- allocAddANewTypeVar; return ([v], [(TypeVar v)])
              genDistinctTypeVar (x:xs) = do 
                (lv, lx) <- genDistinctTypeVar [x]
                (lvs, lxs) <- genDistinctTypeVar xs
                return (lv ++ lvs, lx ++ lxs)
-- 13.2.6.2 
    GenRefExprA ann name exprlist -> do 
            env <- getEnvTypeC 
            if isPrefixOf zChar_delta name -- Delta i
            then do e' <- traverseT (transformSchema (synTraverse2Tree (SchemaTextDecl ann (DeclPart ann [(ExprDecl ann (GenRefExpr ann (RName ann (tail name)) exprlist)), (ExprDecl ann (DecorExpr ann (GenRefExpr ann (RName ann (tail name)) exprlist) zChar_next))]))));
                    case e' of
                       Left err -> return (Left err)
                       Right s -> return (Right (GenRefExprA (TypeAnn (extractType (getAnnTerm s)): ann) name exprlist))  
            else if isPrefixOf zChar_xi name    -- Xi i
                then do e' <- traverseT (transformSchema (synTraverse2Tree (SchemaText ann (DeclPart ann [(ExprDecl ann (GenRefExpr ann (RName ann (tail name)) exprlist)), (ExprDecl ann (DecorExpr ann (GenRefExpr ann (RName ann (tail name)) exprlist) zChar_next))]) (RelPred ann (InfixRel ann (InInfixRel ann (ThetaExpr ann (GenRefExpr ann (RName ann (tail name)) exprlist) []) [(IP_E ann "=" (ThetaExpr ann (GenRefExpr ann (RName ann (tail name)) exprlist) [zChar_next]))])))))); 
                        case e' of
                           Left err -> return (Left err)
                           Right s -> return (Right (GenRefExprA (TypeAnn (extractType (getAnnTerm s)): ann) name exprlist))  
                else case Map.lookup name env of
                    Nothing -> return (Left (err2Msg (ErrTypeCheck (ETypeCUndefName (assembleError ("The reference to the name [" ++ name ++ "] can not be found" ) ann)))))
                    Just t  -> case t of 
                        GenType formals t2  -> if (length formals) /= (length exprlist) 
                                                then return (Left (err2Msg (ErrTypeCheck (ETypeCMismatchNoOfPara (assembleError ("The number of actual parameters (" ++ show (length exprlist) ++ ") to [" ++ name ++ "] in a generic instantiation expression is not equal to expected number of formals (" ++ show (length formals) ++ ")" ) ann)))))
                                                else do el <- traverseT exprlist
                                                        case el of
                                                            Left err -> return (Left err)
                                                            Right el' -> case isAllSet el' of 
                                                                            Left err' -> return (Left err')
                                                                            Right True -> return (Right (GenRefExprA ((TypeAnn (Type (genTypeInst t (extTypeList el')))):ann) name el') )
                        _                   -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The reference [" ++ name ++ "] in a generic instantiation expression is not a generic type" ) ann)))))
        where isSetType (Type (PowerType _ )) = True 
              isSetType _                     = False
              isAllSet [x]  = case (isSetType (extractType (getAnnTerm x))) of
                                    False -> Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The type of the expression [" ++ show x ++ "] is not a set") (getAnnTerm x)))))
                                    True -> Right True
              isAllSet (x:xs) = case (isAllSet [x]) of 
                                    Left err -> Left err
                                    Right True -> (isAllSet xs)
              extType (Type (PowerType a)) = a
--              extType (Type (PowerType a)) = (PowerType a)
              extTypeList [] = [] 
              extTypeList (x:xs) = (extType (extractType (getAnnTerm x))) : (extTypeList xs)
    -- 13.2.6.3 Set extension expression
    SetExpr ann exprlist -> if null exprlist 
                            then do v <- allocAddANewTypeVar 
                                    return (Right (addAnnTerm (TypeAnn (Type (PowerType (TypeVar v)))) e))
                            else do el <- traverseT exprlist
                                    case el of
                                        Left err -> return (Left err)
                                        Right el' -> do r <- isTypeEqual el' (head el') 
                                                        if r
                                                        then return (Right (SetExpr ((TypeAnn (Type (PowerType (getType2FromTerm (head el'))))): ann) el')) 
                                                        else return (Left (err2Msg (ErrTypeCheck (ETypeCNotAllSameType (assembleError ("Not all expressions in set extension expression have the same type") ann) el'))))
         where isTypeEqual [] t     = return True 
               isTypeEqual [x] t    = do r <- unify (getType2FromTerm x) (getType2FromTerm t)  
                                         return r
               isTypeEqual (x:xs) t = do r <- unify (getType2FromTerm x) (getType2FromTerm t) 
                                         if r
                                         then isTypeEqual xs t 
                                         else return False 
    -- 13.2.6.4
    SetCompExprA ann expr1 expr2 -> do 
        e1 <- traverseT expr1; case e1 of
            Left err -> return (Left err)
            Right e1' -> case (extractType (getAnnTerm e1')) of -- check if e1's type annotation is powerset of schema type
                  Type (PowerType (SchType sig)) -> do 
                        env <- getEnvTypeC
                        -- need to replace all type variables by their instantiated one
                        r <- replaceAllTypeVarsInAType2 (SchType sig)
                        case r of
                            Left err -> return (Left err)
                            Right (SchType sig') -> do 
                                setEnvTypeC (Map.union env sig')
                                e2 <- traverseT expr2; case e2 of
                                    Left err -> return (Left err)
                                    Right e2' -> do 
                                        setEnvTypeC env -- restore env
                                        return (Right (SetCompExprA ((TypeAnn (Type (PowerType (getType2FromTerm e2')))) : ann) e1' e2'))
                  _                              -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The expression in the set comprehension expression is not powerset of a schema type") (getAnnTerm e1'))))))
    -- 13.2.6.5
    PowerExpr ann expr -> do
        e1 <- traverseT expr; case e1 of
            Left err -> return (Left err)
            Right e1' -> case (extractType (getAnnTerm e1')) of -- check if e1's type annotation is powerset
                Type (PowerType _)  -> return (Right (PowerExpr (addAnnToAnnListHelper (TypeAnn (Type (PowerType (getType2FromTerm e1')))) ann) e1')) 
                _                   -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The expression in the powerset expression is not a set") (getAnnTerm e1'))))))
    -- 13.2.6.6
    TupleExtExpr ann expr exprlist -> do
        e1 <- traverseT expr; case e1 of
            Left err -> return (Left err)
            Right e1' -> do 
                el <- traverseT exprlist; case el of
                    Left err -> return (Left err)
                    Right el' -> return $ Right $ TupleExtExpr (addAnnToAnnListHelper (TypeAnn (Type (ProdType ((getType2FromTerm e1') : (exprListToType2List el'))))) ann) e1' el'
        where exprListToType2List [] = []
              exprListToType2List (x:xs) = (getType2FromTerm x) : (exprListToType2List xs)
    -- 13.2.6.7
    TupleSelExpr ann expr num -> do
        e1 <- traverseT expr; case e1 of 
            Left err -> return (Left err)
            Right e1' -> case (extractType (getAnnTerm e1')) of -- check if e1's type annotation is the product type
                  Type (ProdType tl)    -> let n = (read num) :: Int
                                           in if n > (length tl)
                                              then return (Left (err2Msg (ErrTypeCheck (ETypeCIdxOutOfRange (assembleError ("The index [" ++ num ++ "] to select from the tuple selection expression is out of range [1," ++ show (length tl) ++ "]") ann)))))
                                              else return (Right (TupleSelExpr ((TypeAnn (Type (tl!!(n-1)))): ann) e1' num)) 
                  _                     -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The expression in the tuple selection expression is not a Cartesian product type") ann)))))
    -- 13.2.6.8                
    BindExtExprA ann nameexprpairs -> if not (null (returnDuplicates (getNames nameexprpairs)))
                                      then return (Left (err2Msg (ErrTypeCheck (ETypeCDupName (assembleError ("[" ++ show (returnDuplicates (getNames nameexprpairs)) ++ "] found in the binding extension expression") ann)))))
--                                      else return (Left "") 
                                      else do r <- traversePairs nameexprpairs 
                                              case r of
                                                    Left err -> return (Left err)
                                                    Right (el, tl) -> return (Right (BindExtExprA ((TypeAnn (Type (SchType (Map.fromList tl)))) : ann) (el) ))
        where getNames [] = []
              getNames ((x,y):xs) = [x] ++ (getNames xs)
              -- return [(i, e'), (i, t')]
              traversePairs [] = return (Right ([], []))
              traversePairs ((x,y):xs) = do 
                    e1 <- traverseT y; case e1 of 
                          Left err -> return (Left err)
                          Right e1' -> do 
                                el <- traversePairs xs; case el of
                                      Left err -> return (Left err)
                                      Right (el', tl')  -> return (Right ((x, e1') : el', (x, (extractType (getAnnTerm e1'))) : tl'))
    -- 13.2.6.9
    ThetaExpr ann expr strokelist -> do 
        e1 <- traverseT expr; case e1 of
            Left err -> return (Left err)
            Right e1' -> case (extractType (getAnnTerm e1')) of
                Type (PowerType (SchType sig))  -> do 
                    env <- getEnvTypeC
                    case checkAllInEnv (Map.toList sig) (Map.toList env) strokelist of
                        Left err -> return (Left err) 
                        Right True -> return (Right (ThetaExpr ((TypeAnn (Type (SchType sig))) : ann) e1' strokelist)) 
                _                               -> do
                    return (Left (err2Msg (ErrTypeCheck (ETypeCNotSchType (assembleError ("The type annotation of the expression in the binding construction expression is not a schema type") ann)))))
        -- checkAllInEnv siglist envlist strokelist 
        where checkAllInEnv [] envlist strokelist = Right True
              checkAllInEnv ((i, t):st) envlist strokelist = case t of  
                    (GenType _ _)   -> Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The type of the variable [" ++ i ++ "] should not be a generic type in the binding construction expression") ann))))
                    _               -> if not (((i ++ (listStroke strokelist)), t) `elem` envlist)
                                       then Left (err2Msg (ErrTypeCheck (ETypeCUndefName (assembleError ("The name [(" ++ (i ++ (listStroke strokelist)) ++ ", " ++ (show t) ++ ")] \n\t" ++ (show envlist) ++ "" ++ showPos ann ++ " is not defined in the environment") ann))))
                                       else checkAllInEnv st envlist strokelist 
              listStroke [] = "" 
              listStroke (x:xs) = x ++ (listStroke xs)
    -- 13.2.6.10
    BindSelExprA ann expr name -> do
        e1 <- traverseT expr; case e1 of
            Left err -> return (Left err)
            Right e1' -> case (extractType (getAnnTerm e1')) of
                Type (SchType sig)  -> case Map.lookup name sig of
                    Just t -> return (Right (BindSelExprA ((TypeAnn t) : ann) e1' name)) 
                    Nothing -> return (Left (err2Msg (ErrTypeCheck (ETypeCUndefName (assembleError ("The name [" ++ name ++ "] to be selected in the binding selection expression doesn't exist") ann)))))
                _                   -> do 
                    return (Left (err2Msg (ErrTypeCheck (ETypeCNotSchType (assembleError ("The type annotation of the expression in the binding selection expression is not a schema type") ann)))))
    -- 13.2.6.11 
    ApplExpr ann expr1 expr2 -> do 
        e1 <- traverseT expr1
        e2 <- traverseT expr2
        case e1 of
            Left err -> return (Left err)
            Right e1' -> case e2 of
                Left err -> return (Left err)
                Right e2' -> case (extractType (getAnnTerm e1')) of
                    Type (PowerType (ProdType ts)) -> if not ((length ts) == 2) 
                                                      then return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The first expression in the application expression is a [" ++ show (length ts) ++ "]-tuple, instead of a pair expected") ann)))))
                                                      else do r <- unify (ts!!0) (getType2FromTerm e2')
                                                              if not r 
                                                              then return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The first type \n\t[" ++ (show (ts!!0)) ++ "] \nin the product type of the first expression \n\t[" ++ (show (e1')) ++ "] \nin the application expression is not equal to the type \n\t[" ++ (show (getType2FromTerm e2')) ++ "] \nof the second expression \n\t[" ++ (show (e2')) ++ "] \n\t") ann)))))
                                                              else return (Right (ApplExpr ((TypeAnn (Type (ts!!1))) : ann) e1' e2')) 
                    GenType2 listname (PowerType (ProdType ts1)) (PowerType (ProdType ts2)) -> if not (((length ts1) == 2) && ((length ts2) == 2))
                                                      then return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The first expression in the application expression is a [" ++ show (length ts1) ++ "]-tuple, instead of a pair expected") ann)))))
                                                      else do r <- unify (ts2!!0) (getType2FromTerm e2')
                                                              if not r 
                                                              then return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch2 (assembleError ("The first type \n\t[" ++ (show (ts2!!0)) ++ "] \nin the product type of the first expression \n\t[" ++ (show (e1')) ++ "] \nin the application expression is not equal to the type \n\t[" ++ (show (getType2FromTerm e2')) ++ "] \nof the second expression \n\t[" ++ (show (e2')) ++ "] ") ann)))))
                                                              else return (Right (ApplExpr ((TypeAnn (Type (ts2!!1))) : ann) e1' e2')) 
--                    t                              -> return (Left ((assembleError ("The first expression in the application expression is not a set of pair") ann) ++ "\n" ++ (show t)))
                    t                              -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch2 (assembleError ("The first expression in the application expression is not a set of pair" ++ "\ne1':" ++ (show e1') ++ "\ne2':" ++ (show e2') ++ "\n") ann)))))
    -- 13.2.6.12 Definite description expression
    MuExprA ann expr1 expr2 -> do 
        e1 <- traverseT expr1; case e1 of
            Left err -> return (Left err)
            Right e1' -> case (extractType (getAnnTerm e1')) of -- check if e1's type annotation is powerset of schema type
                  Type (PowerType (SchType sig)) -> do 
                        env <- getEnvTypeC
                        -- need to replace all type variables by their instantiated one
                        r <- replaceAllTypeVarsInAType2 (SchType sig)
                        case r of
                            Left err -> return (Left err)
                            Right (SchType sig') -> do 
                                setEnvTypeC (Map.union env sig')
                                e2 <- traverseT expr2; case e2 of
                                    Left err -> return (Left err)
                                    Right e2' -> do 
                                        setEnvTypeC env -- restore env
                                        return (Right (MuExprA ((TypeAnn (Type (getType2FromTerm e2'))) : ann) e1' e2'))
                  _                              -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The expression in the mu expression is not a powerset of a schema type") (getAnnTerm e1'))))))
    -- 13.2.6.13
    VarConsExpr ann name expr -> do 
        e' <- traverseT expr; case e' of
            Left err -> return (Left err)
            Right e'' -> case (extractType (getAnnTerm e'')) of
                           Type (PowerType t)   -> return (Right (VarConsExpr ((TypeAnn (Type (PowerType (SchType (Map.fromList [(name, Type t)]))))): ann) name e''))
                           _                    -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("In variable construction expression, the type of the expression for the variable [" ++ name ++ "] is not a set type") ann)))))
    -- 13.2.6.14 Schema construction expression 
    SchemaConsExpr ann expr pred -> do
        e1 <- traverseT expr; case e1 of
            Left err -> return (Left err)
            Right e1' -> case (extractType (getAnnTerm e1')) of -- check if e1's type annotation is powerset of schema type
                  Type (PowerType (SchType sig)) -> do 
                        env <- getEnvTypeC
                        -- need to replace all type variables by their instantiated one
                        r <- replaceAllTypeVarsInAType2 (SchType sig)
                        case r of
                            Left err -> return (Left err)
                            Right (SchType sig') -> do 
                                setEnvTypeC (Map.union env sig')
                                p <- traverseT pred; case p of
                                    Left err -> return (Left err)
                                    Right p' -> do 
                                        setEnvTypeC env -- restore env
                                        return (Right (SchemaConsExpr ((TypeAnn (extractType (getAnnTerm e1'))) : ann) e1' p'))
                  _                              -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The expression in the schema construction expression is not a powerset of a schema type") (getAnnTerm e1'))))))
    -- 13.2.6.15 Schema negation expression
    NegExpr ann expr -> do
        e1 <- traverseT expr; case e1 of
            Left err -> return (Left err)
            Right e1' -> case (extractType (getAnnTerm e1')) of -- check if e1's type annotation is powerset of schema type
                  Type (PowerType (SchType sig)) -> do 
                    return (Right (NegExpr ((TypeAnn (extractType (getAnnTerm e1'))) : ann) e1'))
                  _                              -> do
                    return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The expression in the schema negation expression is not a powerset of a schema type") ann)))))
    --  13.2.6.16 Schema conjunction expression
    AndExpr ann expr1 expr2 -> do 
        e1 <- traverseT expr1; e2 <- traverseT expr2; case e1 of
            Left err -> return (Left err)
            Right e1' -> case (extractType (getAnnTerm e1')) of -- check if e1's type annotation is powerset of schema type
                Type (PowerType (SchType sig1)) -> case e2 of 
                    Left err -> return (Left err)
                    Right e2' -> case (extractType (getAnnTerm e2')) of -- check if e2's type annotation is powerset of schema type
                        Type (PowerType (SchType sig2)) -> do
                            -- compatible relations [e1 ≈ e2] is equal to [(dom e2)▹e1 = (dom e1)▹e2]
                            if not (isCompatible sig1 sig2)
                            then return (Left (err2Msg (ErrTypeCheck (ETypeCSigIncomp (assembleError ("The signatures of two schema expressions in the schema conjunction expression are not incompatible\n\tSignature 1: " ++ (show sig1) ++ "\n\tSignature 2:" ++ (show sig2) ++ "\n") ann)))))
                            else return (Right (AndExpr ((TypeAnn (Type (PowerType (SchType (Map.union sig1 sig2))))) : ann) e1' e2'))
                        _                              -> do
                            return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The second expression \n\t" ++ (show e2') ++ "\nin the schema conjunction expression is not a powerset of a schema type") (getAnnTerm e2'))))))
                _                              -> do
                    return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The first expression \n\t" ++ (show e1') ++ "\nin the schema conjunction expression is not a powerset of a schema type") (getAnnTerm e1'))))))
    -- 13.2.6.17 Schema hiding expression 
    HideExprA ann expr names -> do 
        e1 <- traverseT expr; case e1 of
            Left err -> return (Left err)
            Right e1' -> case (extractType (getAnnTerm e1')) of -- check if e1's type annotation is powerset of schema type
                Type (PowerType (SchType sig)) -> let diff = (listDiff names (Map.keys sig))
                                                  in if not (null diff) 
                                                     then return (Left (err2Msg (ErrTypeCheck (ETypeCNameNotInSig (assembleError ("The names " ++ show diff ++ " to be hidden in the schema hiding expression are not in the schema's signature") ann)))))
                                                     else return (Right (HideExprA ((TypeAnn (Type (PowerType (SchType (dropList sig names))))) : ann) e1' names)) 
                _                              -> do
                    return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The expression \n\t" ++ (show e1') ++ "\nin the schema hiding expression is not a powerset of a schema type") ann)))))
    -- 13.2.6.18 Schema universal quantification expression
    ForallExprA ann expr1 expr2 -> do 
        e1 <- traverseT expr1; case e1 of
            Left err -> return (Left err)
            Right e1' -> case (extractType (getAnnTerm e1')) of -- check if e1's type annotation is powerset of schema type
                  Type (PowerType (SchType sig1)) -> do 
                        env <- getEnvTypeC
                        -- need to replace all type variables by their instantiated one
                        r <- replaceAllTypeVarsInAType2 (SchType sig1)
                        case r of
                            Left err -> return (Left err)
                            Right (SchType sig1') -> do 
                                setEnvTypeC (Map.union env sig1')
                                e2 <- traverseT expr2; case e2 of
                                    Left err -> return (Left err)
                                    Right e2' -> case (extractType (getAnnTerm e2')) of -- check if e2's type annotation is powerset of schema type
                                        Type (PowerType (SchType sig2)) -> 
                                            if not (isCompatible sig1 sig2)
                                            then return (Left (err2Msg (ErrTypeCheck (ETypeCSigIncomp (assembleError ("The signatures of two schema expressions in the universal quantification expression are not incompatible\n\tSignature 1: " ++ (show sig1) ++ "\n\tSignature 2:" ++ (show sig2) ++ "\n") ann)))))
                                            else do 
                                                setEnvTypeC env -- restore env
                                                return (Right (ForallExprA  ((TypeAnn (Type (PowerType (SchType (dropList sig2 (Map.keys sig1)))))) : ann) e1' e2'))
                                        _                              -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The second expression \n\t" ++ (show e2') ++ "\nin the universal quantification expression is not a powerset of a schema type") (getAnnTerm e2'))))))
                  _                              -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The first expression \n\t" ++ (show e1') ++ "\nin the universal quantification expression is not a powerset of a schema type") (getAnnTerm e1'))))))
    -- 13.2.6.19 Schema unique existential quantification expression
    Exists1ExprA ann expr1 expr2 -> do 
        e1 <- traverseT expr1; case e1 of
            Left err -> return (Left err)
            Right e1' -> case (extractType (getAnnTerm e1')) of -- check if e1's type annotation is powerset of schema type
                  Type (PowerType (SchType sig1)) -> do 
                        env <- getEnvTypeC
                        -- need to replace all type variables by their instantiated one
                        r <- replaceAllTypeVarsInAType2 (SchType sig1)
                        case r of
                            Left err -> return (Left err)
                            Right (SchType sig1') -> do 
                                setEnvTypeC (Map.union env sig1')
                                e2 <- traverseT expr2; case e2 of
                                    Left err -> return (Left err)
                                    Right e2' -> case (extractType (getAnnTerm e2')) of -- check if e2's type annotation is powerset of schema type
                                        Type (PowerType (SchType sig2)) -> 
                                            if not (isCompatible sig1 sig2)
                                            then return (Left (err2Msg (ErrTypeCheck (ETypeCSigIncomp (assembleError ("The signatures of two schema expressions in the unique existential quantification expression are not incompatible\n\tSignature 1: " ++ (show sig1) ++ "\n\tSignature 2:" ++ (show sig2) ++ "\n") (getAnnTerm e1'))))))
                                            else do 
                                                setEnvTypeC env -- restore env
                                                return (Right (Exists1ExprA ((TypeAnn (Type (PowerType (SchType (dropList sig2 (Map.keys sig1)))))) : ann) e1' e2'))
                                        _                              -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The second expression \n\t" ++ (show e2') ++ "\nin the unique existential quantification expression is not a powerset of a schema type") (getAnnTerm e2'))))))
                  _                              -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The first expression \n\t" ++ (show e1') ++ "\nin the unique existential quantification expression is not a powerset of a schema type") (getAnnTerm e1'))))))
    -- 13.2.6.20 Schema renaming expression 
    RenameExpr ann expr namepairlist -> do
        if not (null (dups))
        then return (Left (err2Msg (ErrTypeCheck (ETypeCDupName (assembleError ("[" ++ show dups ++ "] are not allowed in the schema renaming expression") ann)))))
        else do e1 <- traverseT expr; case e1 of
                    Left err    -> return (Left err)  
                    Right e1'   -> case (extractType (getAnnTerm e1')) of -- check if e1's type annotation is powerset of schema type
                        Type (PowerType (SchType sig)) -> do
                            if not (null (dups' sig)) 
                            then return (Left (err2Msg (ErrTypeCheck (ETypeCNameExistInSig (assembleError ("The new names [" ++ show (dups' sig) ++ "] have been in the signature of the schema in the schema renaming expression") ann)))))
                            else return (Right (RenameExpr ((TypeAnn (Type (PowerType (SchType (newSig sig))))): ann) e1' namepairlist)) 
                        _                              -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The expression in the schema renaming expression is not a powerset of a schema type") (getAnnTerm e1'))))))
        where renamePairToOldNameList [] = []
              renamePairToOldNameList ((RenamePairA _ oldname newname) : xs) = oldname : (renamePairToOldNameList xs)
              renamePairToList [] = []  
              renamePairToList ((RenamePairA _ oldname newname) : xs) = (oldname, newname) : (renamePairToList xs)
              -- updateSig [(i, t)] [(i',j)] = [if i == i' then (j, t) else (i, t)]
              updateSig [] namepairlist = []
              updateSig ((i,t):xs) namepairlist = (getNewNameTypePair (i,t) namepairlist) : (updateSig xs namepairlist)
              -- getNewNameTypePair (i, t) [(i',j)] = if i == i' then (j, t) else (i, t)
              getNewNameTypePair (i, t) [] = (i, t) 
              getNewNameTypePair (i, t) ((i', j) : xs) = if i == i' then (j, t) else getNewNameTypePair (i,t) xs 
              dups = returnDuplicates (renamePairToOldNameList namepairlist)
              newSig sig = Map.fromList (updateSig (Map.toList sig) (renamePairToList namepairlist))
              dups' sig = returnDuplicates (Map.keys (newSig sig))
    -- 13.2.6.21 Schema precondition expression 
    PreExpr ann expr -> do
        e1 <- traverseT expr; case e1 of
            Left err -> return (Left err)
            Right e1' -> case (extractType (getAnnTerm e1')) of -- check if e1's type annotation is powerset of schema type
                Type (PowerType (SchType sig)) -> do
                    return (Right (PreExpr ((TypeAnn (Type (PowerType (SchType (Map.fromList (removeDecorName (Map.toList sig))))))) : ann) e1')) 
                _                              -> do
                    return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The expression in the schema precondition expression is not a powerset of a schema type") ann)))))
        where removeDecorName [] = []
              removeDecorName ((i, t): xs) = if (isSuffixOf "ʹ" i) || (isSuffixOf "!" i)
                                             then removeDecorName xs
                                             else (i,t) : (removeDecorName xs)
    -- 13.2.6.22 Schema composition expression
    CompExpr ann expr1 expr2 -> do
        e1 <- traverseT expr1; case e1 of
            Left err -> return (Left err)
            Right e1' -> case (extractType (getAnnTerm e1')) of -- check if e1's type annotation is powerset of schema type 
                Type (PowerType (SchType sig1)) -> do 
                    e2 <- traverseT expr2; case e2 of 
                        Left err -> return (Left err)
                        Right e2' -> case (extractType (getAnnTerm e1')) of -- check if e2's type annotation is powerset of schema type
                            Type (PowerType (SchType sig2)) -> 
                                if not (isCompatible (beta3 sig1 sig2) (beta4 sig1 sig2))
                                then return (Left (err2Msg (ErrTypeCheck (ETypeCSigIncomp (assembleError ("The signatures of two schema expressions in the schema composition expression are not incompatible\n\tSignature 1: " ++ (show (beta3 sig1 sig2)) ++ "\n\tSignature 2:" ++ (show (beta4 sig1 sig2)) ++ "\n") ann)))))
                                else if not (isCompatible (beta5 sig1 (match (Map.keys sig1) (Map.toList sig2)) Map.empty) (beta6 sig2 (match (Map.keys sig1) (Map.toList sig2)) Map.empty))
                                     then return (Left (err2Msg (ErrTypeCheck (ETypeCSigIncomp (assembleError ("The signatures of two schema expressions in the schema composition expression are not incompatible\n\tSignature 1: " ++ (show (beta5 sig1 (match (Map.keys sig1) (Map.toList sig2)) Map.empty)) ++ "\n\tSignature 2:" ++ (show (beta6 sig2 (match (Map.keys sig1) (Map.toList sig2)) Map.empty)) ++ "\n") ann)))))
                                     else do return (Right (CompExpr ((TypeAnn (Type (PowerType (SchType (Map.union (beta3 sig1 sig2) (beta4 sig1 sig2)))))) : ann) e1' e2'))
                            _                              -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The second expression \n\t" ++ (show e2') ++ "\nin the schema composition expression is not a powerset of a schema type") (getAnnTerm e2'))))))
                _                              -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The first expression \n\t" ++ (show e1') ++ "\nin the schema composition expression is not a powerset of a schema type") (getAnnTerm e1'))))))
        where match dom1 [] = []
              match dom1 ((i, t):xs) = if (i ++ "ʹ") `elem` dom1 
                                       then i : (match dom1 xs) 
                                       else (match dom1 xs)
              nextDecorName [] = []
              nextDecorName (i:is) = (i ++ "ʹ") : (nextDecorName is)
              beta3 sig1 sig2 = dropList sig1 (nextDecorName (match (Map.keys sig1) (Map.toList sig2)))
              beta4 sig1 sig2 = dropList sig2 (match (Map.keys sig1) (Map.toList sig2))
              beta5 sig1 [] newsig = newsig
              beta5 sig1 (i:is) newsig = case Map.lookup (i ++ "ʹ") sig1 of
                                    Just t -> beta5 sig1 (is) (Map.insert i t newsig)
                                    Nothing -> beta5 sig1 (is) (newsig) 
              beta6 sig2 [] newsig = newsig
              beta6 sig2 (i:is) newsig = case Map.lookup i sig2 of
                                    Just t -> beta6 sig2 (is) (Map.insert i t newsig)
                                    Nothing -> beta6 sig2 (is) (newsig) 

    -- 13.2.6.23 Schema piping expression
    PipeExpr ann expr1 expr2 -> do
        e1 <- traverseT expr1; case e1 of
            Left err -> return (Left err)
            Right e1' -> case (extractType (getAnnTerm e1')) of -- check if e1's type annotation is powerset of schema type 
                Type (PowerType (SchType sig1)) -> do 
                    e2 <- traverseT expr2; case e2 of 
                        Left err -> return (Left err)
                        Right e2' -> case (extractType (getAnnTerm e1')) of -- check if e2's type annotation is powerset of schema type
                            Type (PowerType (SchType sig2)) -> 
                                if not (isCompatible (beta3 sig1 sig2) (beta4 sig1 sig2))
                                then return (Left (err2Msg (ErrTypeCheck (ETypeCSigIncomp (assembleError ("The signatures of two schema expressions in the schema piping expression are not incompatible\n\tSignature 1: " ++ (show (beta3 sig1 sig2)) ++ "\n\tSignature 2:" ++ (show (beta4 sig1 sig2)) ++ "\n") ann)))))
                                else if not (isCompatible (beta5 sig1 (match (Map.keys sig1) (Map.keys sig2)) Map.empty) (beta6 sig2 (match (Map.keys sig1) (Map.keys sig2)) Map.empty))
                                     then return (Left (err2Msg (ErrTypeCheck (ETypeCSigIncomp (assembleError ("The signatures of two schema expressions in the schema piping expression are not incompatible\n\tSignature 1: " ++ (show (beta5 sig1 (match (Map.keys sig1) (Map.keys sig2)) Map.empty)) ++ "\n\tSignature 2:" ++ (show (beta6 sig2 (match (Map.keys sig1) (Map.keys sig2)) Map.empty)) ++ "\n") ann)))))
                                     else do return (Right (PipeExpr ((TypeAnn (Type (PowerType (SchType (Map.union (beta3 sig1 sig2) (beta4 sig1 sig2)))))) : ann) e1' e2'))
                            _                              -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The second expression \n\t" ++ (show e2') ++ "\nin the schema piping expression is not a powerset of a schema type") (getAnnTerm e2'))))))
                _                              -> return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The first expression \n\t" ++ (show e1') ++ "\nin the schema piping expression is not a powerset of a schema type") (getAnnTerm e1'))))))
        where match dom1 [] = []
              match dom1 (i:is) = if (isSuffixOf "?" i) && (((init i) ++ "!") `elem` dom1)
                                       then (init i) : (match dom1 is) 
                                       else (match dom1 is)
              outDecorName [] = []
              outDecorName (i:is) = (i ++ "!") : (outDecorName is)
              inDecorName [] = []
              inDecorName (i:is) = (i ++ "?") : (inDecorName is)
              beta3 sig1 sig2 = dropList sig1 (outDecorName (match (Map.keys sig1) (Map.keys sig2)))
              beta4 sig1 sig2 = dropList sig2 (inDecorName (match (Map.keys sig1) (Map.keys sig2)))
              beta5 sig1 [] newsig = newsig
              beta5 sig1 (i:is) newsig = case Map.lookup (i ++ "!") sig1 of
                                    Just t -> beta5 sig1 (is) (Map.insert i t newsig)
                                    Nothing -> beta5 sig1 (is) (newsig) 
              beta6 sig2 [] newsig = newsig
              beta6 sig2 (i:is) newsig = case Map.lookup (i ++ "?") sig2 of
                                    Just t -> beta6 sig2 (is) (Map.insert i t newsig)
                                    Nothing -> beta6 sig2 (is) (newsig) 
    -- 13.2.6.24 Schema decoration expression
    DecorExpr ann expr stroke -> do
        e1 <- traverseT expr; case e1 of
            Left err -> return (Left err)
            Right e1' -> case (extractType (getAnnTerm e1')) of -- check if e1's type annotation is powerset of schema type
                Type (PowerType (SchType sig)) -> do
                    return (Right (PreExpr ((TypeAnn (Type (PowerType (SchType (Map.fromList (decorName (Map.toList sig) stroke)))))) : ann) e1')) 
                _                              -> do
                    return (Left (err2Msg (ErrTypeCheck (ETypeCTypeMismatch (assembleError ("The expression \n\t" ++ (show e1') ++"\nin the schema decoration expression is not a powerset of a schema type") ann)))))
        where decorName [] stroke = []
              decorName ((i, t): xs) stroke = (i ++ stroke, t) : (decorName xs stroke)
    --
    -- FuncApplExpr should be transformed in SynTransformerTwo, but we delay the transformation 
    -- easily transform it here since we have the information about function or generic templates
    FuncApplExpr ann app -> do 
                    m <- getOpNameFuncGen
                    p <- traverseT (appToExpr newapp m)
                    case p of
                         Left err -> return (Left (err ++ " in appToExpr [" ++ (show app) ++ "]"))
                         Right s -> return (Right s)
                -- if there is a TypeVar "a1" in FuncApplExpr's Anns, it should be passed to final ApplExpr
        where   newapp = case (isAConstrainedType ann) of
                            (True, x)   -> addAnnTerm x app
                            (False, _)  -> app
                isAConstrainedType :: [Ann] -> (Bool, Ann)
                isAConstrainedType [] = (False, (TypeAnn (Type (TypeVar ""))))
                isAConstrainedType (x:xs) = case x of
                                            (TypeAnn (Type (TypeVar v))) -> if isPrefixOf "a" v
                                                                            then (True, x)
                                                                            else isAConstrainedType xs
                                            _                            -> isAConstrainedType xs
  -- 
 traverseTImplInst e = case e of
    NumExpr ann name -> return e 
    -- 13.2.3.3 Implicit instantiation 
    -- 1. check if the type annotation is a GenType2 
    -- 2. determine the type variables used in the type annotation and replaces them with their instantiated types
    -- 3. transform this reference to a generic reference instantiation by 13.2.6.2 
    RefExprA ann name -> 
        if b then
            case t of
            GenType2 formals gentype insttype   -> do
                ret <- replaceAllTypeVarsInAType2 insttype
                case ret of
                    Right t2 -> do
                        exprlist <- (getGenInstExprList varlist)
                        return (GenRefExprA (addAnnToAnnListHelper (TypeAnn (GenType2 formals gentype t2)) newann) name exprlist) 
                        -- new ann: (addAnnToAnnListHelper (TypeAnn (GenType2 formals gentype t2)) newann)
                    Left err -> return e 
            _                                   -> return e
        else return e
      where (b, t) = hasAndExtractType ann
            -- remove the additional Ann (TypeVarAnn)
            (varlist, newann) = getAndRemoveTypeVarAnn ann []
            -- getGenInstExprList :: [String] -> [Expression]
            getGenInstExprList [] = return [] 
            getGenInstExprList (x:xs) = do
                (b, tx) <- getTypeVarType2 x 
                txs <- getGenInstExprList xs 
                return ((carrier tx) : txs)

    GenRefExprA ann name exprlist -> do
        ee <- traverseTImplInst exprlist 
        ann <- updateTypeVarInTypeAnn ann 
        return (GenRefExprA ann name ee)
    SetExpr ann exprlist -> do 
        ee <- traverseTImplInst exprlist 
        ann <- updateTypeVarInTypeAnn ann 
        return (SetExpr ann ee)
    -- 13.2.6.4
    SetCompExprA ann expr1 expr2 -> do 
        e1 <- traverseTImplInst expr1
        e2 <- traverseTImplInst expr2
        ann <- updateTypeVarInTypeAnn ann 
        return (SetCompExprA ann e1 e2)
    -- 13.2.6.5
    PowerExpr ann expr -> do
        e1 <- traverseTImplInst expr
        ann <- updateTypeVarInTypeAnn ann 
        return (PowerExpr ann e1)
    -- 13.2.6.6
    TupleExtExpr ann expr exprlist -> do
        e1 <- traverseTImplInst expr
        el <- traverseTImplInst exprlist
        ann <- updateTypeVarInTypeAnn ann 
        return (TupleExtExpr ann e1 el)
    -- 13.2.6.7
    TupleSelExpr ann expr num -> do
        e1 <- traverseTImplInst expr
        ann <- updateTypeVarInTypeAnn ann 
        return (TupleSelExpr ann e1 num)
    -- 13.2.6.8                
    BindExtExprA ann nameexprpairs -> do
        ne <- traverseNameExpr nameexprpairs  
        ann <- updateTypeVarInTypeAnn ann 
        return (BindExtExprA ann ne)
      where traverseNameExpr [] = return [] 
            traverseNameExpr ((n, expr):xs) = do
                e1 <- traverseTImplInst expr
                l <- (traverseNameExpr xs)
                return ((n, e1): l)
    -- 13.2.6.9
    ThetaExpr ann expr strokelist -> do 
        e1 <- traverseTImplInst expr
        ann <- updateTypeVarInTypeAnn ann 
        return (ThetaExpr ann e1 strokelist)
    -- 13.2.6.10
    BindSelExprA ann expr name -> do
        e1 <- traverseTImplInst expr
        ann <- updateTypeVarInTypeAnn ann 
        return (BindSelExprA ann e1 name)
    -- 13.2.6.11 
    ApplExpr ann expr1 expr2 -> do 
        e1 <- traverseTImplInst expr1
        e2 <- traverseTImplInst expr2
        ann <- updateTypeVarInTypeAnn ann 
        return (ApplExpr ann e1 e2)
    -- 13.2.6.12 Definite description expression
    MuExprA ann expr1 expr2 -> do 
        e1 <- traverseTImplInst expr1
        e2 <- traverseTImplInst expr2
        ann <- updateTypeVarInTypeAnn ann 
        return (MuExprA ann e1 e2)
    -- 13.2.6.13
    VarConsExpr ann name expr -> do 
        e1 <- traverseTImplInst expr
        ann <- updateTypeVarInTypeAnn ann 
        return (VarConsExpr ann name e1)
    -- 13.2.6.14 Schema construction expression 
    SchemaConsExpr ann expr pred -> do
        e1 <- traverseTImplInst expr
        p <- traverseTImplInst pred
        ann <- updateTypeVarInTypeAnn ann 
        return (SchemaConsExpr ann e1 p)
    -- 13.2.6.15 Schema negation expression
    NegExpr ann expr -> do
        e1 <- traverseTImplInst expr
        ann <- updateTypeVarInTypeAnn ann 
        return (NegExpr ann e1)
    --  13.2.6.16 Schema conjunction expression
    AndExpr ann expr1 expr2 -> do 
        e1 <- traverseTImplInst expr1
        e2 <- traverseTImplInst expr2
        ann <- updateTypeVarInTypeAnn ann 
        return (AndExpr ann e1 e2)
    -- 13.2.6.17 Schema hiding expression 
    HideExprA ann expr names -> do 
        e1 <- traverseTImplInst expr
        ann <- updateTypeVarInTypeAnn ann 
        return (HideExprA ann e1 names)
    -- 13.2.6.18 Schema universal quantification expression
    ForallExprA ann expr1 expr2 -> do 
        e1 <- traverseTImplInst expr1
        e2 <- traverseTImplInst expr2
        ann <- updateTypeVarInTypeAnn ann 
        return (ForallExprA ann e1 e2)
    -- 13.2.6.19 Schema unique existential quantification expression
    Exists1ExprA ann expr1 expr2 -> do 
        e1 <- traverseTImplInst expr1
        e2 <- traverseTImplInst expr2
        ann <- updateTypeVarInTypeAnn ann 
        return (Exists1ExprA ann expr1 expr2)
    -- 13.2.6.20 Schema renaming expression 
    RenameExpr ann expr namepairlist -> do
        e1 <- traverseTImplInst expr
        ann <- updateTypeVarInTypeAnn ann 
        return (RenameExpr ann e1 namepairlist)
    -- 13.2.6.21 Schema precondition expression 
    PreExpr ann expr -> do
        e1 <- traverseTImplInst expr
        ann <- updateTypeVarInTypeAnn ann 
        return (PreExpr ann e1)
    -- 13.2.6.22 Schema composition expression
    CompExpr ann expr1 expr2 -> do
        e1 <- traverseTImplInst expr1
        e2 <- traverseTImplInst expr2
        ann <- updateTypeVarInTypeAnn ann 
        return (CompExpr ann expr1 expr2)
    -- 13.2.6.23 Schema piping expression
    PipeExpr ann expr1 expr2 -> do
        e1 <- traverseTImplInst expr1
        e2 <- traverseTImplInst expr2
        ann <- updateTypeVarInTypeAnn ann 
        return (PipeExpr ann expr1 expr2)
    -- 13.2.6.24 Schema decoration expression
    DecorExpr ann expr stroke -> do
        e1 <- traverseTImplInst expr
        ann <- updateTypeVarInTypeAnn ann 
        return (DecorExpr ann e1 stroke)
    FuncApplExpr ann app -> return e
    _       -> error ("Not supported " ++ (show e))

{-
 - Helper functions
 -
 -}

{- |
 The SectTypeEnv of the prelude section.
 -}
preludeSectTypeEnv :: SectTypeEnv 
preludeSectTypeEnv = SectTypeEnv (Map.fromList [
    ("\x01D538", ("prelude", Type (PowerType (GIVEN "𝔸")))), 
    ("\x2115", ("prelude", Type (PowerType (GIVEN "𝔸")))), 
    ("number_literal_0", ("prelude", Type (GIVEN "𝔸"))), 
    ("number_literal_1", ("prelude", Type (GIVEN "𝔸"))), 
    ("\x22C8+\x22C8", ("prelude", Type (PowerType (ProdType [ProdType [(GIVEN "𝔸"), (GIVEN "𝔸")], (GIVEN "𝔸")]))))
    ])

{- |
 Check if all names in the signatures of paragraphs are disjoint.
 -}
disjointNames :: [Paragraph] -> Either String Bool 
disjointNames [] = Right True 
disjointNames (x:xs) = disjointK (x:xs) []
    where disjointK [] keys = Right True
          disjointK (x:xs) keys = case inter of 
                    []  -> disjointK xs (keys ++ keysOfSig)
                    _   -> Left (err2Msg (ErrTypeCheck (ETypeCDupName (assembleError ("Redefinitions of [" ++ show inter ++ "] found. It is not allowed to have the same name in the paragraph level") (getAnnTerm x)))))
            where keysOfSig = (Map.keys (extractSign (getAnnTerm x)))
                  inter = (List.intersect keysOfSig keys)

{- |
 - Extract Signature from annotations.
 -}
extractSign :: [Ann] -> Sig
extractSign [] = Map.empty  
extractSign ((SigAnn s) : _) = s
extractSign (_ : xs) = extractSign xs

{- |
Check if the annotation contains type annotation. 

- If so, just return it. 
- Otherwise, return false.
 -}
hasAndExtractType :: [Ann] -> (Bool, Type)
hasAndExtractType ((TypeAnn (Type (TypeVar t))) : xs) = if isPrefixOf "a" t 
                                                  then hasAndExtractType xs
                                                  else (True, Type (TypeVar t))
hasAndExtractType ((TypeAnn s) : _) = (True, s)
hasAndExtractType (_ : xs) = hasAndExtractType xs
hasAndExtractType ann = (False, Type (TypeVar ""))  

{- |
Extract type from annotations.
 -}
extractType :: [Ann] -> Type
--extractType [] = Map.empty  
extractType ((TypeAnn (Type (TypeVar t))) : xs) = if isPrefixOf "a" t 
                                                  then extractType xs
                                                  else Type (TypeVar t) 
extractType ((TypeAnn s) : _) = s
extractType (_ : xs) = extractType xs
extractType ann = error ("No TypeAnn in " ++ (show ann))

{- |
Get Type2 from a term's type annotation.
 -}
getType2FromTerm :: SynTraverse1 a => a -> Type2 
getType2FromTerm t = typeToType2 $ extractType $ getAnnTerm t

{- |
Combine SectTypeEnv from several sections (as specified) into one SectTypeEnv (only map is return and no constructor)
 -}
combineSectEnv :: [String] -> SectEnv -> (Map.Map String (String, Type))
combineSectEnv [] env = Map.empty
combineSectEnv (x:xs) env = case (Map.lookup x env) of
        Just v -> Map.unionWith withFunc (getSectTypeEnvMap v) (combineSectEnv xs env)
        Nothing -> combineSectEnv xs env

{- |
Combine all signatures from paragraphs into one.
 -}
combineSignsFromParas :: [Paragraph] -> Sig 
combineSignsFromParas [] = Map.empty 
combineSignsFromParas (x:xs) = Map.union (extractSign (getAnnTerm x)) (combineSignsFromParas xs)

{- |
Combine a collection of signatures into one
 -}
combineSigns :: [Sig] -> Sig 
combineSigns [] = Map.empty
combineSigns [x] = x 
combineSigns (x:xs) = Map.union (combineSigns [x]) (combineSigns xs) 

-- | Turn a section's signature into SectTypeEnv
signToSectTypeEnv :: String -> Sig -> (Map.Map String (String, Type))
signToSectTypeEnv sectname sig = if Map.null sig 
                                    then Map.empty
                                    else Map.fromList (comb sectname (Map.toList sig)) 
    where comb sectname [] = []
          comb sectname [(n, t)] = [(n, (sectname, t))]
          comb sectname (x:xs) = (comb sectname [x]) ++ (comb sectname xs)


-- | Used for Map.union to combine the same key (String, (String, Type)) (especially for SectTypeEnv).
--
-- - For the same key, if they are from the same section with the same type, just return any one. 
-- - Otherwise, put a "<Conflict>" in front of concatenation of section names: s1 and s2 to indicate it is a conflict type or sections for a name.
--
-- @
-- Map.unionWith withFunc 
--  (fromList [("name1", ("sect", T1)), ("name2", ("sect", T2))]) 
--  (fromList [("name1", ("sect", T1)), ("name3", ("sect", T2)), ("name2", ("sect1", T2))]) 
-- == fromList [("name1", ("sect", T1)), ("name3", ("sect", T2)), ("name2", ("<Conflict>sectsect1", T2))] 
-- @
withFunc :: (String, Type) -> (String, Type) -> (String, Type)
withFunc (s1, t1) (s2, t2) 
        | t1 == t2      = (s1, t1)
        -- Conflict only if there are entries with the same key but introduced from different sections
        | otherwise     = ("<Conflict>" ++ s1 ++ s2, t1)

-- | Check if it is a conflict name (with withFunc together) 
filterConflictFunc :: (String, Type) -> Bool
filterConflictFunc (s, t) = isPrefixOf "<Conflict>" s

-- | Assemble error messages
assembleError :: String -> [Ann] -> String
assembleError msg anns = msg ++ " at (" ++ (show line) ++ ":" ++ (show col) ++ ")"
    where (line, col) = extractLineCol (getPosnFromListAnns anns)

{- |
Return duplicates elements in the list. Even one element occurs several times in the list, it still just returns once.

For example, 

@
returnDuplicates [a, b, a, c, c, c] = [a, c]
@
-}
returnDuplicates :: Eq a => [a] -> [a]
returnDuplicates l = rdHelper [] l []
    where rdHelper seen [] dups = dups 
          rdHelper seen (x:xs) dups 
              | x `elem` seen = if x `elem` dups then rdHelper seen xs (dups) else rdHelper seen xs (dups ++ [x]) 
              | otherwise = rdHelper (seen ++ [x]) xs dups

-- | Extract declarations from a schema text.
extractDecl :: SchemaText -> [Declaration]
extractDecl (SchemaTextDecl _ (DeclPart _ s))   = s
extractDecl (SchemaText _ (DeclPart _ s) _)     = s
extractDecl _                                   = []

--getOpNameFromTemplate :: CategoryTemplate -> NAME
--getOpNameFromTemplate (PrefixCatTemplate _ (PrefixTemplate _ prename)) = 
--        case prename of
--            PrePrefixName _ pre     -> pre 
--            PrePPrefixName _ prep   -> prep
--            LPrefixName _ l _ _     -> l 
--            LPPrefixName _ lp _ _   -> lp
--getOpNameFromTemplate (PrefixCatTemplate _ (PowerPrefixTemplate _)) = "\8473"
--getOpNameFromTemplate (PostfixCatTemplate _ (PostfixTemplate _ postname)) = 
--        case postname of
--            PostPostfixName _ post      -> post 
--            PostPPostfixName _ postp    -> postp 
--            ELPostfixName _ el _ _      -> el 
--            ELPPostfixName _  elp _ _   -> elp 
--getOpNameFromTemplate (InfixCatTemplate _ _ _ (InfixTemplate _ infixname)) = 
--        case infixname of
--            InInfixName _ i             -> i
--            InPInfixName _ inp          -> inp
--            ELInfixName _ el _ _        -> el 
--            ELPInfixName _ elp _ _      -> elp
--getOpNameFromTemplate (NofixCatTemplate _ (NofixTemplate _ nofixname)) = 
--        case nofixname of
--            LNofixName _ name _ _        -> name
--            LPNofixName _ llp _ _        -> llp

-- | 13.2.3.1 Generic type instantiation
--
genTypeInst :: Type -- ^ Generic type
        -> [Type2]  -- ^ A square-bracketed list of argument types
        -> Type2    -- ^ Instantiated type 
genTypeInst (GenType formals (GIVEN i)) ts = (GIVEN i)
genTypeInst (GenType formals (GENTYPE i)) ts = getRightType formals i ts 
    where getRightType (f:fs) i ((e):es) = if f == i then e else (getRightType fs i es)   
genTypeInst (GenType formals (PowerType t)) ts = (PowerType (genTypeInst (GenType formals t) ts)) 
genTypeInst (GenType formals (ProdType lt)) ts = (ProdType (expandToProd formals lt ts)) 
    where expandToProd formals [] ts = []
          expandToProd formals [t] ts = [(genTypeInst (GenType formals t) ts)]
          expandToProd formals (x:xs) ts = (expandToProd formals [x] ts) ++ (expandToProd formals xs ts) 
genTypeInst (GenType formals (SchType sig)) ts = SchType (Map.fromList (expandToSchType formals (Map.toList sig) ts))
    where expandToSchType formals [] ts = []
          expandToSchType formals [(i, (Type t2))] ts = [(i, Type (genTypeInst (GenType formals t2) ts))]
          expandToSchType formals (x:xs) ts = (expandToSchType formals [x] ts) ++ (expandToSchType formals xs ts) 
-- TODO: what about if
--  t :: Type.Type = Type.GenType ["X"] (Type.TypeVar "α0")
--  ts = [Type.ProdType [Type.GENTYPE "X",Type.GENTYPE "Y"]]
genTypeInst (GenType formals (TypeVar v)) ts = TypeVar v

-- | 13.2.3.2 Carrier set
--
carrier :: Type2 -> Expression
carrier (GIVEN i) = RefExprA [(TypeAnn (Type (PowerType (GIVEN i))))] (i ++ givenTypeSTROKE)
carrier (GENTYPE i) = RefExprA [(TypeAnn (Type (PowerType (GENTYPE i))))] (i ++ genTypeSTROKE)
carrier (PowerType t) = PowerExpr [(TypeAnn (Type (PowerType (PowerType t))))] (carrier t) 
-- but how to transform ProdExpr to typechecked 
-- should we use transformed (ProdExpr) or just this ProdExpr (though it should not occur after type checking)
-- we transform ProdExpr to SetCompExpr then add type annotation
carrier (ProdType tl) = addAnnTerm (TypeAnn (Type (PowerType (ProdType tl)))) (synTraverse2Tree (ProdExpr [] (toListCarrierExprs tl)))
    where toListCarrierExprs [t] = [(carrier t)]
          toListCarrierExprs (t:xs) = (toListCarrierExprs [t]) ++ (toListCarrierExprs xs)
carrier (SchType sig) = addAnnTerm (TypeAnn (Type (PowerType (SchType sig)))) (transformSchema (synTraverse2Tree (SchemaTextDecl [] (DeclPart [] (listDecl (Map.toList sig))))))
    where listDecl [] = [] 
          listDecl ((i, t) : xs) = (Declaration [] [(DName [] i)] (carrier (typeToType2 t))) : (listDecl xs)
carrier (TypeVar t) = error (err2Msg (ErrTypeCheck (ETypeCTypeVarInCarrier ("Type variables [" ++ show t ++ "] should not occur in carrier!" ) )))

{- |
Check whether two signatures are incompatible or not.

__Compatible__ relations [e1 ≈ e2] is equal to [(dom e2)▹e1 = (dom e1)▹e2].
 -}
isCompatible sig1 sig2 = let d1 = (domRes sig1 sig2) 
                             d2 = (domRes sig2 sig1)
                         in (Map.isSubmapOf d1 d2) && (Map.isSubmapOf d2 d1)
    where domRes sig1 sig2 = Map.filterWithKey (\k _ -> k `elem` (Map.keys sig1)) sig2

{- |
Return the difference of two lists, like the set minus (l1 - l2).

Finally it is equal to a modified list one in which all elements, that are in the list two as well, are removed.

@
listDiff [2, 1, 3, 4] [3, 5, 7, 1] == [2, 4]
@
 -}
listDiff [] l2 = []
listDiff (x:xs) l2 = if x `elem` l2 then (listDiff xs l2) else ([x] ++ (listDiff xs l2))

{- |
Drop all entries in which the key is in the list ("names").
 -}
dropList sig names = Map.filterWithKey (\k _ -> k `notElem` names) sig

{- |
 Unify two types.
 -}
unify :: Type2 -> Type2 -> TypeC Bool
unify (GIVEN i)     (GIVEN j)       = return $ i == j
unify (GENTYPE i)   (GENTYPE j)     = return $ i == j
unify (PowerType s) (PowerType t)   = unify s t 
unify (ProdType s)  (ProdType t)    = if (length s) /= (length t) 
                                      then return False 
                                      else unifyList s t 
    where unifyList [] []           = return True
          unifyList [] (y:ys)       = return False
          unifyList (y:ys) []       = return False
          unifyList (x:xs) (y:ys)   = do 
            r <- unify x y; case r of
                True -> unifyList xs ys
                False -> return False
{- 
 - Check if two signatures are unifiable 
 -}
unify (SchType s1) (SchType s2)     = do
        if (Map.size s1) /= (Map.size s2)
        then return False 
        -- Keys in two maps should be equal to make a unified signature
        else case (equalListTestStr (Map.keys s1) (Map.keys s2) []) of
                (False, pair)    -> return False 
                (True, pair)     -> unifyList pair s1 s2 
    where unifyList [] s1 s2 = return True
          unifyList ((x,y):xs) s1 s2 = case Map.lookup x s1 of
            Nothing -> return False
            Just ts1 -> case Map.lookup y s2 of
                Nothing -> return False
                Just ts2 -> do 
                    r <- unify (typeToType2 ts1) (typeToType2 ts2)   
                    case r of
                        False -> return False
                        True -> unifyList xs s1 s2
{- if unifying two type variables, they will be bound to each other
 -  1. if both of them are not bound to others Types, they will be bound to type variable each other. 
 -      So. [(s, (TypeVar t)), (t, (TypeVar s))]
 -  2. if one of them has been bound, and another hasn't, then the bound one is not changed and 
 -     another one is bound to what the original one binds 
 -      [(s, Ts), (t, (TypeVar t))]
 -      => 
 -      [(s, Ts), (t, Ts)]
 -  3. if both of them have been bound, then unify them
 -      [(s, Ts), (t, Tt)]
 -      =>
 -      unify Ts Tt 
 -}
unify (TypeVar s)   (TypeVar t)     = do 
    -- if unifying the same type variable, return True
    if s == t 
    then return True
    else do 
        -- get the linked types of s and t from the state
        (bs, ts) <- getTypeVarType2 s
        (bt, tt) <- getTypeVarType2 t
        if bs && bt
        then if ts == (TypeVar s) && tt == (TypeVar t) 
             then do
               instATypeVar s (TypeVar t) 
               instATypeVar t (TypeVar s) 
               return True
             else if ts == (TypeVar s) && tt /= (TypeVar t)  
                  then do  
                    instATypeVar s (tt) 
                    return True
                  else if ts /= (TypeVar s) && tt == (TypeVar t)  
                       then do 
                        instATypeVar t (ts) 
                        return True
                       else unify ts tt
        else return False 
{-
 -}
unify s             (TypeVar t)     = unify (TypeVar t) s
{- 
 - unify a type variable with a Type2 which is not a type variable
 - 1. if t occurs in s which is not a type variable, then they don't unify 
 - 2. get the linked type of t, 
 -      2.1 if t hasn't been bound yet, then just bind it to s
 -      2.2 if t has been bound, then unify the bound type of t with s
 -
 -}
-- if t = "a0" and s = GIVEN "𝔸", then 
unify (TypeVar t)   s               = do
    if t `elem` (typeVarsInAType2 s)
    then return False
    else do
        (bt, tt) <- getTypeVarType2 t
        if not bt 
        then return False
        else if tt == (TypeVar t)
             then do 
                    instATypeVar t s 
                    return True
             else unify s tt 
unify _ _                           = return False

{- |
Check if two lists have the same elements which might be not in the same order.
 -}
equalListTest :: (Eq a) => [a] -> [a] -> Bool
equalListTest s t = (null (s \\ t)) && (null (t \\ s))

equalListTestStr :: [String] -> [String] -> [(String, String)] -> (Bool, [(String, String)])
equalListTestStr [] [] cur = (True, cur) 
equalListTestStr [] _ cur = (False, cur) 
equalListTestStr s@(x:xs) t@(y:ys) cur
    | x `elem` t                =  equalListTestStr xs (t \\ [x]) ((x, x):cur)
    | (x++zChar_next) `elem` t  =  equalListTestStr xs (t \\ [(x++zChar_next)]) ((x, (x++zChar_next)):cur)
    | ([last x] == zChar_next) && ((tail x) `elem` t) =  equalListTestStr xs (t \\ [x]) ((x, (tail x)):cur)
    | otherwise                 = (False, cur)

{- |
Replace all type variables in annotations by its instantiated type
-}
updateTypeVarInTypeAnn :: [Ann] -> TypeC [Ann]
updateTypeVarInTypeAnn ann = do
    case b of
        True    -> do
            ret <- replaceAllTypeVarsInAType2 (typeToType2 t)
            case ret of 
                Right t2 -> case t of
                    Type t1                 -> return (addAnnToAnnListHelper (TypeAnn (Type t2)) ann)
                    GenType formals t1      -> return (addAnnToAnnListHelper (TypeAnn (GenType formals t2)) ann)
                    GenType2 formals tg t1  -> return (addAnnToAnnListHelper (TypeAnn (GenType2 formals tg t2)) ann)
                Left err -> return ann
        False   -> return ann
  where (b, t) = hasAndExtractType ann 
