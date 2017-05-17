{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{- |
Module      : Language.ISOZ.Common.TypeCMonad
Description : 
Copyright   : (C) Kangfeng Ye, 2017
License     : BSD3
Maintainer  : ye.randall@gmail.com 
Stability   : experimental

A state monad to maintain information for parsing and typechecking.

-}
module Language.ISOZ.Common.TypeCMonad where

import Language.ISOZ.Common.AbsIsoz
import Control.Applicative (Applicative (..))
import Data.Map (Map)
import Control.Monad
import qualified Data.Map as Map
import Language.ISOZ.Common.Type

-- | the state enclosed 
data TypeState = TypeState {
        -- | type environment, from NAME to a Type
        env :: Map String Type,
        -- | section environment, from section name to SectTypeEnv
        sectEnv :: Map String SectTypeEnv,
        -- | current section name
        curSectName :: String,
        -- | a list of pair from a type variable to a Type2. Initially, its type is equal to (TypeVar a). Therefore, if a type variable (α) has its type equal to (TypeVar α). It means it is not instantiated
        typeVarList :: Map String Type2,
        -- | true for function
        opNameFuncGen :: Map String Bool 
    } deriving (Eq, Ord, Show, Read)

-- state: TypeState
-- result: a
newtype TypeC a = TypeC { 
-- | unTypeC :: TypeC a -> TypeState -> Either String (TypeState, a). A function to unwrap (state, result) from a TypeC.  
-- unTypeC (TypeC a) oldState . 
-- Return (Left str) or Right (NewState, result)
        unTypeC :: TypeState -> 
                    Either String (TypeState, a) 
    }

runTypeC:: TypeC a -> Either String a
runTypeC (TypeC f) 
   = case f (TypeState { env = Map.empty,
                         sectEnv = Map.empty,
                         curSectName = "",
                         typeVarList = Map.empty,
                         opNameFuncGen = Map.empty }) of 
            Left msg -> Left msg
            Right ( _, a ) -> Right a


instance Functor TypeC where
  fmap f a = TypeC $ \s -> case unTypeC a s of
                            Left msg -> Left msg
                            Right (s', a') -> Right (s', f a')

instance Applicative TypeC where
  pure a   = TypeC $ \s -> Right (s, a)
  fa <*> a = TypeC $ \s -> case unTypeC fa s of
                            Left msg -> Left msg
                            Right (s', f) -> case unTypeC a s' of
                                               Left msg -> Left msg
                                               Right (s'', b) -> Right (s'', f b)

instance Monad TypeC where
  m >>= k  = TypeC $ \s -> case unTypeC m s of 
                                Left msg -> Left msg
                                Right (s',a) -> unTypeC (k a) s'
  return a = TypeC $ \s -> Right (s,a)

-- | get Env from State
getEnvTypeC:: TypeC (Map String Type) 
getEnvTypeC = TypeC $ \s@TypeState{env=sc} -> Right (s, sc) 

-- | set Env to State
setEnvTypeC :: (Map String Type) -> TypeC ()
setEnvTypeC sc = TypeC $ \s -> Right (s{env=sc}, ())

-- | get SectEnv from State
getSectEnvTypeC:: TypeC (Map String SectTypeEnv) 
getSectEnvTypeC = TypeC $ \s@TypeState{sectEnv=sc} -> Right (s, sc) 

-- | set SectEnv to State
setSectEnvTypeC :: (Map String SectTypeEnv) -> TypeC ()
setSectEnvTypeC sc = TypeC $ \s -> Right (s{sectEnv=sc}, ())

-- | get curSectName from State
getCurSectName :: TypeC String 
getCurSectName = TypeC $ \s@TypeState{curSectName=sc} -> Right (s, sc) 

-- | set curSectName to State
setCurSectName :: String -> TypeC ()
setCurSectName sc = TypeC $ \s -> Right (s{curSectName=sc}, ())

-- | get opNameFuncGen from State
getOpNameFuncGen:: TypeC (Map String Bool)
getOpNameFuncGen = TypeC $ \s@TypeState{opNameFuncGen=sc} -> Right (s, sc) 

-- | set opNameFuncGen from State
setOpNameFuncGen :: (Map String Bool) -> TypeC ()
setOpNameFuncGen sc = TypeC $ \s -> Right (s{opNameFuncGen=sc}, ())

typeError :: String -> TypeC a
typeError message = TypeC $ \s -> Left message

runDo :: TypeC String 
runDo = do
    m <- getEnvTypeC 
    setEnvTypeC (Map.insert "v" (Type (GIVEN "T")) m)
    return "ok"

-- | get typeVarList from State
getTypeVarList :: TypeC (Map String Type2)
getTypeVarList = TypeC $ \s@TypeState{typeVarList=sc} -> Right (s, sc) 

-- | set typeVarList to State
setTypeVarList :: (Map String Type2) -> TypeC ()
setTypeVarList sc = TypeC $ \s -> Right (s{typeVarList=sc}, ())

-- | clear all type variables
clearTypeVarList :: TypeC ()
clearTypeVarList = setTypeVarList (Map.empty) 

{- |
 allocAddANewTypeVar will add a variable to the map
 -  If there are (n-1) variables in the typeVarList, then the new type variable name
 -  will be ("αn", TypeVar "αn")
 - and return "αn"
 -}
allocAddANewTypeVar :: TypeC String
allocAddANewTypeVar = do 
    m <- getTypeVarList
    let v = ("α" ++ (show (Map.size m))) in do 
        setTypeVarList $ Map.insert v (TypeVar v) m
        return v 

{- | 
 check if a type variable is in the typeVarList or not
 - return
 -      (True, t)           - in the typeVarList and return its bound type
 -      (False, TypeVar v)  - not in the typeVarList 
 -}
getTypeVarType2 :: String -> TypeC (Bool, Type2)
getTypeVarType2 v = do 
    m <- getTypeVarList
    case Map.lookup v m of
        Nothing -> return (False, TypeVar v)
        Just t -> return (True, t) 

{- |
 get all type variables which occur in a Type2
 -}
typeVarsInAType2 :: Type2 -> [String]
typeVarsInAType2 (GIVEN _) = []
typeVarsInAType2 (GENTYPE _) = []
typeVarsInAType2 (PowerType p) = typeVarsInAType2 p
typeVarsInAType2 (ProdType s@(x:xs)) = typeVarsInAProdType s
    where typeVarsInAProdType [] = [] 
          typeVarsInAProdType (x:xs) = (typeVarsInAType2 x) ++ (typeVarsInAProdType xs)
typeVarsInAType2 (TypeVar t) = [t]
typeVarsInAType2 (SchType sig) = typeVarsInSig (Map.toList sig)
    where typeVarsInSig [] = []
          typeVarsInSig ((v, tv):xs) = (typeVarsInAType2 (typeToType2 tv)) ++ (typeVarsInSig xs)

-- | check if a type variable is instantiated or not
isATypeVarInstantiated :: String -> (Map String Type2) -> Bool
isATypeVarInstantiated a m = case Map.lookup a m of
        Nothing -> False
        Just t -> case typeVarsInAType2 t of
                [] -> True 
                [a] -> False 
                as -> isTypeVarsInstantiated as m

-- | check if a list of type variables is instantiated or not
isTypeVarsInstantiated :: [String] -> (Map String Type2) -> Bool
isTypeVarsInstantiated [x] m = isATypeVarInstantiated x m 
isTypeVarsInstantiated (x:xs) m = if isATypeVarInstantiated x m
                                  then isTypeVarsInstantiated xs m
                                  else False 
isTypeVarsInstantiated [] m = True 
    
-- | check if all generic types are instantiated 
isAllGenInsted :: TypeC Bool
isAllGenInsted = do 
    m <- getTypeVarList
    return $ isTypeVarsInstantiated (Map.keys m) m

{- |
 instantiate a type variable (v) to a type (t) at the same time, bind all type variables whose type2 is linked to this type variable, to this type2
 -}
instATypeVar :: String -> Type2 -> TypeC ()
instATypeVar v t = do
    m <- getTypeVarList
    -- it is possible that there are some type variables, which have been instantiated, 
    -- in t and we should replace them by their instantiated types  
    ret <- replaceAllTypeVarsInAType2 t
    case ret of
        Left  err   -> error ("Not all type variables in a type [" ++ show t ++ "] can be replaced by their instantiated copies.") 
        Right t'    -> do
            setTypeVarList $ Map.insert v t' m
            updateAllType2BoundThisVar v t' 

{- |
 Update all entries in typeVarList that are bound to the type variable v by its type t
 -}
updateAllType2BoundThisVar :: String -> Type2 -> TypeC ()
updateAllType2BoundThisVar v t = do
    m <- getTypeVarList
    setTypeVarList $ Map.fromList $ update v t (Map.toList m) 
    where update v t [] = []
          update v t ((u, s):xs) = (u, updateOne v t s): (update v t xs)
          updateMore v t [] = []
          updateMore v t (x:xs) = (updateOne v t x) : (updateMore v t xs)
          updateOne v t (GIVEN s) = GIVEN s
          updateOne v t (GENTYPE s) = GENTYPE s
          updateOne v t (PowerType s) = PowerType (updateOne v t s)
          updateOne v t (ProdType s) = ProdType (updateMore v t s)
          updateOne v t (TypeVar s) = if v == s
                                      then t
                                      else (TypeVar s) 
          updateOne v t (SchType s) = SchType (Map.fromList (updateMapList v t (Map.toList s)))
          updateMapList v t [] = []
          updateMapList v t ((u, Type s):xs) = (u, Type (updateOne v t s)) : (updateMapList v t xs)
          updateMapList v t ((u, GenType l s):xs) = (u, GenType l (updateOne v t s)) : (updateMapList v t xs)
          updateMapList v t ((u, GenType2 l s1 s2):xs) = (u, GenType2 l (updateOne v t s1) (updateOne v t s2)) : (updateMapList v t xs)

{- |
 Replace all type variables in a Type2 by its instantiated types
 -}
replaceAllTypeVarsInAType2 :: Type2 -> TypeC (Either String Type2)
replaceAllTypeVarsInAType2 (GIVEN t) = return (Right (GIVEN t))
replaceAllTypeVarsInAType2 (GENTYPE t) = return (Right (GENTYPE t))
replaceAllTypeVarsInAType2 (PowerType p) = do
    ret <- replaceAllTypeVarsInAType2 p
    case ret of
        Right t     -> return (Right (PowerType t))
        Left  err   -> return (Left err)
replaceAllTypeVarsInAType2 (ProdType s@(x:xs)) = do
        ret <- typeVarsInAProdType s
        case ret of
            Right t     ->  return (Right (ProdType t))
            Left err    ->  return (Left err)
    where typeVarsInAProdType [] = return (Right [])
          typeVarsInAProdType (x:xs) = do
            ret <- replaceAllTypeVarsInAType2 x
            case ret of
                Right t     -> do
                    ret <- typeVarsInAProdType xs
                    case ret of
                        Right ts    -> return (Right (t : ts))
                        Left  err   -> return (Left err)
                Left  err   -> return (Left err)
replaceAllTypeVarsInAType2 (SchType sig) = do
        ret <- typeVarsInSig (Map.toList sig)
        case ret of
            Right t     ->  return (Right (SchType (Map.fromList t)))
            Left err    ->  return (Left err)
    where typeVarsInSig [] = return (Right [])
          typeVarsInSig ((v, Type tv):xs) = do
                ret <- replaceAllTypeVarsInAType2 tv
                case ret of
                    Right t     -> do
                        ret <- typeVarsInSig xs
                        case ret of
                            Right ts    -> return (Right ((v, Type t) : ts))
                            Left  err   -> return (Left err)
                    Left  err   -> return (Left err)
          typeVarsInSig ((v, GenType formals tv):xs) = do
                ret <- replaceAllTypeVarsInAType2 tv
                case ret of
                    Right t     -> do
                        ret <- typeVarsInSig xs
                        case ret of
                            Right ts    -> return (Right ((v, GenType formals t) : ts))
                            Left  err   -> return (Left err)
                    Left  err   -> return (Left err)
          typeVarsInSig ((v, GenType2 formals tg tv):xs) = do 
                ret <- replaceAllTypeVarsInAType2 tv
                case ret of
                    Right t     -> do
                        ret <- typeVarsInSig xs
                        case ret of
                            Right ts    -> return (Right ((v, GenType2 formals tg t) : ts))
                            Left  err   -> return (Left err)
                    Left  err   -> return (Left err)
replaceAllTypeVarsInAType2 (TypeVar t) = do
    (b, tv) <- getTypeVarType2 t 
    if b 
    then return (Right tv)
    else return (Left ("Type variable [" ++ t ++ "] is not in the variable lists!"))
