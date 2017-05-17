{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{- |
Module      : Language.ISOZ.Common.OpTableType
Description : 
Copyright   : (C) Kangfeng Ye, 2017
License     : BSD3
Maintainer  : ye.randall@gmail.com 
Stability   : experimental

Operators table monad.

-}
module Language.ISOZ.Common.OpTableMonad where

import Language.ISOZ.Common.AbsIsoz
import Control.Applicative (Applicative (..))
import Data.Map (Map)
import Control.Monad
import qualified Data.Map as Map
import Language.ISOZ.Common.Type

-- | The state enclosed 
data OtState = OtState {
        -- | operator table from operator names to their template
        opTable :: Map String String, 
        schNameTable :: Map String Bool 
    } deriving (Eq, Ord, Show, Read)

-- | Wrap of the type (a -> Either String (OtState, a)) into the Constructor of OpTableType
--
-- unOState: unWrap the constructor of OpTableType
newtype OpTableType a = OpTableType { 
        unOState :: OtState -> Either String (OtState, a) 
    }

-- | Return the type a in the constructor of OpTableType  
runOpTableType :: OpTableType a -> Either String a
runOpTableType (OpTableType f) 
   = case f (OtState { opTable = Map.empty, schNameTable = Map.empty }) of 
            Left msg -> Left msg
            Right ( _, a ) -> Right a

-- | Functor 
instance Functor OpTableType where
  fmap f a = OpTableType $ \s -> case unOState a s of
                            Left msg -> Left msg
                            Right (s', a') -> Right (s', f a')

-- | Applicative 
instance Applicative OpTableType where
  pure a   = OpTableType $ \s -> Right (s, a)
  fa <*> a = OpTableType $ \s -> case unOState fa s of
                            Left msg -> Left msg
                            Right (s', f) -> case unOState a s' of
                                               Left msg -> Left msg
                                               Right (s'', b) -> Right (s'', f b)

-- | Monad 
instance Monad OpTableType where
  m >>= k  = OpTableType $ \s -> case unOState m s of 
                                Left msg -> Left msg
                                Right (s',a) -> unOState (k a) s'
  return a = OpTableType $ \s -> Right (s,a)

-- | Get OpTable from State
getOpTableType :: OpTableType (Map String String) 
getOpTableType = OpTableType $ \s@OtState{opTable=sc} -> Right (s, sc) 

-- | Set OpTable to State
setOpTableType :: (Map String String) -> OpTableType ()
setOpTableType sc = OpTableType $ \s -> Right (s{opTable=sc}, ())

-- | Get SchNameTable from State
getSchNameTable :: OpTableType (Map String Bool) 
getSchNameTable = OpTableType $ \s@OtState{schNameTable=sc} -> Right (s, sc) 

-- | Set SchNameTable to State
setSchNameTable :: (Map String Bool) -> OpTableType ()
setSchNameTable sc = OpTableType $ \s -> Right (s{schNameTable=sc}, ())
