{- |
Module      : Language.ISOZ.Common.Type
Description : 
Copyright   : (C) Kangfeng Ye, 2017
License     : BSD3
Maintainer  : ye.randall@gmail.com 
Stability   : experimental

annotated syntax (see clause 10.2).

-}
module Language.ISOZ.Common.Type where

import Data.Map (Map)
import qualified Data.Map as Map

-- | section type environment 
data SectTypeEnv = SectTypeEnv (Map String (String, Type))
  deriving (Eq, Ord, Show, Read)

-- | section environment (map from section names to SectTypeEnv)
type SectEnv = Map String SectTypeEnv

-- | Type (see clause 10)  
data Type = Type Type2  -- ^ Type2
          | GenType [String] Type2 -- ^ Generic Type2 ([X] GIVEN)
          | GenType2 [String] Type2 Type2 -- ^ Generic Type2 ([X] GIVEN, GIVEN)
  deriving (Eq, Ord, Show, Read)

-- | Type2 (see clause 10.2)
data Type2 = GIVEN String   -- ^ given type 
           | GENTYPE String -- ^ generic parameter type 
           | PowerType Type2    -- ^ powerset type
           | ProdType [Type2]   -- ^ Cartesian product type 
           | SchType Sig        -- ^ schema type
           | TypeVar String     -- ^ type variable 
--           | NumType String 
  deriving (Eq, Ord, Show, Read)

-- | signature
type Sig = Map String Type  -- ^ a map from names to their types
--deriving (Eq, Ord, Show, Read)

-- | get section type environment's map
getSectTypeEnvMap :: SectTypeEnv -> (Map String (String, Type)) 
getSectTypeEnvMap (SectTypeEnv map) = map 

-- | convert a SectTypeEnv to a list of pair from a name to its type
sectTypeEnvToTypeEnv :: SectTypeEnv -> [(String, Type)]
sectTypeEnvToTypeEnv (SectTypeEnv m) = map extractTypeEnvFromSectTypeEnv (Map.toList m)

-- | extract a type environment from section type environment 
extractTypeEnvFromSectTypeEnv :: (String, (String, Type)) -> (String, Type)
extractTypeEnvFromSectTypeEnv (a, (b, c)) = (a, c) 

-- | Get Type2 of a Type
typeToType2 :: Type -> Type2
typeToType2 (Type s) = s 
typeToType2 (GenType formals t2) = t2
typeToType2 (GenType2 formals t21 t22) = t22

