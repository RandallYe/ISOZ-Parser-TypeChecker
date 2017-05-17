{- |
Module      : Language.ISOZ.Common.Error
Description : 
Copyright   : (C) Kangfeng Ye, 2017
License     : BSD3
Maintainer  : ye.randall@gmail.com 
Stability   : experimental

Error messages of the parser.

-}
module Language.ISOZ.Common.Error where

import Data.Map (Map)
import qualified Data.Map as Map
import Language.ISOZ.Common.AbsIsoz
import Language.ISOZ.Common.TypeCMonad 
import Language.ISOZ.Common.Type
import Language.ISOZ.Common.Ann 

-- | All error categories 
data ErrorType = ErrSecSolve ErrSecSolve    -- ^ Errors during section solving
               | ErrTex2Uni ErrTex2Uni      -- ^ Errors during translation of Latex to Unicode
               | ErrUniLexer ErrUniLexer    -- ^ Errors duing lexing of unicode
               | ErrParser ErrParser        -- ^ Errors duing parsing 
               | ErrTypeCheck ErrTypeCheck  -- ^ Errors duing type checking 
  deriving (Eq, Ord, Show, Read)

-- | Errors during sections solving of Latex specifications
data ErrSecSolve = ESecLexer String -- ^ Lexing error of sections environment
                 | ESecNoFile String -- ^ file doesn't exist
                 | ESecNoEnv String -- ^ Env variable doesn't exist 
  deriving (Eq, Ord, Show, Read)

-- | Errors during translation of Latex specifications to Unicode
data ErrTex2Uni = ET2ULexer String -- ^ Lexing error of sections environment
                | ET2UCmdNotDef String  -- ^ a \cmd is not defined 
                | ET2UCmdEmpty String  --  ^ a \cmd is empty 
                | ET2UCmdRedef String  --  ^ a command is redefined 
                | ET2UUnbalStruct String  --  ^ unbalanced syntax, such as '(' but no ') will cause unbalanced parenthesis  
                | ET2UResSymb String  --  ^ restricted symbol 
                | ET2UNoEndEnv String  --  ^ No \end{} environment found 
                | ET2UInternal String  --  ^ internal error of the module 
  deriving (Eq, Ord, Show, Read)

data ErrUniLexer = EUniLexer String 
  deriving (Eq, Ord, Show, Read)

data ErrParser = EParseSyntaxError String   -- ^ general syntax error 
               | EParseNotASchema String    -- ^ not a schema expression error
               | EParseEPNotExpr String     -- ^ ExprPredicate cannot be expression
               | EParseEPNotPred String     -- ^ ExprPredicate cannot be predicate 
               | EParseOpNotDef String      -- ^ Operator not defined
               | EParseOpInvalidPrec String -- ^ Invalid operator precedence  
               | EParseDiffAssocUsed String -- ^ Different associativities used for operators with the same precedence
  deriving (Eq, Ord, Show, Read)

data ErrTypeCheck = ETypeCDupSect String        -- ^ duplicate/conflict section names  
                  | ETypeCUndefSect String      -- ^ undefined parent or section
                  | ETypeCDupName String        -- ^ duplicate/conflict name definition 
                  | ETypeCGenericUninst String  -- ^ not all generic variables are instantiated 
                  | ETypeCNotSchType String     -- ^ not all generic variables are instantiated 
                  | ETypeCTypeMismatch String --Type2 Expression Type2 Expression -- ^ Expect: a type1 in expr1 but Actual: type2 in expr2 
                  | ETypeCTypeMismatch1 String  -- ^ expect: xxx Actual: yyy  
                  | ETypeCTypeMismatch2 String --Type2 Expression -- ^ Expect: xxx Actual: yyy  
                  | ETypeCUndefName String      -- ^ undefined name 
                  | ETypeCMismatchNoOfPara String -- [String] [Expression]  -- ^ mismatched number of parameters to NAME, expected from a list of formals but actual from a list of expression
                  | ETypeCNotGenericType String -- ^ not a generic type error 
--                  | ETypeCNotSetExpr Expression -- ^ not a set expression 
                  | ETypeCNotAllSameType String [Expression] -- ^ not all expressions in the list have the same type 
                  | ETypeCIdxOutOfRange String -- [Type2] Expression -- ^ index n is out of range of expression 
                  | ETypeCNameNotInSig String -- [String] Expression -- ^ a list of name is not in the signature of an expression
                  | ETypeCSigIncomp String -- ^ incompatible signature 
                  | ETypeCNameExistInSig String -- ^ the new name exists in current signature 
                  | ETypeCTypeVarInCarrier String -- ^ type variable occurs in carrier 
  deriving (Eq, Ord, Show, Read)


err2Msg :: ErrorType -> String
-- Section Solving 
err2Msg (ErrSecSolve (ESecLexer msg)) = "[Section Solving] Lexing Error: " ++ msg
err2Msg (ErrSecSolve (ESecNoFile msg))= "[Section Solving] File Not Exist: " ++ msg
err2Msg (ErrSecSolve (ESecNoEnv msg)) = "[Section Solving] Environment Variable Non Exist: " ++ msg
-- Tex2Unicode
err2Msg (ErrTex2Uni (ET2ULexer msg))      = "[Tex2Unicode] Lexing Error: " ++ msg
err2Msg (ErrTex2Uni (ET2UCmdNotDef msg))  = "[Tex2Unicode] Command Undefined: " ++ msg
err2Msg (ErrTex2Uni (ET2UCmdEmpty msg))   = "[Tex2Unicode] Empty LaTeX Command: " ++ msg 
err2Msg (ErrTex2Uni (ET2UCmdRedef msg))   = "[Tex2Unicode] Command Redefined: " ++ msg
err2Msg (ErrTex2Uni (ET2UUnbalStruct msg))= "[Tex2Unicode] Unbalanced: " ++ msg
err2Msg (ErrTex2Uni (ET2UResSymb msg))    = "[Tex2Unicode] Unallowed Symbols: " ++ msg
err2Msg (ErrTex2Uni (ET2UNoEndEnv msg))   = "[Tex2Unicode] No \\end{} Environment: " ++ msg
err2Msg (ErrTex2Uni (ET2UInternal msg))   = "[Tex2Unicode] Internal Error: " ++ msg
-- Lexing of Unicode
err2Msg (ErrUniLexer (EUniLexer msg))       = "[Lexing] Lexing Error: " ++ msg
-- Parsing
err2Msg (ErrParser (EParseSyntaxError msg)) = "[Parsing] Syntax Error: " ++ msg 
err2Msg (ErrParser (EParseNotASchema msg))  = "[Parsing] Not Schema: " ++ msg 
err2Msg (ErrParser (EParseEPNotExpr msg))   = "[Parsing] Not Expression: " ++ msg 
err2Msg (ErrParser (EParseEPNotPred msg))   = "[Parsing] Not Predicate: " ++ msg 
err2Msg (ErrParser (EParseOpNotDef msg))    = "[Parsing] Operator Not Defined: " ++ msg 
err2Msg (ErrParser (EParseOpInvalidPrec msg))  = "[Parsing] Operator Invalid Precedence: " ++ msg 
err2Msg (ErrParser (EParseDiffAssocUsed msg))  = "[Parsing] Operator Different Associativity Used: " ++ msg 
-- Type checking
err2Msg (ErrTypeCheck (ETypeCDupSect msg))          = "[Typechecking] Duplicate Section Name: " ++ msg
err2Msg (ErrTypeCheck (ETypeCUndefSect msg))        = "[Typechecking] Unknown Section Name: " ++ msg
err2Msg (ErrTypeCheck (ETypeCDupName msg))          = "[Typechecking] Duplicate Names: " ++ msg
err2Msg (ErrTypeCheck (ETypeCGenericUninst msg))    = "[Typechecking] Not All Generic Instantiated: " ++ msg
err2Msg (ErrTypeCheck (ETypeCNotSchType msg))       = "[Typechecking] Not A Schema Type: " ++ msg
err2Msg (ErrTypeCheck (ETypeCTypeMismatch msg))     = "[Typechecking] Type Mismatch: " ++ msg
err2Msg (ErrTypeCheck (ETypeCTypeMismatch1 msg))    = "[Typechecking] Type Mismatch: " ++ msg
err2Msg (ErrTypeCheck (ETypeCTypeMismatch2 msg))    = "[Typechecking] Type Mismatch: " ++ msg
err2Msg (ErrTypeCheck (ETypeCUndefName msg))        = "[Typechecking] Undefined Name: " ++ msg
err2Msg (ErrTypeCheck (ETypeCMismatchNoOfPara msg)) = "[Typechecking] Number Of Parameters Mismatch: " ++ msg
err2Msg (ErrTypeCheck (ETypeCNotGenericType msg))   = "[Typechecking] Redefined Section Name: " ++ msg
--err2Msg (ErrTypeCheck (ETypeCNotSetExpr expr))      = "[Typechecking] Redefined Section Name: " ++ (show expr)
err2Msg (ErrTypeCheck (ETypeCNotAllSameType msg listexprs)) = "[Typechecking] Not All Same Types: " ++ (show msg) ++ "\n\t" ++ (show listexprs) 
err2Msg (ErrTypeCheck (ETypeCIdxOutOfRange msg ))   = "[Typechecking] Index Out of Range: " ++ msg
err2Msg (ErrTypeCheck (ETypeCNameNotInSig msg))     = "[Typechecking] Name Not in Signature: " ++ msg 
err2Msg (ErrTypeCheck (ETypeCSigIncomp msg))        = "[Typechecking] Incompatible signatures: " ++ msg 
err2Msg (ErrTypeCheck (ETypeCNameExistInSig msg))   = "[Typechecking] Name Exist in Signature: " ++ msg 
err2Msg (ErrTypeCheck (ETypeCTypeVarInCarrier msg)) = "[Typechecking] Type Variables in Carrier: " ++ msg 

