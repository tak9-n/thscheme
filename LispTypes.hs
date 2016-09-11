module LispTypes where

import Data.Map (Map)

data ProcTypes = Procedure | Primitive

data LispTypes = LispSymbol String
  | LispList [LispTypes]
  | LispPair LispTypes LispTypes
  | LispInteger Integer
  | LispFloat Float
  | LispString String
  | LispFalse
  | LispTrue
  | LispClosure ([Env] ,LispTypes,LispTypes)
    deriving Show
    
data Env = Env (Map String LispTypes) deriving Show
