module LispTypes where

data LispTypes = Symbol String
  | List [LispTypes]
  | Pair LispTypes LispTypes
  | Integer Integer
  | Float Float
  | String String
  | Bool Bool
    
