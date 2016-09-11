module Eval where

import LispTypes
import Data.Map as Map
import Debug.Trace

true_p LispFalse = False
true_p _         = True

false_p LispFalse = True
false_p _         = False

make_procedure env parameters body = LispClosure (env, parameters, body)
lambda_parameters (LispList (_:lst:_)) = lst
lambda_body (LispList (_:_:body:[])) = body
procedure_parameters (LispClosure (_, _, (LispList parameters))) = parameters
procedure_body (LispClosure (_,_,body)) = body
procedure_environment (LispClosure (env, _, _)) = env
lookup_variable_value ((Env now_env):upper_env) (LispSymbol var) = case Map.lookup var now_env of
                                                                     Just s -> s
                                                                     Nothing -> lookup_variable_value upper_env (LispSymbol var)
lookup_variable_value [] (LispSymbol var) = error "Unknown symbol name -- " var
text_of_quotation (LispList (_:s)) = LispList s

begin_actions (LispList (_:hs)) = LispList hs

operator (LispList (hd:_)) = hd
operands (LispList (_:tl)) = tl

eval :: ([Env],LispTypes) -> ([Env],LispTypes)
eval (env,exp)
  | self_evaluating_p exp = (env,exp)
  | variable_p exp        = (env,(lookup_variable_value env exp))
  | quoted_p exp          = (env,(text_of_quotation exp))
  | assignment_p exp      = ((eval_assignment env exp),exp)
  | definition_p exp      = ((eval_definition env exp),exp)
  | if_p exp              = eval_if (env,exp)
  | lambda_p exp          = (env,(make_procedure env (lambda_parameters exp) (lambda_body exp)))
  | begin_p exp           = eval_sequence (env, (begin_actions exp))
  | application_p exp     = let (env2,LispList exp2) = list_of_values (env,(LispList (operands exp))) [] in
                              let (env3,procudure) = eval (env2 ,(operator exp)) in
                              apply env3 procudure exp2
  | otherwise             = error "Unknown expression type -- Eval" exp

primitive_proc_plus arguments = primitive_proc_plus_rec 0 arguments
primitive_proc_plus_rec acum ((LispInteger i):rest) = primitive_proc_plus_rec (acum + i) rest
primitive_proc_plus_rec acum [] = LispInteger acum
primitive_proc_plus_rec acum _ = error "Illegal type for plus"

primitive_proc_minus arguments = primitive_proc_minus_rec 0 arguments
primitive_proc_minus_rec acum ((LispInteger i):rest) = primitive_proc_minus_rec (acum - i) rest
primitive_proc_minus_rec acum [] = LispInteger acum
primitive_proc_minus_rec acum _ = error "Illegal type for minus"

primitive_procedure_map = Map.fromList [("+",primitive_proc_plus),("-",primitive_proc_minus)]

primitive_procedure_p (LispClosure _) = False
primitive_procedure_p _ = True

compound_procedure_p (LispClosure _) = True
compound_procedure_p _ = False

apply_primitive_procedure (LispSymbol sym) arg = case (Map.lookup sym primitive_procedure_map) of
                                            Just x  -> x arg
                                            Nothing -> (LispList [])

extend_environment :: [Env] -> [LispTypes] -> [LispTypes] -> [Env]
extend_environment ((Env env_hd):env_tl) ((LispSymbol sym_hd):sym_tl) (value_hd:value_tl) = extend_environment ((Env (Map.insert sym_hd value_hd env_hd)):env_tl) sym_tl value_tl
extend_environment env [] [] = env

apply :: [Env] -> LispTypes -> [LispTypes] -> ([Env],LispTypes)
apply env procedure arguments
  | primitive_procedure_p procedure = let (env2,(LispList args2)) = list_of_values (env,(LispList arguments)) [] in
                                        (env2,apply_primitive_procedure procedure args2)
  | compound_procedure_p procedure = let (_,ret) = eval_sequence ((extend_environment (procedure_environment procedure) (procedure_parameters procedure) arguments),(procedure_body procedure)) in
                                       (env,ret)
  | otherwise                     = error "Unknown procedure type -- APPLY" procedure

list_of_values:: ([Env],LispTypes) -> [LispTypes] -> ([Env],LispTypes)
list_of_values (env,(LispList [])) result = (env,(LispList (reverse result)))
list_of_values (env,(LispList (hd:tl))) now = case eval (env,hd) of
                                   (env2,exp) -> list_of_values (env2,(LispList tl)) (exp:now)

if_predicate (LispList (_:pred:_:_:[])) = pred
if_consequent (LispList (_:_:c:_:[])) = c
if_alternative (LispList (_:_:_:a:[])) = a
if_alternative _ = error "illegal if"

eval_if (env,exp) = case eval (env,(if_predicate exp)) of
                      (env2,LispFalse) -> eval (env2,if_alternative exp)
                      (env2,_)  -> eval (env2,if_consequent exp)

eval_sequence:: ([Env],LispTypes) -> ([Env],LispTypes)
eval_sequence (env,(LispList (x:[]))) = (env,x)
eval_sequence (env,(LispList (x:xs))) = eval_sequence ((case (eval (env,x)) of
                                                      (env, _) -> env),LispList xs)
define_variable_d ((Env env):hs) name value = (Env (Map.insert name value env)):hs
update_variable (Env env:hs) name value = (Env (Map.update name value env)):hs -- TODO
definition_value (LispList (_:value:[])) = value
definition_symbol (LispList ((LispSymbol sym):_:[])) = sym
eval_assignment::[Env] -> LispTypes -> [Env]
eval_assignment env exp = let (env2,val) = eval (env,(definition_value exp)) in
                            (define_variable_d env2 (definition_symbol exp) val)
eval_definition::[Env] -> LispTypes -> [Env]
eval_definition env exp = define_variable_d env (definition_symbol exp) (let (_,exp2) = (eval (env,(definition_value exp))) in
                                                                           exp2)

self_evaluating_p (LispInteger x) = True
self_evaluating_p (LispFloat x) = True
self_evaluating_p (LispString x) = True
self_evaluating_p (LispSymbol x) = True
self_evaluating_p (LispList []) = True
self_evaluating_p _ = False

variable_p (LispSymbol _) = True
variable_p _ = False

quoted_p (LispList ((LispSymbol "quote"):xs)) = True
quoted_p _ = False
assignment_p (LispList ((LispSymbol "set!"):xs)) = True
assignment_p _ = False
definition_p (LispList ((LispSymbol "define"):xs)) = True
definition_p _ = False
if_p (LispList ((LispSymbol "if"):xs)) = True
if_p _ = False
lambda_p (LispList ((LispSymbol "lambda"):xs)) = True
lambda_p _ = False
begin_p (LispList ((LispSymbol "begin"):xs)) = True
begin_p _ = False
cond_p (LispList ((LispSymbol "cond"):xs)) = True
cond_p _ = False
application_p (LispList _) = True
application_p _ = False
