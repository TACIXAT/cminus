(* The type of the abstract syntax tree (AST). *)
type expr =
  | Var of string
  | Int of int
  | Ift of expr*expr list
  | Ife of expr*expr list*expr list
  | Repeat of expr list*expr
  | Read of expr
  | Write of expr
  | Assign of expr*expr
  | Add of expr*expr
  | Sub of expr*expr
  | Mul of expr*expr
  | Div of expr*expr
  | Equiv of expr*expr
  | Less of expr*expr