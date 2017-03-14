(* AST definition *)

type expr =
    | IntDecl of expr
    | ArrayDecl of expr*int
    | Var of string
    | Access of expr*expr
    | Call of expr*expr list
    | Int of int
    | Ift of expr*expr list
    | Ife of expr*expr list*expr list
    | While of expr*expr list
    | Assign of expr*expr
    | Add of expr*expr
    | Sub of expr*expr
    | Mul of expr*expr
    | Div of expr*expr
    | Equiv of expr*expr
    | Unequiv of expr*expr
    | Less of expr*expr
    | LessEq of expr*expr
    | Greater of expr*expr
    | GreaterEq of expr*expr
    | Return of expr

type decl = 
    | FuncDeclInt of expr*expr list*expr list
    | FuncDeclVoid of expr*expr list*expr list
    | VarDecl of expr

type program = 
    | Program of decl list


(* Print function so main.ml isn't boring *)

let ccatl l = String.concat "" l

let rec expr_to_string = function
    | IntDecl e -> ccatl ["IntDecl( "; expr_to_string e; " )\n"] 
    | ArrayDecl (e, i) -> ccatl ["ArrayDecl( "; expr_to_string e; ", "; (string_of_int i); " )\n"] 
    | Access (e1, e2) -> ccatl ["Access( "; expr_to_string e1; "["; expr_to_string e2; "] )"]
    | Call (e, el) -> ccatl ("Call( " :: (expr_to_string e) :: "( " :: (List.map expr_to_string el) @ [" ) )"])
    | Return e -> ccatl ["Return( "; expr_to_string e; " )"] 
    | Var s -> ccatl ["( Var "; s; " )"]
    | Int i -> ccatl ["( Int "; (string_of_int i); " )"]
    | Assign (e1, e2) -> ccatl ((expr_to_string e1) :: " = " :: (expr_to_string e2) :: ["\n"])
    | Equiv (e1, e2) -> ccatl ((expr_to_string e1) :: " == " :: (expr_to_string e2) :: [])
    | Unequiv (e1, e2) -> ccatl ((expr_to_string e1) :: " != " :: (expr_to_string e2) :: [])
    | LessEq (e1, e2) -> ccatl ((expr_to_string e1) :: " <= " :: (expr_to_string e2) :: [])
    | GreaterEq (e1, e2) -> ccatl ((expr_to_string e1) :: " >= " :: (expr_to_string e2) :: [])
    | Greater (e1, e2) -> ccatl ((expr_to_string e1) :: " > " :: (expr_to_string e2) :: [])
    | Less (e1, e2) -> ccatl ((expr_to_string e1) :: " < " :: (expr_to_string e2) :: [])
    | Add (e1, e2) -> ccatl ((expr_to_string e1) :: " + " :: (expr_to_string e2) :: [])
    | Sub (e1, e2) -> ccatl ((expr_to_string e1) :: " - " :: (expr_to_string e2) :: [])
    | Mul (e1, e2) -> ccatl ((expr_to_string e1) :: " * " :: (expr_to_string e2) :: [])
    | Div (e1, e2) -> ccatl ((expr_to_string e1) :: " / " :: (expr_to_string e2) :: [])
    | Ift (e, el) -> ccatl 
        ("If ( " :: (expr_to_string e) :: " )\n(\n" :: (List.map expr_to_string el) @ [")\n"])
    | Ife (e, e1, e2) -> ccatl 
        (("If ( " :: (expr_to_string e) :: " )\n(\n" :: (List.map expr_to_string e1)) 
        @ ("\n) else (\n" :: (List.map expr_to_string e2) @ ["\n)\n"]))
    | While (e, el) -> ccatl (["While ("; (expr_to_string e); " )\n"] @ (List.map expr_to_string el))

let rec decl_to_string = function 
    | FuncDeclInt (e, pl, el) -> ccatl ("IntFunc " :: (expr_to_string e) :: "(\n" 
        :: (List.map expr_to_string pl) @ [")\n(\n"] @ (List.map expr_to_string el) @ [" )\n"])
    | FuncDeclVoid (e, pl, el) -> ccatl ("VoidFunc " :: (expr_to_string e) :: "(\n" 
        :: (List.map expr_to_string pl) @ [")\n(\n"] @ (List.map expr_to_string el) @ [" )\n"])
    | VarDecl e -> expr_to_string e

let rec prog_to_string = function
    | Program dl -> ccatl (List.map decl_to_string dl)
