(* AST definition *)
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

let ccatl l = String.concat "" l

let rec to_string = function
    | Var s -> ccatl ["( Var "; s; " )"]
    | Int i -> ccatl ["( Int "; (string_of_int i); " )"]
    | Assign (e1, e2) -> ccatl ((to_string e1) :: " := " :: (to_string e2) :: ["\n"])
    | Equiv (e1, e2) -> ccatl ((to_string e1) :: " = " :: (to_string e2) :: [])
    | Less (e1, e2) -> ccatl ((to_string e1) :: " < " :: (to_string e2) :: [])
    | Add (e1, e2) -> ccatl ((to_string e1) :: " + " :: (to_string e2) :: [])
    | Sub (e1, e2) -> ccatl ((to_string e1) :: " - " :: (to_string e2) :: [])
    | Mul (e1, e2) -> ccatl ((to_string e1) :: " * " :: (to_string e2) :: [])
    | Div (e1, e2) -> ccatl ((to_string e1) :: " / " :: (to_string e2) :: [])
    | Ift (e, el) -> ccatl 
        ("If ( " :: (to_string e) :: " )\n(\n" :: (List.map to_string el) @ [")\n"])
    | Ife (e, e1, e2) -> ccatl 
        (("If ( " :: (to_string e) :: " )\n(\n" :: (List.map to_string e1)) 
        @ ("\n) else (\n" :: (List.map to_string e2) @ [")\n"]))
    | Repeat (el, e) -> ccatl ("Repeat (\n" :: 
        (List.map to_string el) @ [") until ("; (to_string e); " )\n"])
    | Read v -> ccatl ("( Read" :: (to_string v) :: [" )\n"])
    | Write e -> ccatl ("( Write" :: (to_string e) :: [" )\n"])