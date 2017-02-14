open Ast
(*
(* [subst e1 e2 x] is [e1] with [e2] substituted for [x]. *)
let rec subst e1 e2 x = match e1 with
  | Var y      -> if x=y then e2 else e1
  | Int c      -> Int c
  | Add(el,er) -> Add(subst el e2 x, subst er e2 x)
  | Let(y,ebind,ebody) ->
    if x=y 
    then Let(y, subst ebind e2 x, ebody)
    else Let(y, subst ebind e2 x, subst ebody e2 x)

(* A single step of evaluation. *)
let rec step = function
  | Int n               -> failwith "Does not step"
  | Var _               -> failwith "Unbound variable"
  | Add(Int n1, Int n2) -> Int (n1+n2)
  | Add(Int n1, e2)     -> Add(Int n1, step e2)
  | Add(e1,e2)          -> Add(step e1, e2)
  | Let(x,Int n,e2)     -> subst e2 (Int n) x
  | Let(x,e1,e2)        -> Let(x,step e1, e2)

(* The reflexive, transitive closure of [step]. *)
let rec multistep = function
  | Int n -> Int n
  | e     -> multistep (step e)
*)

(***********************************************************************)
(* Everything above this is essentially the same as we saw in lecture. *)
(***********************************************************************)

(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

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

let file = "sample.tny" 
let contents = Core.Std.In_channel.read_all file
let parsed = parse contents
let str_list = List.map to_string parsed;;

print_string (String.concat "" (str_list));;

(* let ast = parse "2 + 2;" ;; *)
(* let gctx = Llvm.global_context () ;; *)

(* Extract a value from an ast node.
   Raises Failure if the argument is a node containing a value. *)
(* let extract_value = function
  | Int i -> i
  | _ -> failwith "Not a value"

(* Interpret an expression *)
let interp e =
  e |> parse |> multistep |> extract_value *)

(* A few test cases *)
(* let run_tests () =
  assert (22 = interp "22");
  assert (22 = interp "11+11");
  assert (22 = interp "(10+1)+(5+6)");
  assert (22 = interp "let x = 22 in x");
  assert (22 = interp "let x = 0 in let x = 22 in x") *)
      

