open Ast

exception Error of string

(* I think I get this for free in codegen *)
let typetbl = Hashtbl.create 5;;
Hashtbl.add typetbl "input" "int";;
Hashtbl.add typetbl "output" "void";;

(* 
*  Undecalred variable      (list of lists of strings)
*  Undeclared function      (list of lists of strings)
*  Multiple declarations    (list of strings)
*  Correct scope            (free with undec)
*  Parameter mismatch       (function definitions - check call against ???)
*)

let check_mult scopel s = 
    let head = List.hd scopel in
    match (List.mem s head) with 
        | true -> 
            let msg = String.concat "" ("Redeclaration of variable " :: [s]) in
            raise (Error msg)
        | false -> 
            let new_head = s :: head in
            let tail = List.tl scopel in 
            new_head :: tail 

let check_decl scopel e = 
    match e with
        | Var s -> check_mult scopel s

let check_exists scopel s = 
    let check = List.map (fun l -> List.mem s l) scopel in
    match (List.mem true check) with 
        | true -> scopel
        | false -> 
            let msg = String.concat "" ("Undeclared variable " :: [s]) in
            raise (Error msg)

(* let check_use scopel e =
    match e with 
        | Var s -> check_exists scopel s *)

let rec check_both scopel e1 e2 =
    analyze_expr scopel e1;
    analyze_expr scopel e2
and analyze_expr scopel e = 
    match e with 
        | IntDecl e -> check_decl scopel e
        | ArrayDecl (e, i) -> check_decl scopel e
        | Access (e1, e2) ->
            check_both scopel e1 e2
        | Call (e, el) -> 
            analyze_expr scopel e;
            for i=0 to (List.length el)-1 do
                analyze_expr scopel (List.nth el i)
            done ;
            scopel
        | Return e ->
            analyze_expr scopel e
        | Var s -> check_exists scopel s
        | Int i -> scopel
        | Assign (e1, e2) -> 
            check_both scopel e1 e2;
        | Equiv (e1, e2) ->
            check_both scopel e1 e2
        | Unequiv (e1, e2) ->
            check_both scopel e1 e2
        | LessEq (e1, e2) ->
            check_both scopel e1 e2
        | GreaterEq (e1, e2) ->
            check_both scopel e1 e2
        | Greater (e1, e2) ->
            check_both scopel e1 e2
        | Less (e1, e2) ->
            check_both scopel e1 e2
        | Add (e1, e2) ->
            check_both scopel e1 e2
        | Sub (e1, e2) ->
            check_both scopel e1 e2
        | Mul (e1, e2) ->
            check_both scopel e1 e2
        | Div (e1, e2) ->
            check_both scopel e1 e2
        | Ift (e, el) -> 
            analyze_expr scopel e;
            let scopel_1 = ref ([] :: scopel) in
            for i=0 to (List.length el)-1 do
                scopel_1 := (analyze_expr !scopel_1 (List.nth el i))
            done ;
            scopel
        | Ife (e, e1, e2) ->
            analyze_expr scopel e;
            let scopel_1 = ref ([] :: scopel) in
            for i=0 to (List.length e1)-1 do
                scopel_1 := (analyze_expr !scopel_1 (List.nth e1 i))
            done ;
            let scopel_2 = ref ([] :: scopel) in
            for i=0 to (List.length e2)-1 do
                scopel_2 := (analyze_expr !scopel_2 (List.nth e2 i))
            done ;
            scopel
        | While (e, el) -> 
            analyze_expr scopel e;
            let scopel_1 = ref ([] :: scopel) in
            for i=0 to (List.length el)-1 do
                scopel_1 := (analyze_expr !scopel_1 (List.nth el i))
            done ;
            scopel

let set_func e t = 
    match e with 
        | Var s ->
            Hashtbl.add typetbl s t 

let rec analyze_decl scopel d = 
    match d with 
        | VarDecl e -> analyze_expr scopel e
        (* | FuncDeclInt (e, pl, el) ->  *)
        | FuncDeclVoid (e, pl, el) -> 
            let scopel_1 = ref (check_decl scopel e) in
            set_func e "void";
            let scopel_2 = ref ([] :: !scopel_1) in (* create a scope for inside function *)
            for i=0 to (List.length el)-1 do
                scopel_2 := (analyze_expr !scopel_2 (List.nth el i))
            done ;
            !scopel_1 (* disregard function scope *)
        | FuncDeclInt (e, pl, el) ->
            let scopel_1 = ref (check_decl scopel e) in
            set_func e "int";
            let scopel_2 = ref ([] :: !scopel_1) in (* create a scope for function and parameters *)
            for i=0 to (List.length pl)-1 do
                scopel_2 := (analyze_expr !scopel_2 (List.nth pl i))
            done ;
            for i=0 to (List.length el)-1 do
                scopel_2 := (analyze_expr !scopel_2 (List.nth el i))
            done ;
            !scopel_1 (* disregard function scope *)

let rec analyze_program p = 
    let scopel = ref [["input"; "output"]] in
    match p with
        | Program dl -> 
            for i = 0 to (List.length dl)-1 do
                scopel := (analyze_decl !scopel (List.nth dl i))
            done ;
            !scopel
            

