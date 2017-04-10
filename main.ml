open Ast

let parse s =
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.read lexbuf in
    ast

let file = Sys.argv.(1)
let contents = Core.Std.In_channel.read_all file
let parsed = parse contents;;
print_string (prog_to_string parsed);;
let scopel = Semantic.analyze_program parsed;;
print_string (String.concat ", " (List.map (fun l -> String.concat ", " l) scopel));;
print_string "\n";; 

(* let str_list = List.map to_string parsed;;
print_string (String.concat "" (str_list));; *)

(* Codegen.codegen parsed;;
Llvm.dump_module Codegen.llvm_mod;; *)