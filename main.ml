open Ast

let parse s =
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.read lexbuf in
    ast

let file = Sys.argv.(1)
let contents = Core.Std.In_channel.read_all file
let parsed = parse contents;;
(* AST Printer *)
(* print_string (prog_to_string parsed);; *)
Semantic.analyze_program parsed;;
Codegen.codegen parsed;;
(* Prints to stderr... *)
Llvm.dump_module Codegen.llvm_mod;;