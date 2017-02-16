open Llvm

exception Error of string

let llvm_ctx = global_context ()
let llvm_mod = create_module llvm_ctx "tiny";;

let llvm_builder = builder llvm_ctx
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let i32_t = i32_type llvm_ctx
let fn_t = function_type i32_t [| |]
let main = define_function "main" fn_t llvm_mod
let entry = entry_block main;;
position_at_end entry llvm_builder


let create_entry_block_alloca func name =
  let block_builder = builder_at llvm_ctx (instr_begin (entry_block func)) in
  let alloca = build_alloca i32_t name block_builder in
  Hashtbl.add named_values name alloca;
  alloca

let rec codegen_expr = function
  	| Ast.Var n -> 
  		let v = try Hashtbl.find named_values n with
  			| Not_found -> raise (Error "unknown variable name")
  		in 
  		build_load v n llvm_builder
	| Ast.Int i -> const_int i32_t i
 	| Ast.Assign (e1, e2) -> 
 		let name = match e1 with 
			| Ast.Var name -> name
			| _ -> raise (Error "lhs of := must be var")
		in
		let _val = codegen_expr e2 in
		let variable = try Hashtbl.find named_values name with
			| Not_found -> create_entry_block_alloca main name;
		in
		ignore(build_store _val variable llvm_builder);
		_val
 	(* | Equiv (e1, e2) -> 
 	| Less (e1, e2) ->  *)
 	| Ast.Add (e1, e2) -> 
 		let lhs = codegen_expr e1 in
 		let rhs = codegen_expr e2 in
 		build_add lhs rhs "addtmp" llvm_builder
 	| Ast.Sub (e1, e2) -> 
 		let lhs = codegen_expr e1 in
 		let rhs = codegen_expr e2 in
 		build_sub lhs rhs "subtmp" llvm_builder
 	| Ast.Mul (e1, e2) -> 
 		let lhs = codegen_expr e1 in
 		let rhs = codegen_expr e2 in
 		build_mul lhs rhs "multmp" llvm_builder
 	(* | Div (e1, e2) -> 
 	| Ift (e, el) -> 
 	| Ife (e, e1, e2) -> 
 	| Repeat (el, e) -> 
 	| Read v -> 
 	| Write e ->  *)

let codegen expr_list = 
	List.map codegen_expr expr_list
