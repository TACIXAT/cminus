open Llvm

let llvm_ctx = global_context ()
let llvm_mod = create_module llvm_ctx "tiny"
let llvm_builder = builder llvm_ctx
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let int_type = int_type context
let void_type = void_type context
let main = declare_function "main" void_type llvm_mod

let create_entry_block_alloca func name =
  let builder = builder_at llvm_ctx (instr_begin (entry_block func)) in
  build_alloca int_type name builder

let rec codegen = function
  	| Ast.Var n -> 
  		let v = try Hashtbl.find named_values n with
  			| Not_found -> raise (Error "unknown variable name")
  		in build_load v n builder
	| Ast.Int i -> const_int int_type i
 	| Ast.Assign (e1, e2) -> 
 		let name = match e1 with 
			| Ast.Variable name -> name
			| _ -> raise (Error "lhs of = must be var")
		in
		let val = codegen e2 in
		let variable = try Hashtbl.find named_values name with
			| Not_found -> create_entry_block_alloca main name
		in
		build_store val variable builder
 	(* | Equiv (e1, e2) -> 
 	| Less (e1, e2) ->  *)
 	| Ast.Add (e1, e2) -> 
 		let lhs = codegen e1 in
 		let rhs = codegen e2 in
 		build_add lhs rhs "addtmp" builder
 	| Ast.Sub (e1, e2) -> 
 		let lhs = codegen e1 in
 		let rhs = codegen e2 in
 		build_sub lhs rhs "subtmp" builder
 	| Ast.Mul (e1, e2) -> 
 		let lhs = codegen e1 in
 		let rhs = codegen e2 in
 		build_mul lhs rhs "multmp" builder
 	(* | Div (e1, e2) -> 
 	| Ift (e, el) -> 
 	| Ife (e, e1, e2) -> 
 	| Repeat (el, e) -> 
 	| Read v -> 
 	| Write e ->  *)