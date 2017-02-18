open Llvm

exception Error of string

let llvm_ctx = global_context ()
let llvm_mod = create_module llvm_ctx "tiny";;

let llvm_builder = builder llvm_ctx
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let i32_t = i32_type llvm_ctx
let void_t = void_type llvm_ctx

let fn_t = function_type i32_t [| |]
let wrt_t = function_type void_t [| i32_t |]

let read = declare_function "read" fn_t llvm_mod
let write = declare_function "write" wrt_t llvm_mod

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
 	| Ast.Equiv (e1, e2) -> 
 		let lhs = codegen_expr e1 in
 		let rhs = codegen_expr e2 in
 		build_icmp Llvm.Icmp.Eq lhs rhs "eqtmp" llvm_builder
 	| Ast.Less (e1, e2) -> 
 		let lhs = codegen_expr e1 in
 		let rhs = codegen_expr e2 in
 		build_icmp Llvm.Icmp.Slt lhs rhs "slttmp" llvm_builder
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
 	| Ast.Div (e1, e2) -> 
 		let lhs = codegen_expr e1 in
 		let rhs = codegen_expr e2 in
 		build_sdiv lhs rhs "divtmp" llvm_builder
 	| Ast.Ift (e, el) -> 
 		(* Create conditional *)
 		let cond = codegen_expr e in
 		(* Create if block *)
 		let if_blk = append_block llvm_ctx "if.true" main in
 		(* Add instructions to if block *)
 		ignore(position_at_end if_blk llvm_builder);
 		ignore(List.map codegen_expr el);
 		(* Create end block *)
 		let end_blk = append_block llvm_ctx "if.end" main in
 		(* Create explicit branch from if -> end block *)
 		let ifendbr = build_br end_blk llvm_builder in
 		(* Create conditional branch *)
 		let cond_blk = instr_parent cond in 
 		ignore(position_at_end cond_blk llvm_builder);
 		let condbr = 
 			build_cond_br cond if_blk end_blk llvm_builder in
 		(* Move to end *)
 		ignore(position_at_end end_blk llvm_builder);
 		condbr
 	| Ast.Ife (e, el1, el2) -> 
 		(* Create conditional *)
 		let cond = codegen_expr e in
 		(* Create if block *)
 		let if_blk = append_block llvm_ctx "if.true" main in
 		(* Add instructions to if block *)
 		ignore(position_at_end if_blk llvm_builder);
 		ignore(List.map codegen_expr el1);
 		(* Create else block *)
 		let else_blk = append_block llvm_ctx "if.false" main in
 		(* Add instructions to else block *)
 		ignore(position_at_end else_blk llvm_builder);
 		ignore(List.map codegen_expr el2);
 		(* Create end block *)
 		let end_blk = append_block llvm_ctx "if.end" main in
 		(* Create conditional *)
 		let cond_blk = instr_parent cond in 
 		ignore(position_at_end cond_blk llvm_builder);
 		let condbr = 
 			build_cond_br cond if_blk else_blk llvm_builder in
 		(* Create explicit branch from if -> end *)
 		ignore(position_at_end if_blk llvm_builder);
 		let ifendbr = build_br end_blk llvm_builder in
 		(* Create explicit branch from else -> end *)
 		ignore(position_at_end else_blk llvm_builder);
 		let elseendbr = build_br end_blk llvm_builder in
 		(* Move to end *)
 		ignore(position_at_end end_blk llvm_builder);
 		elseendbr
 	| Ast.Repeat (el, e) -> 
 		(* Create repeat block *)
 		let rpt_blk = append_block llvm_ctx "repeat.true" main in
 		(* Create explicit branch to repeat *)
 		let brrpt = build_br rpt_blk llvm_builder in
 		(* Add instructions to repeat block *)
 		ignore(position_at_end rpt_blk llvm_builder);
 		ignore(List.map codegen_expr el);
 		(* Set up conditional *)
 		let cond = codegen_expr e in
 		(* Add end block *)
 		let end_blk = append_block llvm_ctx "repeat.end" main in
 		(* Create branch from rpt -> rpt | end *)
 		ignore(position_at_end rpt_blk llvm_builder);
 		let condbr = 
 			build_cond_br cond end_blk rpt_blk llvm_builder in
 		ignore(position_at_end end_blk llvm_builder);
 		condbr
 	| Ast.Read v -> 
 		let read_val = 
 			build_call read [| |] "readtmp" llvm_builder in
 		let name = match v with 
			| Ast.Var name -> name
			| _ -> raise (Error "argument to read must be var")
		in
		let variable = try Hashtbl.find named_values name with
			| Not_found -> create_entry_block_alloca main name;
		in
		ignore(build_store read_val variable llvm_builder);
		read_val
 	| Ast.Write e -> 
 		let _val = codegen_expr e in
 		ignore(build_call write [| _val |] "" llvm_builder);
 		_val

let codegen expr_list = 
	List.map codegen_expr expr_list;
	let ret = const_int i32_t 0 in 
	build_ret ret llvm_builder;
