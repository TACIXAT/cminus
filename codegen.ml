open Llvm

exception Error of string

let llvm_ctx = global_context ()
let llvm_mod = create_module llvm_ctx "cminus"

let llvm_builder = builder llvm_ctx
let i32_t = i32_type llvm_ctx
let void_t = void_type llvm_ctx

let in_t = function_type i32_t [| |]
let out_t = function_type i32_t [| i32_t |]

let input = declare_function "input" in_t llvm_mod
let output = declare_function "output" out_t llvm_mod

(* let main = define_function "main" fn_t llvm_mod
let entry = entry_block main;;
position_at_end entry llvm_builder *)

let global_scope:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let scope_list = ref [global_scope];;
Hashtbl.add global_scope "input" input;
Hashtbl.add global_scope "output" output

(* List of hash tables for scope *)
(* Function to traverse up and find variables *)
(*  *)
let rec get_var scopel name = 
    let scope_tbl = List.hd scopel in
    if Hashtbl.mem scope_tbl name then
        Hashtbl.find scope_tbl name
    else 
        get_var (List.tl scopel) name

let create_entry_block_alloca func name =
  let block_builder = builder_at llvm_ctx (instr_begin (entry_block func)) in
  let alloca = build_alloca i32_t name block_builder in
  let scopel = !scope_list in 
  let scope = List.hd scopel in 
  Hashtbl.add scope name alloca;
  alloca

let rec codegen_expr func expr =
    match expr with 
        | Ast.IntDecl e -> 
            let name = match e with 
                | Var n -> n
            in
            create_entry_block_alloca func name
        (* | Ast.ArrayDecl (e, i) ->  *)
        (* | Ast.Access (e1, e2) ->  *)
        | Ast.Call (e, el) -> 
            let args = Array.of_list (List.map (fun e -> codegen_expr func e) el) in
            let fn_name = match e with 
                | Var name -> name
            in
            let fn = Hashtbl.find global_scope fn_name in
            build_call fn args "calltmp" llvm_builder
        | Ast.Return e -> 
            build_ret (codegen_expr func e) llvm_builder;
        | Ast.Var s -> 
            let v = get_var !scope_list s in
            build_load v s llvm_builder
        | Ast.Int i -> const_int i32_t i
        | Ast.Assign (e1, e2) -> 
            let name = match e1 with 
                | Ast.Var name -> name
                | _ -> raise (Error "lhs of assign must be var")
            in
            let _val = codegen_expr func e2 in
            let variable = get_var !scope_list name in
            ignore(build_store _val variable llvm_builder);
            _val
        | Ast.Equiv (e1, e2) -> 
            let lhs = codegen_expr func e1 in
            let rhs = codegen_expr func e2 in
            build_icmp Llvm.Icmp.Eq lhs rhs "eqtmp" llvm_builder
        | Ast.Unequiv (e1, e2) -> 
            let lhs = codegen_expr func e1 in
            let rhs = codegen_expr func e2 in
            build_icmp Llvm.Icmp.Ne lhs rhs "netmp" llvm_builder
        | Ast.LessEq (e1, e2) -> 
            let lhs = codegen_expr func e1 in
            let rhs = codegen_expr func e2 in
            build_icmp Llvm.Icmp.Sle lhs rhs "sletmp" llvm_builder
        | Ast.Less (e1, e2) -> 
            let lhs = codegen_expr func e1 in
            let rhs = codegen_expr func e2 in
            build_icmp Llvm.Icmp.Slt lhs rhs "slttmp" llvm_builder
        | Ast.GreaterEq (e1, e2) -> 
            let lhs = codegen_expr func e1 in
            let rhs = codegen_expr func e2 in
            build_icmp Llvm.Icmp.Sge lhs rhs "sgetmp" llvm_builder
        | Ast.Greater (e1, e2) -> 
            let lhs = codegen_expr func e1 in
            let rhs = codegen_expr func e2 in
            build_icmp Llvm.Icmp.Sgt lhs rhs "sgttmp" llvm_builder
        | Ast.Add (e1, e2) -> 
            let lhs = codegen_expr func e1 in
            let rhs = codegen_expr func e2 in
            build_add lhs rhs "addtmp" llvm_builder
        | Ast.Sub (e1, e2) -> 
            let lhs = codegen_expr func e1 in
            let rhs = codegen_expr func e2 in
            build_sub lhs rhs "subtmp" llvm_builder
        | Ast.Mul (e1, e2) -> 
            let lhs = codegen_expr func e1 in
            let rhs = codegen_expr func e2 in
            build_mul lhs rhs "multmp" llvm_builder
        | Ast.Div (e1, e2) -> 
            let lhs = codegen_expr func e1 in
            let rhs = codegen_expr func e2 in
            build_sdiv lhs rhs "divtmp" llvm_builder
        | Ast.Ift (e, el) -> 
            (* Create conditional *)
            let cond = codegen_expr func e in
            (* Create if block *)
            let if_blk = append_block llvm_ctx "if.true" func in
            (* Add scope *)
            let scope:(string, llvalue) Hashtbl.t = Hashtbl.create 10 in
            scope_list := scope :: !scope_list;
            (* Add instructions to if block *)
            ignore(position_at_end if_blk llvm_builder);
            ignore(List.map (fun e -> codegen_expr func e) el);
            (* Create end block *)
            let end_blk = append_block llvm_ctx "if.end" func in
            (* Create explicit branch from if -> end block *)
            let ifendbr = build_br end_blk llvm_builder in
            (* Create conditional branch *)
            let cond_blk = instr_parent cond in 
            ignore(position_at_end cond_blk llvm_builder);
            let condbr = 
                build_cond_br cond if_blk end_blk llvm_builder in
            (* Move to end *)
            ignore(position_at_end end_blk llvm_builder);
            scope_list := List.tl !scope_list;
            condbr
        | Ast.Ife (e, e1, e2) -> 
            (* Create conditional *)
            let cond = codegen_expr func e in
            (* Create if block *)
            let if_blk = append_block llvm_ctx "if.true" func in
            (* Add scope *)
            let scope_t:(string, llvalue) Hashtbl.t = Hashtbl.create 10 in
            scope_list := scope_t :: !scope_list;
            (* Add instructions to if block *)
            ignore(position_at_end if_blk llvm_builder);
            ignore(List.map (fun e -> codegen_expr func e) e1);
            (* Remove Scope *)
            scope_list := List.tl !scope_list;
            (* Add Scope *)
            let scope_e:(string, llvalue) Hashtbl.t = Hashtbl.create 10 in
            scope_list := scope_e :: !scope_list;
            (* Create else block *)
            let else_blk = append_block llvm_ctx "if.false" func in
            (* Add instructions to else block *)
            ignore(position_at_end else_blk llvm_builder);
            ignore(List.map (fun e -> codegen_expr func e) e2);
            (* Remove Scope *)
            scope_list := List.tl !scope_list;
            (* Create end block *)
            let end_blk = append_block llvm_ctx "if.end" func in
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
        | Ast.While (e, el) -> 
            (* Create blocks *)
            let cond_blk = append_block llvm_ctx "whl.cond" func in
            let whl_blk = append_block llvm_ctx "whl.true" func in
            let end_blk = append_block llvm_ctx "whl.end" func in
            (* Create branch to conditional block *)
            let br_cond = build_br cond_blk llvm_builder in
            (* Build conditional *)
            ignore(position_at_end cond_blk llvm_builder);
            let cond = codegen_expr func e in
            (* Build branch to while or end *)
            let condbr = 
                build_cond_br cond whl_blk end_blk llvm_builder in
            (* Build while block *)
            ignore(position_at_end whl_blk llvm_builder);
            ignore(List.map (fun e -> codegen_expr func e) el);
            (* Explicit branch back to conditional *)
            let loop_br = build_br cond_blk llvm_builder in
            (* Move to end *)
            ignore(position_at_end end_blk llvm_builder);
            condbr

(*  
    let scope:(string, llvalue) Hashtbl.t = Hashtbl.create 10 in
    scope_list := scope :: !scope_list;
    scope_list := List.tl !scope_list 
*)

let handle_global decl = 
    let v = match decl with 
        | Ast.IntDecl v -> v 
        (* TODO: arrays *)
    in
    let name = match v with
        | Ast.Var n -> n
    in
    let global = declare_global i32_t name llvm_mod in
    Hashtbl.add global_scope name global;
    global

let rec codegen_decl = function 
    | Ast.FuncDeclInt (e, pl, el) -> 
        let name = match e with 
            | Ast.Var n -> n
        in
        let args = Array.make (List.length pl) i32_t in
        let fn_t = function_type i32_t args in 
        let fn = define_function name fn_t llvm_mod in
        Hashtbl.add global_scope name fn;
        (* Set names for all arguments. *)
        let scope:(string, llvalue) Hashtbl.t = Hashtbl.create 10 in
        let entry = entry_block fn in 
        position_at_end entry llvm_builder;
        Array.iteri (
            fun i arg ->
                let v = match (List.nth pl i) with
                    | Ast.IntDecl n -> n 
                in
                let name = match v with 
                    | Ast.Var n -> n 
                in
                (* set_value_name name arg; *)
                let alloca = create_entry_block_alloca fn name in
                build_store arg alloca llvm_builder;
                Hashtbl.add scope name alloca;
            ) 
            (params fn);
        scope_list := scope :: !scope_list;
        List.map (fun e -> codegen_expr fn e) el;
        scope_list := List.tl !scope_list
    | Ast.FuncDeclVoid (e, pl, el) -> 
        let name = match e with 
            | Ast.Var n -> n
        in
        let args = Array.make (List.length pl) i32_t in
        let fn_t = function_type i32_t args in 
        let fn = define_function name fn_t llvm_mod in
        Hashtbl.add global_scope name fn;
        let scope:(string, llvalue) Hashtbl.t = Hashtbl.create 10 in
        scope_list := scope :: !scope_list;
        let entry = entry_block fn in 
        position_at_end entry llvm_builder;
        List.map (fun e -> codegen_expr fn e) el;
        let ret = const_int i32_t 0 in 
        build_ret ret llvm_builder;
        scope_list := List.tl !scope_list
    | Ast.VarDecl e -> 
        handle_global e; ()

let rec codegen = function
    | Ast.Program dl -> List.map codegen_decl dl









