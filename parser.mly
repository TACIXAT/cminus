(* Included code *)

%{
open Ast
%}

(* Tokens *)

%token VOID_TYPE
%token INT_TYPE

%token IF
%token ELSE

%token WHILE

%token PLUS
%token MINUS
%token MUL
%token DIV

%token EQ
%token NOTEQ
%token LT
%token GT
%token LTEQ
%token GTEQ

%token LPAREN
%token RPAREN
%token LCURLY
%token RCURLY
%token LBRACE
%token RBRACE
%token COMMA
%token SEMI

%token ASSIGN
%token RETURN
%token <string> ID
%token <int> INT
%token EOF

(* Precedence and associativity *)

%right ASSIGN RETURN IF ELSE
%left EQ NOTEQ LT GT LTEQ GTEQ
%left PLUS MINUS
%left MUL DIV

(* Entry point rule *)

%start <Ast.program> prog

%%

(* Grammar rules *)

prog:
    | dl = decl_list { Program dl }
    ;
     
decl_list:
    | d = decl; EOF { [ d ] }
    | d = decl; p = decl_list { d :: p }
    ;

decl:
    | f = func_decl { f }
    | v = var_decl { v }
    ;

var_decl:
    | INT_TYPE; n = ID; SEMI { VarDecl(IntDecl(Var(n))) }
    | INT_TYPE; n = ID; LBRACE; s = INT; RBRACE; SEMI { VarDecl(ArrayDecl(Var(n), s)) }
    ;

func_decl:
    | INT_TYPE; n = ID; 
        LPAREN; p = param_list; RPAREN; sl = stmt_block 
        { FuncDeclInt(Var(n), p, sl) }
    | VOID_TYPE; n = ID; 
        LPAREN; p = param_list; RPAREN; sl = stmt_block 
        { FuncDeclVoid(Var(n), p, sl) }
    ;

param_list:
    | VOID_TYPE { [] }
    | INT_TYPE; n = ID; COMMA; l = param_list { IntDecl(Var(n)) :: l }
    | INT_TYPE; n = ID; LBRACE; s = INT; RBRACE; COMMA; l = param_list { ArrayDecl(Var(n), s) :: l }
    | INT_TYPE; n = ID { [ IntDecl(Var(n)) ] }
    | INT_TYPE; n = ID; LBRACE; s = INT; RBRACE { [ ArrayDecl(Var(n), s) ] }
    ;

stmt_block:
    | LCURLY; sl = stmt_list; RCURLY { sl }
    | s = stmt { [ s ] }
    ;

stmt:
    | f = if_stmt { f }
    | w = while_stmt { w }
    | e = expr; SEMI { e }
    ;
    
stmt_list:
    | s = stmt { [ s ] }
    | s = stmt ; sl = stmt_list { s :: sl }
    ;

if_stmt: 
    | IF; e = expr; sb = stmt_block { Ift(e, sb) }
    | IF; e = expr; sbi = stmt_block; ELSE; sbe = stmt_block { Ife(e, sbi, sbe) }
    ;

while_stmt:
    | WHILE; LPAREN; e = expr; RPAREN; sl = stmt_block; { While(e, sl) }
    ;

arg_list: 
    | e = expr; COMMA; al = arg_list { e :: al }
    | e = expr { [ e ] }
    ;

expr:
    | i = INT { Int i }
    | x = ID; LBRACE; e = expr; RBRACE { Access(Var(x), e) }
    | x = ID; LPAREN; al = arg_list; RPAREN { Call(Var(x), al) }
    | x = ID; LPAREN; RPAREN { Call(Var(x), []) }
    | x = ID { Var x }
    | e1 = expr; EQ; e2 = expr { Equiv(e1, e2) }
    | e1 = expr; NOTEQ; e2 = expr { Unequiv(e1, e2) }
    | e1 = expr; LT; e2 = expr { Less(e1, e2) }
    | e1 = expr; GT; e2 = expr { Greater(e1, e2) }
    | e1 = expr; LTEQ; e2 = expr { LessEq(e1, e2) }
    | e1 = expr; GTEQ; e2 = expr { GreaterEq(e1, e2) }
    | e1 = expr; PLUS; e2 = expr { Add(e1, e2) }
    | e1 = expr; MINUS; e2 = expr { Sub(e1, e2) }
    | e1 = expr; MUL; e2 = expr { Mul(e1, e2) }
    | e1 = expr; DIV; e2 = expr { Div(e1, e2) }
    | LPAREN; e = expr; RPAREN { e } 
    | INT_TYPE; n = ID { IntDecl(Var(n)) }
    | INT_TYPE; n = ID; LBRACE; s = INT; RBRACE { ArrayDecl(Var(n), s) }
    | RETURN; e = expr { Return(e) }
    | x = ID; ASSIGN ; e = expr { Assign(Var(x), e) }
    | x = ID; LBRACE; i = expr; RBRACE; ASSIGN ; e = expr { Assign(Access(Var(x), i), e) }
    ;