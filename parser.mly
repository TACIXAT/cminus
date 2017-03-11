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
%token ASSIGN
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
%token <string> ID
%token <int> INT
%token EOF

(* Precedence and associativity *)

%right ASSIGN
%left EQUALS
%left LT
%left PLUS
%left MINUS
%left MUL
%left DIV

(* Entry point rule *)

%start <Ast.expr list> prog

%%

(* Grammar rules *)
     
prog:
    | d = decl; EOF { [ d ] }
    | d = decl; p = prog { d :: p }
    ;

decl:
    | f = func_decl { f }
    | v = var_decl { v }
    ;

func_decl:
    | INT_TYPE; n = ID; 
        LPAREN; p = param_list; RPAREN; sl = stmt_block 
        { FuncDeclInt(Decl(n), p, sl) }
    | VOID_TYPE; n = ID; 
        LPAREN; p = param_list; RPAREN; sl = stmt_block 
        { FuncDeclVoid(Decl(n), p, sl) }

stmt_block:
    | LCURLY; sl = stmt_list; RCURLY { sl }
    | sl = stmt_list { [ sl ] }
    ;

param_list:
    | VOID_TYPE { [] }
    | INT_TYPE; n = ID; COMMA; l = param_list { Decl(Var(n)) :: l }
    | INT_TYPE; n = ID { [ Decl(Var(n)) ] }
    ;

var_decl:
    | INT_TYPE; n = ID; SEMI { VarDecl(Var(n)) }
    | INT_TYPE; n = ID; LBRACE; s = INT; RBRACE; SEMI { ArrayDecl(Var(n), s) }

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
    | IF; e = expr; THEN; sl = stmt_list; END { Ift(e, sl) }
    | IF; e = expr; THEN; slt = stmt_list; ELSE; sle = stmt_list; END { Ife(e, slt, sle) }
    ;

while_stmt:
    | WHILE; LPAREN; e = expr; RPAREN; sl = stmt_block; { While(e, sl) }
    ;

arg_list = 
    | e = expr; COMMA; el = arg_list { e :: el }
    | e = expr { [ e ] }
    ;

expr:
    | i = INT { Int i }
    | x = ID; LBRACE; e = expr; RBRACE { Access(Var(x), e) }
    | x = ID; LPAREN; el = arg_list; RPAREN { Call(Var(x), el) }
    | x = ID { Var x }
    | e1 = expr; EQUALS; e2 = expr { Equiv(e1, e2) }
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
    | x = ID; ASSIGN ; e = expr { Assign(Var(x), e) }
    ;