(* Included code *)

%{
open Ast
%}

(* Tokens *)

%token <int> INT
%token <string> ID
%token IF
%token THEN
%token ELSE
%token END
%token REPEAT
%token UNTIL
%token PLUS
%token MINUS
%token MUL
%token DIV
%token LPAREN
%token RPAREN
%token EQUALS
%token LT
%token ASSIGN
%token READ
%token WRITE
%token SEMI
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
    | s = stmt; EOF { [ s ] }
    | s = stmt; p = prog { s :: p }
    ;

stmt:
    | f = if_stmt { f }
    | i = io_stmt { i }
    | r = repeat_stmt { r }
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

io_stmt:
    | READ; x = ID; SEMI { Read(Var(x)) }
    | WRITE; e = expr; SEMI { Write(e) }
    ;

repeat_stmt:
    | REPEAT; sl = stmt_list; UNTIL; e = expr SEMI { Repeat(sl, e) }
    ;

expr:
    | i = INT { Int i }
    | x = ID { Var x }
    | e1 = expr; EQUALS; e2 = expr { Equiv(e1, e2) }
    | e1 = expr; LT; e2 = expr { Less(e1, e2) }
    | e1 = expr; PLUS; e2 = expr { Add(e1, e2) }
    | e1 = expr; MINUS; e2 = expr { Sub(e1, e2) }
    | e1 = expr; MUL; e2 = expr { Mul(e1, e2) }
    | e1 = expr; DIV; e2 = expr { Div(e1, e2) }
    | LPAREN; e = expr; RPAREN { e } 
    | x = ID; ASSIGN ; e = expr { Assign(Var(x), e) }
    ;
