{
(* Included code *)
open Parser
}

(* Regexes *)

let white = [' ' '\t' '\r']+
let newline = '\n'
let num = ['0'-'9']
let int = '-'? num+
let alpha = ['a'-'z' 'A'-'Z']
let alphanum = alpha | num
let id = alpha alphanum+?
let comment = "/*" _*? "*/"

(* Character stream to token *)

rule read = parse
    | white     { read lexbuf }
    | newline   { read lexbuf }
    | comment   { read lexbuf }
    | "void"    { VOID_TYPE }
    | "int"     { INT_TYPE }
    | "if"      { IF }
    | "else"    { ELSE }
    | "while"   { WHILE }
    | "return"  { RETURN }
    | "="       { ASSIGN }
    | "+"       { PLUS }
    | "-"       { MINUS }
    | "*"       { MUL }
    | "/"       { DIV }
    | "=="      { EQ }
    | "!="      { NOTEQ }
    | "<"       { LT }
    | ">"       { GT }
    | "<="      { LTEQ }
    | ">="      { GTEQ }
    | "("       { LPAREN }
    | ")"       { RPAREN }
    | "{"       { LCURLY }
    | "}"       { RCURLY }
    | "["       { LBRACE }
    | "]"       { RBRACE }
    | ","       { COMMA }
    | ";"       { SEMI }
    | id        { ID (Lexing.lexeme lexbuf) }
    | int       { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | eof       { EOF }