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
let id = letter alphanum+

(* Character stream to token *)

rule read = parse
    | white     { read lexbuf }
    | newline   { NEWLINE }
    | "void"    { VOID }
    | "int"     { INT }
    | "if"      { IF }
    | "else"    { ELSE }
    | "while"   { WHILE }
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
    | "/*"      { COMMENT_START }
    | "*/"      { COMMENT_END }
    | id        { ID (Lexing.lexeme lexbuf) }
    | int       { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | eof       { EOF }