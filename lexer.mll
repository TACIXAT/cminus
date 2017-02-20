{
(* Included code *)
open Parser
}

(* Regexes *)

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let comment = '{' [^'}']* '}'

(* Character stream to token *)

rule read = parse
    | white     { read lexbuf }
    | comment   { read lexbuf }
    | "if"      { IF }
    | "then"    { THEN }
    | "else"    { ELSE }
    | "end"     { END }
    | "repeat"  { REPEAT }
    | "until"   { UNTIL }
    | "read"    { READ }
    | "write"   { WRITE }
    | ":="      { ASSIGN }
    | "+"       { PLUS }
    | "-"       { MINUS }
    | "*"       { MUL }
    | "/"       { DIV }
    | "="       { EQUALS }
    | "<"       { LT }
    | "("       { LPAREN }
    | ")"       { RPAREN }
    | ";"       { SEMI }
    | id        { ID (Lexing.lexeme lexbuf) }
    | int       { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | eof       { EOF }
  