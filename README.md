# cminus

This project is to implement a compiler from C- to LLVM IR. C Minus is a subset of C, defined in the book [Compiler Construction: Principles and Practice by Kenneth C. Louden](http://www.cs.sjsu.edu/~louden/cmptext/). 

This project will initially implement the TINY language, also defined in Compiler Construction. A tag will be made when a full compiler for TINY is completed.

This is my first project in OCaml, so pardon the mess.

## Resources

I'm using the [Cornell's CS3110 parsing code](http://www.cs.cornell.edu/courses/cs3110/2015fa/l/12-interp/rec.html) as my basis. This was the cleanest code I found that defined a language and produced an AST. The comments are very thorough (though I will delete most of them as time goes on). It uses the [menhir](http://gallium.inria.fr/~fpottier/menhir/) parser generator.

A good resource for code is the [LLVM Kaleidoscope OCaml tutorial](http://llvm.org/docs/tutorial/OCamlLangImpl1.html). It uses camlp4 and the code is more complex than the Cornell tutorial. It includes examples of code generation and other further pieces of a compiler.

## Building

`ocamlbuild -use-menhir main.byte`

Then use `utop` to access the `parse` function.

## Installing OCaml on CentOS 6

TBD (for professor)
