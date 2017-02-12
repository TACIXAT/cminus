# cminus

This project is to implement a compiler from C- to LLVM IR. C Minus is a subset of C, defined in the book [Compiler Construction: Principles and Practice by Kenneth C. Louden](http://www.cs.sjsu.edu/~louden/cmptext/). 

This project will initially implement the TINY language, also defined in Compiler Construction. A tag will be made when a full compiler for TINY is completed.

This is my first project in OCaml, so pardon the mess.

## Resources

I'm using the [Cornell's CS3110 parsing code](http://www.cs.cornell.edu/courses/cs3110/2015fa/l/12-interp/rec.html) as my basis. This was the cleanest code I found that defined a language and produced an AST. The comments are very thorough (though I will delete most of them as time goes on). It uses the [menhir](http://gallium.inria.fr/~fpottier/menhir/) parser generator.

A good resource for code is the [LLVM Kaleidoscope OCaml tutorial](http://llvm.org/docs/tutorial/OCamlLangImpl1.html). It uses camlp4 and the code is more complex than the Cornell tutorial. It includes examples of code generation and other further pieces of a compiler.

## Dependencies

Install ocaml.

Build LLVM from source, it should detect ocaml and install bindings when you `sudo make install`. On OS Sierra you can check for `/usr/local/lib/ocaml/llvm*`.

Alternatively, you can try to install LLVM with your package manager, then install the bindings with `opam install llvm`.

If you want these in utop you have to build a custom one. You can build an `llvmutop` with step 3 [here](https://xysun.github.io/posts/install-llvm-ocaml-bindings-and-toplevel.html), credit @xysun.

```
echo "let () = UTop_main.main ()" > myutop_main.ml
ocamlfind ocamlmktop -o llvmutop -thread -linkpkg -package utop llvm.cma myutop_main.ml -cc g++
```

## Building

`ocamlbuild -lib=llvm main.byte`

Then use `utop` to access the `parse` function.

## Installing OCaml on CentOS 6

TBD (for professor)
