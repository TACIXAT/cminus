# cminus

This project is to implement a compiler from C- to LLVM IR. C Minus is a subset of C, defined in the book [Compiler Construction: Principles and Practice by Kenneth C. Louden](http://www.cs.sjsu.edu/~louden/cmptext/). 

This project will initially implement the TINY language, also defined in Compiler Construction. A tag will be made when a full compiler for TINY is completed.

This is my first project in OCaml, so pardon the mess.

## Resources

I'm using the [Cornell's CS3110 parsing code](http://www.cs.cornell.edu/courses/cs3110/2015fa/l/12-interp/rec.html) as my basis. This was the cleanest code I found that defined a language and produced an AST. The comments are very thorough (though I will delete most of them as time goes on). It uses the [menhir](http://gallium.inria.fr/~fpottier/menhir/) parser generator.

A good resource for code is the [LLVM Kaleidoscope OCaml tutorial](http://llvm.org/docs/tutorial/OCamlLangImpl1.html). It uses camlp4 and the code is more complex than the Cornell tutorial. It includes examples of code generation and other further pieces of a compiler.

The [documentation for the LLVM OCaml bindings](https://llvm.moe/ocaml/). 

[Part 3](https://www.wzdftpd.net/blog/ocaml-llvm-03.html) of @chifflier's OCaml LLVM bindings tutorial is very helpful for code generation.

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

`ocamlbuild -lib=llvm -use-ocamlfind -pkg core -tag thread  main.byte`

## Installing LLVM / OCaml on Ubuntu 16.04

### Install LLVM, OCaml, Dependencies 

    wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key | sudo apt-key add -
    [ENTER YOUR PASSWORD]
    sudo apt-add-repository "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-3.9 main"
    sudo apt-get update
    sudo apt-get install llvm-3.9 ocaml clang-3.9
    sudo apt-get install git opam m4 pkg-config cmake

### OCaml Setup and Project Dependencies

    opam init # answer yes
    eval `opam config env`
    opam install core menhir ctype ctype-foreign llvm

### Project Build and Run

    git clone https://github.com/douggard/cminus.git
    cd cminus
    ocamlbuild -use-ocamlfind -pkg core -pkg llvm -tag thread  main.byte
    cd samples/fact
    ~/cminus/main.byte 2> fact.ll
    clang-3.9 fact.ll ../io.c
    ./a.out

