# Analyser

Analyser is a little and without warranty program which performs a static interpretation of simple C programs with inline assembly in order to find errors or undefined behaviours in a finite time. It has been made during the internship of Jérôme Boillot at the end of his bachelor in the ANTIQUE team located in the ENS in Paris under the direction of [Marc Chevalier](https://marc-chevalier.com/) and [Jérôme Ferret](https://www.di.ens.fr/~feret/).
The documentation of this analyser is available [here](https://jboillot.github.io/analyser/).

## Built with

- [OCaml 4.05.0](https://github.com/ocaml/ocaml)
- [dune 1.10.0](https://github.com/ocaml/dune)
- [menhir 20190620](https://gitlab.inria.fr/fpottier/menhir)
- [Zarith 1.7](https://github.com/ocaml/Zarith)

## Current state of the project

### Features already implemented

- There is just one little file (currently ~10% of the project in term of lines of code) which implements the abstract domain that I have chosen (intervals) and the other files don't depend on this choice.
- Analysis of basic C code with arithmetic operations, boolean expressions, assignments, function calls, simple pointer assignments, `if then else` statements (aka branch), while loops, `return` statements with parameter and absence of `return`.
- Analysis of basic ASM code: `jmp` to a location in the same function, in another branch, in another function, but only in inline ASM code because we can't assume the length that the C code could take.
- Analysis of `while` statements will always finish because of the widening function.
- ASM functions can be called in C and vice versa.
- The ASM functions can change the return point and it will be taken into account.

### Features that I would have liked to implement

- The recursivity can be used but it can not finish because it doesn't use the widening function. I think that it is really hard  correct that point.
- Types are not taken into account, and they are only two. There are no `struct` or `union` types.
- Only two types have been implemented : `int` and pointers to implemented types.
- Variables have to be declared at the beginning of the functions. In fact, if you declare a variable in a branch, the variable will continue to exist after the end of the branch and will be usable.
- `else if` statements or `JMP` to relative positions are not implemented because I've just implemented the join of two locations with the branch, not more.
- ASM functions can not terminate because the widening function is not used. I think that it is really hard to correct that point.
- The division in boolean expressions is not implemented yet.
- There are no boolean expressions with side effect because it is more difficult to modelize.
- ASM loops don't assure their termination because the widening function is not used. However, we can consider that loops should be made in C with an ASM body.

## License

This project is licensed under the terms of the MIT license — see the [LICENSE](https://github.com/jboillot/analyser/blob/master/LICENSE) file for details
