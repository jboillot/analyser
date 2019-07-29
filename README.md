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

- There is just one little file (currently ~13% of the project in term of lines of code) which implements the abstract domain that I have chosen (intervals) and the other files don't rely on this choice.
- Analysis of basic C code with arithmetic operations, boolean expressions, assignments, function calls, pointer assignments, `if then else` statements (aka branch), while loops, ternary operators with parenthesis around the boolean expression and `return` statements with parameter and absence of `return`.
- Analysis of basic ASM code: `jmp` to a location in the same function, in another branch, in another function, but only in inline ASM code because we can't assume the length that the C code could take.
- Analysis of `while`/`for` statements will always finish because of the widening function.
- ASM functions can be called in C and vice versa.
- The ASM functions can change the return point and it will be taken into account.
- Pointers given to functions as parameters can be modified and the side effects will be taken into account.
- Variable can be declared wherever you want (for exemple in a branch or at the end of a program).
- `else if` and `for` statements are implemented because transformed into already implemented statements.

### Features that I would have liked to implement

- The recursivity can be used but it can not finish because it doesn't use the widening function. I think that it is really hard to correct that point.
- Types are not taken into account, and they are only two: `int` and pointers to implemented types. There are no `struct` or `union` types.
- `JMP` to relative positions are not implemented yet.
- ASM functions/loops can not terminate because the widening function is not used. I think that it is really hard to correct that point. However, we can consider that loops or functions should be made in C with an ASM body.
- The division in boolean expressions is not implemented yet.
- There are no boolean expressions with side effect but with my new version of the environments, it's relatively easy to add it.
- Turning every boolean expression into arithmetic expression in order to be able to parse more programs and to make the parser easier to read.

## License

This project is licensed under the terms of the MIT license — see the [LICENSE](https://github.com/jboillot/analyser/blob/master/LICENSE) file for details
