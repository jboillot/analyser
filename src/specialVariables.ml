(*
Analyser is a static analyser which finds errors and undefinded behaviors in C programs with inline assembly.

Copyright 2019 Jérôme Boillot

Permission is hereby granted, free of charge, to any person obtaining a copy of this
software and associated documentation files (the "Software"), to deal in the Software
without restriction, including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
*)

(** Module to store the special variables which are used to store registers in contexts *)

(** Variable name of the register EAX *)
let eax_reg = "#REG_EAX"

(** Variable name of the register EBX *)
let ebx_reg = "#REG_EBC"

(** Variable name of the register ECX *)
let ecx_reg = "#REG_ECX"

(** Variable name of the register EDX *)
let edx_reg = "#REG_EDX"

(** Is a variable name a register's one
    @param vn The name of the variable
    @return Is [vn] the name of a register *)
let is_register vn : bool =
  match String.sub vn 0 5 with
  | exception _ -> false
  | "#REG_" -> true
  | _ -> false