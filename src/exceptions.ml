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

open Types
open Debug
open Options

(** Module which is used to raise or handle non-fatal exceptions *)

(** The possible types of the exceptions *)
type exception_type = DIVIDE_BY_ZERO | OVERFLOW | PTRINTMATCH | UNDEFINEDVAR | ALREADYDEFINEDVAR | WRONGSTACKOFFSET | WRONGJMP | NONEMPTYCALLSTACK | LOCMATCH

(** The informations which will be stored about an exception *)
type except = exception_type * generic_stat_loc

(** The list of the current exceptions *)
let exceptions : except list ref = ref []

(** Give the description of each exception type
    @param t The type of the exception
    @return The description of this exception *)
let exception_type2string t : string =
  match t with
  | DIVIDE_BY_ZERO -> "division by zero"
  | OVERFLOW -> "overflow"
  | PTRINTMATCH -> "try to match pointer and int"
  | UNDEFINEDVAR -> "a variable is not declared in the context"
  | ALREADYDEFINEDVAR -> "a variable has already been defined"
  | WRONGSTACKOFFSET -> "the stack offset is not equal to 0 at the end of an ASM block"
  | WRONGJMP -> "this jump statement is invalid!"
  | NONEMPTYCALLSTACK -> "the callstack is not empty!"
  | LOCMATCH -> "try to match location with another thing"

(** Handle an exception and print the associated error
    @param exception The exception
    @return unit *)
let handleException except : unit =
  let (l1_str, s_str, l2_str) =
    match snd except with
    | StatC(l1, Branch(b, _, _), l2) -> (loc2string l1, bool_expr2string b, loc2string l2)
    | StatC(l1, Asm(_), l2) -> (loc2string l1, "asm(", loc2string l2)
    | StatC(l1, While(b, _), l2) -> (loc2string l1, bool_expr2string b, loc2string l2)
    | StatC(l1, s, l2) -> (loc2string l1, stat_c2string s "", loc2string l2)
    | StatA(l1, s, l2) -> (loc2string l1, stat_a2string s "", loc2string l2)
  in
  Printf.eprintf "Type: %s, statement: %s %s %s\n" (except |> fst |> exception_type2string) l1_str s_str l2_str

(** Raise an exception
    @param t The type of the exception
    @param generic_stat The located statement in which the exception has been raised
    @return unit *)
let raiseException t generic_stat : unit =
  exceptions := (t, generic_stat)::!exceptions;
  if !verbose then
    (handleException (t, generic_stat);
     Printf.eprintf "%s\n" (Printexc.get_callstack 5 |> Printexc.raw_backtrace_to_string))
  else
    ()

(** Handle every exception in chronological order
    @return unit *)
let handleExceptions () : unit =
  let rec handleExceptionsList (l : except list) : unit =
    match l with
    | [] -> ()
    | h::t -> handleException h; handleExceptionsList t
  in handleExceptionsList (List.rev !exceptions)