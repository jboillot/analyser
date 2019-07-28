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

open SortedList

(** Types used during the analysis *)

(** Name of variables, functions and labels *)
type name_var = string

(** Locations of statements in programs *)
type loc = Loc_c of int | Loc_a of int

(** Type of C variables *)
type type_c = Int_c | Ptr of type_c

(** Binary operators which are of type bool -> bool -> bool *)
type boolOpBoolBin = And | Or

(** Binary operators which are of type int -> int -> bool *)
type boolOpArithBin = Eq | Neq | Lt | Gt | Le | Ge

(** Binary operators which are of type int -> int -> int *)
type arithOpArithBin = Plus | Minus | Times | Div

(** Unary operators which are of type bool -> bool *)
type boolOpBoolUna = Not

(** Unary operators which are of type int -> bool *)
type arithOpArithUna = MinusUna | BitwiseNot

(** Registers actually implemented in the analyser *)
type reg32 = EAX | EBX | ECX | EDX

(** ASM expression aka operands of ASM statements *)
type asm_expr = AsmReg of reg32 | AsmPtr of asm_expr | AsmVar of name_var | AsmCst of Z.t | AsmLbl of name_var | AsmLblLoc of loc

(** C statements *)
type stat_c = Declare of type_c * name_var | DeclareAssign of type_c * name_var * arith_expr
            | Assign of name_var * arith_expr | Return of arith_expr
            | Branch of bool_expr * loc_stat_c * loc_stat_c | BranchWithoutLoc of bool_expr * stat_c list * stat_c list
            | Asm of loc_stat_a | AsmWithoutLoc of stat_a list
            | While of bool_expr * loc_stat_c | WhileWithoutLoc of bool_expr * stat_c list
            | CallAssignC of name_var * name_var * arith_expr list | CallAssignAsm of name_var * loc * arith_expr list
            | PtrAssign of ptr_var * arith_expr

(** ASM statements *)
and stat_a = Label of name_var | Jmp of asm_expr | Mov of (asm_expr * asm_expr) | Ret | Call of asm_expr
           | Push of asm_expr | Pop of asm_expr

(** Located C block *)
and loc_stat_c = loc * (stat_c * loc) list

(** Located ASM block *)
and loc_stat_a = loc * (stat_a * loc) list

(** C arithmetic expression *)
and arith_expr = Var of name_var | Deref of arith_expr | Addr of name_var | Cons of Z.t | Interval of Z.t * Z.t
               | ArithArithBinExpr of arith_expr * arithOpArithBin * arith_expr
               | ArithArithUnaExpr of arithOpArithUna * arith_expr

(** C boolean expressions *)
and bool_expr = True | False | BoolBinExpr of bool_expr * boolOpBoolBin * bool_expr
              | BoolUnaExpr of boolOpBoolUna * bool_expr
              | BoolArithBinExpr of arith_expr * boolOpArithBin * arith_expr

(** Definition of the arguments of C functions *)
type arguments = (type_c * name_var) list

(** Statements which are globals. So far, functions. *)
type global_statement = Function of (type_c * name_var * arguments * loc_stat_c) | FunctionWithoutLoc of (type_c * name_var * arguments * stat_c list)

(** C program with inline assembly *)
type program = loc * (global_statement * loc) list

(** Statement with its location associated *)
type generic_stat_loc = StatC of (loc * stat_c * loc) | StatA of (loc * stat_a * loc)

(** Abstract domain *)
type abstrD = AbstrD of (Z.t * Z.t) | AbstrPtr of sortedList | AbstrLoc of loc | AbstrBot

(** Real domain *)
type realD = RealD of Z.t list | RealBot

(** Minimum number *)
let abstrMin : Z.t = Z.of_int (-2147483648)

(** Maximum number *)
let abstrMax : Z.t = Z.of_int 2147483647

(** Top element of the abstract domains *)
let abstrTop : abstrD = AbstrD(abstrMin, abstrMax)

(** Map with variable names as key *)
module Context = Map.Make(String)

(** Context *)
type abstrContext = AbstrContext of abstrD Context.t | AbstrContextBot

(** Memory stack *)
type memoryStack = abstrD list

(** Map with locations as keys *)
module Environment = Map.Make(struct type t = loc let compare = compare end)

(** Environment *)
type abstrLocContext = AbstrEnvironment of abstrContext Environment.t | GoForward of loc * abstrContext * abstrLocContext | AbstrEnvironmentBot

(** Set (in fact map) of all known C functions *)
type functionContext = global_statement Environment.t

(** Call stack (which is really a stack) *)
type callStack = ((bool * abstrD * abstrContext * memoryStack)) Stack.t

(** Does the join function have to return bottom *)
exception ShouldBeBottom

(** A return/ret instruction has been encountered and the function/block of statements is returned with the abstract domain of the value returned, the context and the memory stack associated *)
exception ReturnRaised of abstrD * abstrContext * memoryStack

(** Do we have to go back in the past to start again a branch with more anticipation of a jump *)
exception GoBackward

(** Set (in fact map) of all the labels which have been encountered with their locations associated *)
let labels : loc Context.t ref = ref Context.empty