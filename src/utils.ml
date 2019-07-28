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
open SpecialVariables

(** Utilitary function in order to achieve static interpretation *)

(** Return the minimum and the maximum of a list. Primitive recursive function.
    @param l The list of numbers
    @raise Failure If the list is empty
    @return The pair made of the minimum and the maximum of the elements of the list *)
let minmax l : (Z.t * Z.t) =
  let rec minmax_aux (l : Z.t list) (last_min, last_max : Z.t * Z.t) : Z.t * Z.t =
    match l with
    | [] -> (last_min, last_max)
    | h::t -> minmax_aux t (Z.min h last_min, max h last_max)
  in match l with
  | [] -> failwith "minmax: empty list given!"
  | h::t -> minmax_aux t (h, h)

(** Return the last location of a program
    @param program The block of C/ASM located statements
    @return The last location of the block *)
let lastLocOfProgram program : loc =
  let rec lastLoc_aux (program_aux : ('a * loc) list) : loc =
    match program_aux with
    | [_, l] -> l
    | _::t -> lastLoc_aux t
    | [] -> fst program
  in lastLoc_aux (snd program)

(** Apply the not function to a boolean 
    @param b The initial boolean expression
    @return The boolean expression on which we have apply a not *)
let rec notBoolExpr b : bool_expr =
  match b with
  | True -> False
  | False -> True
  | BoolBinExpr(b1, op, b2) ->
    BoolBinExpr(notBoolExpr b1, begin match op with And -> Or | Or -> And end, notBoolExpr b2)
  | BoolUnaExpr(op, b1) -> begin match op with Not -> b1 end
  | BoolArithBinExpr(a1, op, a2) ->
    BoolArithBinExpr(a1, begin match op with Eq -> Neq | Neq -> Eq | Lt -> Ge | Le -> Gt | Gt -> Le | Ge -> Lt end, a2)

(** Adds locations to an unlocated program
    @param program The unlocated program
    @return The program located *)
let addLocationsProgram program : program =
  let rec addLocationsProgramAsm (stats : stat_a list) (i : int) : (stat_a * loc) list * int =
    match stats with
    | [] -> [], i
    | Label(ln)::t -> labels := Context.add ln (Loc_a(i-1)) !labels; addLocationsProgramAsm t i
    | h::t ->
      let (p1, i1) = addLocationsProgramAsm t (i+1) in
      (h, Loc_a(i))::p1, i1
  in
  let rec addLocationsProgramC (stats : stat_c list) (i : int) : (stat_c * loc) list * int =
    match stats with
    | [] -> [], i
    | Branch(_,_,_)::_ -> failwith "addLocationsProgram: Branch found in the presupposed without locations program!"
    | Asm(_)::_ -> failwith "addLocationsProgram: Asm found in the presupposed without locations program!"
    | While(_, _)::_ -> failwith "addLocationsProgram: While found in the presupposed without locations program!"
    | BranchWithoutLoc(b, l1, l2)::t ->
      let (p1, i1) = addLocationsProgramC l1 (i+1) in
      let (p2, i2) = addLocationsProgramC l2 (i1+1) in
      let (p3, i3) = addLocationsProgramC t (i2+1) in
      (Branch(b, (Loc_c(i), p1), (Loc_c(i1), p2)), Loc_c(i2))::p3, i3
    | AsmWithoutLoc(l1)::t ->
      let (p1, i1) = addLocationsProgramAsm l1 (i+1) in
      let (p2, i2) = addLocationsProgramC t (i1+1) in
      (Asm(Loc_a(i), p1), Loc_c(i1))::p2, i2
    | WhileWithoutLoc(b, l1)::t ->
      let (p1, i1) = addLocationsProgramC l1 (i+1) in
      let (p2, i2) = addLocationsProgramC t (i1+1) in
      (While(b, (Loc_c(i), p1)), Loc_c(i1))::p2, i2
    | h::t ->
      let (p1, i1) = addLocationsProgramC t (i+1) in
      (h, Loc_c(i))::p1, i1
  in
  let rec addLocationsFunctions (functions : global_statement list) (i : int) : (global_statement * loc) list * int =
    match functions with
    | [] -> [], i
    | (FunctionWithoutLoc(t, vn, args, stats))::l ->
      let (stats_located, i') = addLocationsProgramC stats (i+1) in
      let (located_l, i'') = addLocationsFunctions l (i'+1) in
      (Function(t, vn, args, (Loc_c(i), stats_located)), Loc_c(i'))::located_l, i''
    | (Function(_,_,_,_))::_ -> failwith "addLocationsFunctions: Function found in the presupposed without locations program!"
  in
  (Loc_c(0), addLocationsFunctions program 1 |> fst)

(** Remove the labels of the program and replace them by the locations associated
    @param program The located program
    @return The located program with the labels which have been replaced *)
let removeLabelsProgram program : program =
  let rec removeLabelsProgramASM (block : (stat_a * loc) list) : (stat_a * loc) list =
    match block with
    | [] -> []
    | (Jmp(AsmLbl(lbl)), l1)::t when Context.mem lbl !labels ->
      (Jmp(AsmLblLoc(Context.find lbl !labels)), l1)::(removeLabelsProgramASM t)
    | (Mov(ae1, AsmLbl(lbl)), l1)::t when Context.mem lbl !labels ->
      (Mov(ae1, AsmLblLoc(Context.find lbl !labels)), l1)::(removeLabelsProgramASM t)
    | (Push(AsmLbl(lbl)), l1)::t when Context.mem lbl !labels ->
      (Push(AsmLblLoc(Context.find lbl !labels)), l1)::(removeLabelsProgramASM t)
    | (Call(AsmLbl(vn)), l1)::t when Context.mem vn !labels -> (Call(AsmLblLoc(Context.find vn !labels)), l1)::(removeLabelsProgramASM t)
    | (Jmp(_), _)::_ -> failwith "removeLabelsProgramASM: Label unknown!"
    | (stat, l1)::t -> (stat, l1)::(removeLabelsProgramASM t)
  in
  let rec removeLabelsProgramC (block : (stat_c * loc) list) : (stat_c * loc) list =
    match block with
    | [] -> []
    | (Branch(b, (l1, block1), (l2, block2)), l)::t ->
      (Branch(b, (l1, removeLabelsProgramC block1), (l2, removeLabelsProgramC block2)), l)::(removeLabelsProgramC t)
    | (While(b, (l1, block1)), l)::t ->
      (While(b, (l1, removeLabelsProgramC block1)), l)::(removeLabelsProgramC t)
    | (Asm(l2, block), l1)::t -> (Asm(l2, removeLabelsProgramASM block), l1)::(removeLabelsProgramC t)
    | (CallAssignC(vn1, vn2, args), l1)::t when Context.mem vn2 !labels -> (CallAssignAsm(vn1, Context.find vn2 !labels, args), l1)::(removeLabelsProgramC t)
    | h::t -> h::(removeLabelsProgramC t)
  in
  let rec removeLabelsFunctions (functions : (global_statement * loc) list) =
    match functions with
    | [] -> []
    | (Function(t, vn, args, (l2, block)), l1)::l -> (Function(t, vn, args, (l2, removeLabelsProgramC block)), l1)::(removeLabelsFunctions l)
    | _ -> failwith "addLocationsFunctions: Unlocated function found!"
  in
  let result = removeLabelsFunctions (snd program) in
  labels := Context.empty;
  ((fst program), result)

(** Returns the inverse of the minimum where a et b are already inversed
    @param a 1/[c]
    @param b 1/[d]
    @return min(1/([c], [d])) *)
let invMin a b =
  let s1 = (fun x -> if x = 0 then 1 else x) (Z.sign a) in
  let s2 = (fun x -> if x = 0 then 1 else x) (Z.sign b) in
  if s1 = s2
  then max a b
  else min a b

(** Returns the inverse of the maximum where a et b are already inversed
    @param a 1/[c]
    @param b 1/[d]
    @return max(1/([c], [d])) *)
let invMax a b =
  let s1 = (fun x -> if x = 0 then 1 else x) (Z.sign a) in
  let s2 = (fun x -> if x = 0 then 1 else x) (Z.sign b) in
  if s1 = s2
  then min a b
  else max a b

(** Remove the statements of a block from an environment
    @param block The block of statements to remove
    @param lastLoc The first location of the block (because this location is not in the block; [lastLoc] is named like that because it is the last location read).
    @param The last environement
    @return The new environment  *)
let rec removeStatementContextBlock block_c lastLoc j : abstrLocContext =
  match block_c with
  | [] -> j
  | (stat, loc)::t -> j |> removeStatementContext (lastLoc, stat) |> removeStatementContextBlock t loc

(** Remove a C statement from an environment
    @param l The location of [stat]
    @param stat The C statement
    @param j The last environment
    @return The new environment *)
and removeStatementContext (l, stat) j : abstrLocContext =
  match j with
  | AbstrEnvironmentBot -> AbstrEnvironmentBot
  | GoForward(_,_,_) -> j
  | AbstrEnvironment(map) ->
    match stat with
    | Branch(_, (l1, block1), (l2, block2)) ->
      AbstrEnvironment(Environment.remove l map) |> removeStatementContextBlock block1 l1 |> removeStatementContextBlock block2 l2
    | While(_, (l1, block1)) ->
      AbstrEnvironment(Environment.remove l map) |> removeStatementContextBlock block1 l1
    | Asm(l, block1) ->
      List.fold_left (fun j l -> removeAsmStatementContext l j) (AbstrEnvironment(Environment.remove l map)) block1
    | _ -> AbstrEnvironment(Environment.remove l map)

(** Remove an ASM statement from an environment
    @param stat The located ASM statement
    @param j The last environment
    @return The new environment *)
and removeAsmStatementContext stat j : abstrLocContext =
  match j with
  | AbstrEnvironmentBot -> AbstrEnvironmentBot
  | GoForward(_,_,_) -> j
  | AbstrEnvironment(map) -> AbstrEnvironment(Environment.remove (snd stat) map)

(** Remove the C variables from a context. Used when we jump in another function and that we have no clue on the variable stack
    @param context The last context
    @return The new context *)
let removeCVariables context : abstrContext =
  match context with
  | AbstrContext(map) ->
    let map' = Context.filter (fun vn _ -> is_register vn) map in
    if Context.is_empty map' then AbstrContextBot else AbstrContext(map')
  | _ -> context

(** Return the number associated to a location
    @param l The location
    @return The number associated *)
let getLocationNumber l : int =
  match l with
  | Loc_a(n) | Loc_c(n) -> n

(** Return the variable name of a register
    @param reg The register
    @return It's name *)
let getRegNameVar reg : name_var =
  match reg with
  | EAX -> eax_reg
  | EBX -> ebx_reg
  | ECX -> ecx_reg
  | EDX -> edx_reg