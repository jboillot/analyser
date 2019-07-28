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

let minmax l : (Z.t * Z.t) =
  let rec minmax_aux (l : Z.t list) (last_min, last_max : Z.t * Z.t) : Z.t * Z.t =
    match l with
    | [] -> (last_min, last_max)
    | h::t -> minmax_aux t (Z.min h last_min, max h last_max)
  in match l with
  | [] -> failwith "minmax: empty list given!"
  | h::t -> minmax_aux t (h, h)

let rec notBoolExpr b : bool_expr =
  match b with
  | True -> False
  | False -> True
  | BoolBinExpr(b1, op, b2) ->
    BoolBinExpr(notBoolExpr b1, begin match op with And -> Or | Or -> And end, notBoolExpr b2)
  | BoolUnaExpr(op, b1) -> begin match op with Not -> b1 end
  | BoolArithBinExpr(a1, op, a2) ->
    BoolArithBinExpr(a1, begin match op with Eq -> Neq | Neq -> Eq | Lt -> Ge | Le -> Gt | Gt -> Le | Ge -> Lt end, a2)

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

let invMin a b =
  let s1 = (fun x -> if x = 0 then 1 else x) (Z.sign a) in
  let s2 = (fun x -> if x = 0 then 1 else x) (Z.sign b) in
  if s1 = s2
  then max a b
  else min a b

let invMax a b =
  let s1 = (fun x -> if x = 0 then 1 else x) (Z.sign a) in
  let s2 = (fun x -> if x = 0 then 1 else x) (Z.sign b) in
  if s1 = s2
  then min a b
  else max a b

let removeCVariables context : abstrContext =
  match context with
  | AbstrContext(map) ->
    let map' = Context.filter (fun vn _ -> is_register vn) map in
    if Context.is_empty map' then AbstrContextBot else AbstrContext(map')
  | _ -> context

let getLocationNumber l : int =
  match l with
  | Loc_a(n) | Loc_c(n) -> n

let getRegNameVar reg : name_var =
  match reg with
  | EAX -> eax_reg
  | EBX -> ebx_reg
  | ECX -> ecx_reg
  | EDX -> edx_reg