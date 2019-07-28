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
open SpecificAbstract
open Utils
open Debug
open Exceptions
open SortedList
open SpecialVariables

(** Static interpretation methods which are not dependant to the abstraction domain chosen. *)

(* Generic methods *)

(** Test the equality of two contexts
    @param c1 The first context
    @param c2 The second context
    @return Are the contexts equal *)
let areContextEqual c1 c2 : bool =
  match (c1, c2) with
  | (AbstrContextBot, AbstrContextBot) -> true
  | (AbstrContext(map1), AbstrContext(map2))-> Context.compare compare map1 map2 = 0
  | (_, _) -> false

(** Test equality of two environments
    @param e1 The first environment
    @param e2 The second environment
    @return Are the environment equal *)
let rec areEnvironmentEqual e1 e2 : bool =
  match (e1, e2) with
  | (AbstrEnvironmentBot, AbstrEnvironmentBot) -> true
  | (GoForward(l1, c1, j1), GoForward(l2, c2, j2)) -> l1 = l2 && areContextEqual c1 c2 && areEnvironmentEqual j1 j2
  | (AbstrEnvironment(map1), AbstrEnvironment(map2)) -> Environment.equal areContextEqual map1 map2
  | (_, _) -> false

(** Joins two contexts by joining all the abstract domains of their variables
    @param a1 The first context
    @param a2 The second context
    @return The context made of the join of the two last contexts *)
let contextJoin a1 a2 : abstrContext =
  (* To be quicker *)
  if areContextEqual a1 a2
  then a1
  else
    match (a1, a2) with
    | (AbstrContextBot, other) | (other, AbstrContextBot) -> other
    | (AbstrContext(map1), AbstrContext(map2)) ->
      try
        let contextJoinMerge (_ : Context.key) (a1 : abstrD option) (a2 : abstrD option) : abstrD option =
          match (a1, a2) with
          | (Some(AbstrBot), _) | (_, Some(AbstrBot)) -> raise ShouldBeBottom
          | (None, other) | (other, None) -> other
          | (Some(a1'), Some(a2')) -> Some(join a1' a2')
        in AbstrContext(Context.merge contextJoinMerge map1 map2)
      with ShouldBeBottom -> AbstrContextBot

(** Meets two contexts by meeting all the abstract domains of their variables
    @param a1 The first context
    @param a2 The second context
    @return The context made of the meet of the two last contexts *)
let contextMeet a1 a2 : abstrContext =
  match (a1, a2) with
  | (AbstrContextBot, _) | (_, AbstrContextBot) -> AbstrContextBot
  | (AbstrContext(map1), AbstrContext(map2)) ->
    try
      let contextMeetMerge (_ : Context.key) (a1 : abstrD option) (a2 : abstrD option) : abstrD option =
        match (a1, a2) with
        | (None, _) | (_, None) -> None
        | Some(a1'), Some(a2') ->
          match meet a1' a2' with
          | AbstrBot -> raise ShouldBeBottom
          | result -> Some(result)
      in AbstrContext(Context.merge contextMeetMerge map1 map2)
    with ShouldBeBottom -> AbstrContextBot

(** Apply widening on two contexts by applying the widening to all the abstract domains of their variables 
    @param a1 The first context
    @param a2 The second context
    @return The context made of the application of the widening of the two last contexts *)
let wideningOnContexts a1 a2 : abstrContext =
  (* To be quicker *)
  if a1 = a2
  then a1
  else
    match (a1, a2) with
    | (AbstrContextBot, other) | (other, AbstrContextBot) -> other
    | (AbstrContext(map1), AbstrContext(map2)) ->
      try
        let contextWideningMerge (_ : Context.key) (a1 : abstrD option) (a2 : abstrD option) : abstrD option =
          match (a1, a2) with
          | (Some(AbstrBot), _) | (_, Some(AbstrBot)) -> raise ShouldBeBottom
          | (None, other) | (other, None) -> other
          | (Some(a1'), Some(a2')) -> Some(widening a1' a2')
        in AbstrContext(Context.merge contextWideningMerge map1 map2)
      with ShouldBeBottom -> AbstrContextBot

(** Apply widening on two environments by applying the widening to all the contexts
    @param e1 The first environment
    @param e2 The second environment
    @return The environment made of the application of the widening of the two last environments *)
let wideningOnEnvironments e1 e2 : abstrLocContext =
  let environmentWideningMerge (_ : Environment.key) (a1 : abstrContext option) (a2 : abstrContext option) : abstrContext option =
    match (a1, a2) with
    | (None, other) | (other, None) -> other
    | (Some(a1'), Some(a2')) -> Some(wideningOnContexts a1' a2')
  in
  match (e1, e2) with
  | (AbstrEnvironmentBot, other) | (other, AbstrEnvironmentBot) -> other
  | (GoForward(_,_,_), _) | (_, GoForward(_,_,_)) -> failwith "wideningOnEnvironments: Not sure what to do!"
  | (AbstrEnvironment(map1), AbstrEnvironment(map2)) ->
    AbstrEnvironment(Environment.merge environmentWideningMerge map1 map2)

(** Joins two environment by joining all the contexts
    @param e1 The first environment
    @param e2 The second environment
    @return The environment made of the join of the two last environments *)
let environmentJoin e1 e2 : abstrLocContext =
  let environmentJoinMerge (_ : Environment.key) (a1 : abstrContext option) (a2 : abstrContext option) : abstrContext option =
    match (a1, a2) with
    | (None, other) | (other, None) -> other
    | (Some(a1'), Some(a2')) -> Some(contextJoin a1' a2')
  in
  match (e1, e2) with
  | (AbstrEnvironmentBot, other) | (other, AbstrEnvironmentBot) -> other
  | (GoForward(_,_,_), _) | (_, GoForward(_,_,_)) -> failwith "environmentJoin: Not sure what to do!"
  | (AbstrEnvironment(map1), AbstrEnvironment(map2)) ->
    AbstrEnvironment(Environment.merge environmentJoinMerge map1 map2)

(** Replaces a context at a location by another on and returns the new environment
    @param l The location where the context will be set
    @param context The context to set
    @param j The environment in which this change will be performed
    @return The resulting environment *)
let replaceContext l context j : abstrLocContext =
  match j with
  | AbstrEnvironmentBot -> AbstrEnvironmentBot
  | GoForward(_,_,_) -> j
  | AbstrEnvironment(mapEnv) ->
    AbstrEnvironment(Environment.add l context mapEnv) 

(** Returns the context at a locations or initialize it to bottom ans return the new environment too
    @param l The location where the context will be get
    @param j The environment in which the context will be get
    @return The pair of the context found and the possibily new environment in which the context has been created *)
let getContext l j : abstrContext * abstrLocContext =
  match j with
  | AbstrEnvironment(map) when Environment.mem l map -> (Environment.find l map, j)
  | AbstrEnvironment(map) ->
    let newContext = AbstrContextBot in
    (newContext, AbstrEnvironment(Environment.add l newContext map))
  | GoForward(_,_,_) -> (AbstrContextBot, j)
  | AbstrEnvironmentBot -> (AbstrContextBot, AbstrEnvironmentBot)

(** Return a variable at a location in an environment
    @param l The location where the variable will be searched
    @param vn The name of the variable
    @param j The environment in which the variable will be searched
    @raise UNDEFINEDVAR if the variable is not declared at the location specified
    @return The abstract domain of the variable at the location specified *)
let getVar l vn j : abstrD =
  match getContext l j |> fst with
  | AbstrContextBot -> AbstrBot
  | AbstrContext(map) when Context.mem vn map -> Context.find vn map
  | AbstrContext(_) -> raiseException UNDEFINEDVAR !lastStat; abstrTop

(** Calculates the least fixpoint of an environment by a function
    @param e1 The function to apply on the environment until we have fixpoint
    @param j The initial environment
    @param The intiial memory stack
    @return The least fixpoint of [x] by [e1] *)
let rec lfp e1 (j, lastStack) : abstrLocContext * memoryStack =
  if j = AbstrEnvironmentBot
  then (AbstrEnvironmentBot, lastStack)
  else
    let (nextJ, nextStack) = e1 (j, lastStack) in
    match nextJ with
    | AbstrEnvironmentBot -> (j, lastStack)
    | GoForward(_,_,_) -> (nextJ, nextStack)
    | _ when !Options.withoutWidening1 ->
      if areEnvironmentEqual j nextJ
      then (j, lastStack)
      else lfp e1 (environmentJoin j nextJ, nextStack)
    | _ when !Options.withoutWidening2 ->
      if areEnvironmentEqual j nextJ
      then (j, lastStack)
      else
        let (finalJ, finalStack) = lfp e1 (nextJ, nextStack) in
        (environmentJoin j finalJ, finalStack)
    | _ ->
      let newEnv = wideningOnEnvironments j nextJ in
      if areEnvironmentEqual j newEnv
      then (j, lastStack)
      else lfp e1 (newEnv, nextStack)

(** Add or update the abstract value of a variable in an environment at a location
    @param addMode Do we have to add or update the variable
    @param l The location where the variable has to be added/updated
    @param vn The name of the variable
    @param newValue The value of the variable that we want to set
    @param j The environment
    @raise ALREADYDEFINED if the addMode is set to true and that the variable already exists at the specified location
    @raise UNDEFINEDVAR if the addMode is set to false and that the variable doesn't exist at the specified location
    @return The new environment in which the value of the variable has been added/updated *)
let addVarToContextOrUpdateVarInContext addMode l vn newValue j : abstrLocContext =
  match j with
  | AbstrEnvironmentBot -> AbstrEnvironmentBot
  | GoForward(_,_,_) -> failwith "addVarToContexOrUpdateVarInContext: Not sure what to do!"
  | AbstrEnvironment(mapEnv) ->
    let lastContext =
      if Environment.mem l mapEnv
      then Environment.find l mapEnv
      else AbstrContextBot
    in
    let newContext =
      match lastContext with
      | AbstrContextBot -> AbstrContextBot
      | AbstrContext(mapContext) ->
        if addMode then
          begin
            if Context.mem vn mapContext
            then (raiseException ALREADYDEFINEDVAR !lastStat; lastContext)
            else AbstrContext(Context.add vn newValue mapContext)
          end
        else
          begin
            if Context.mem vn mapContext
            then AbstrContext(Context.add vn newValue mapContext)
            else (raiseException UNDEFINEDVAR !lastStat; lastContext)
          end
    in
    AbstrEnvironment(Environment.add l newContext mapEnv)

(** Adds an undefined variable to a context and returns the new one
    @param l The location where the variable has to be added
    @param vn The name of the variable
    @param newValue The value of the variable that we want to set
    @param j The environment
    @raise ALREADYDEFINED if the variable already exists at the specified location
    @return The new environment in which the value of the variable has been added *)
let addVarToContext = addVarToContextOrUpdateVarInContext true

(** Update an existing variable in a context and returns the new one
    @param l The location where the variable has to be updated
    @param vn The name of the variable
    @param newValue The value of the variable that we want to set
    @param j The environment
    @raise UNDEFINEDVAR if the variable doesn't exist at the specified location
    @return The new environment in which the value of the variable has been updated *)
let updateVarInContext = addVarToContextOrUpdateVarInContext false

(** Removes (if it exists) a variable from a context and return the new one
    @param l The location where the variable has to be removed
    @param vn The name of the variable
    @param j The environment
    @return The new environment where the variable specified at the location has been removed *)
let removeVarToContext l vn j : abstrLocContext =
  match j with
  | AbstrEnvironmentBot -> AbstrEnvironmentBot
  | GoForward(_,_,_) -> failwith "removeVarToContext: Not sure what to do!"
  | AbstrEnvironment(mapEnv) ->
    let lastContext =
      if Environment.mem l mapEnv
      then Environment.find l mapEnv
      else AbstrContextBot
    in
    let newContext =
      match lastContext with
      | AbstrContextBot -> AbstrContextBot
      | AbstrContext(mapContext) -> AbstrContext(Context.remove vn mapContext)
    in
    AbstrEnvironment(Environment.add l newContext mapEnv)

(** Get the abstract interpretation of an ASM expression at a location
    @param l The location
    @param ae The ASM expression
    @param The environment
    @return The abstract interpretation of the ASM expression *)
let getAsmValue l ae j : abstrD =
  match ae with
  | AsmReg(r') -> getVar l (getRegNameVar r') j
  | AsmVar(vn) -> getVar l vn j
  | AsmCst(cst) -> alpha (RealD([cst]))
  | AsmLbl(_) -> failwith "getAsmValue: Not supposed to have labels in located programs!"
  | AsmLblLoc(loc) -> AbstrLoc(loc)
  | AsmPtr(ae') ->
    match ae' with
    | AsmReg(r') -> AbstrPtr([LastPtrVar(getRegNameVar r')])
    | AsmVar(vn) -> AbstrPtr([LastPtrVar(vn)])
    | _ -> failwith "getAsmValue: Invalid pointer of ASM expression!"

(** Get the C function in which a location is
    @param l The location
    @param f The reference to the set of the C functions which are known
    @return The function as a global statement *)
let getLastFunction l f : global_statement =
  let funLoc = ref (Loc_c(-1)) in
  let funContext = ref (FunctionWithoutLoc(Int_c, "This function is an error! Not supposed to happen", [], [])) in
  let getLastFunctionIterator (l' : loc) lastFun : unit =
    if getLocationNumber l' <= getLocationNumber l && getLocationNumber l' > getLocationNumber !funLoc
    then (funLoc := l'; funContext := lastFun)
  in
  Environment.iter getLastFunctionIterator !f;
  if !funLoc <> (Loc_c(-1))
  then !funContext
  else failwith "getLastFunction: No function found before a location!"

(** Return true iff the location is in a branch which has been explored before
    @param l The location
    @param last_fun The function in which the location is (see getLastFunction)
    @param j The environment
    @return Is the location in a branch explored before *)
let isInBranch l last_fun j : bool =
  let lastLoc = ref l in
  let rec isInBranchRecursive (stats : (stat_c * loc) list) : bool =
    match stats with
    | (Branch(_,_,_), l1)::t when getLocationNumber l1 >= getLocationNumber l ->
      begin match getContext !lastLoc j |> fst with
        | AbstrContext(_) -> true
        | _ -> lastLoc := l1; isInBranchRecursive t
      end
    | (_, l1)::_ when getLocationNumber l1 > getLocationNumber l -> false
    | (_,l1)::t -> lastLoc := l1; isInBranchRecursive t
    | [] -> false
  in
  match last_fun with
  | Function(_,_,_,(_,stats)) -> isInBranchRecursive stats
  | FunctionWithoutLoc(_,_,_,_) -> failwith "isInBranch: I found a FunctionWithoutLoc in a located program!"

(** Join the abstract values of two memory stacks
    @param s1 The first memory stack
    @param s2 The second memory stack
    @raise WRONGSTACKOFFSET If the stacks don't have the same length
    @return The memory stack in which each element is the join of the initials memory stacks *)
let rec joinMemoryStack s1 s2 : memoryStack =
  match (s1, s2) with
  | ([], []) -> []
  | (h1::t1, h2::t2) -> (join h1 h2)::(joinMemoryStack t1 t2)
  | _ -> raiseException WRONGSTACKOFFSET !lastStat; []

(** Initialize the call stack in order to prepare it before the call of a function
    @param cCall Is it a call to a C function (respectively ASM function)
    @param callStack A reference to the call stack
    @return unit *)
let initFunction cCall callStack : unit =
  Stack.push (cCall, AbstrBot, AbstrContextBot, []) !callStack

(** Remove the registers from a context after an ASM block and checks that the memory stack is empty
    @param context The context
    @param stack The memory stack
    @raise WRONGSTACKOFFSET If the memory stack is not empty
    @return The new context without the registers' values *)
let postlude_asm context stack : abstrContext =
  match context with
  | AbstrContextBot -> context
  | AbstrContext(_) when stack <> [] ->
    raiseException WRONGSTACKOFFSET !lastStat; context
  | AbstrContext(map) ->
    AbstrContext(
      map
      |> Context.remove eax_reg
      |> Context.remove ebx_reg
      |> Context.remove ecx_reg
      |> Context.remove edx_reg
    )

(** Raise a return (called by return/ret statement), join the return code, the return context and the memory stack with the last ones saved
    @param cCall Is the statement which calls it a return (respectively ret)
    @param addReturnPossibility Do we have to wait for another return/ret
    @param returnContext The context to return
    @param callStack A reference to the actual call stack
    @param stack The memory stack to return
    @raise Failure If you try to return a C function in ASM and vice versa
    @return unit *)
let raiseReturn cCall addReturnPossibility returnCode returnContext callStack stack : unit =
  match Stack.top !callStack with
  | exception Stack.Empty -> failwith "raiseReturn: Return raised but the call stack is empty!"
  | (cCall',_,_,_) when cCall <> cCall' ->  failwith "raiseReturn: You can't return a C function in ASM and vice versa!"
  | (_, returnCode', returnContext', stack') when not addReturnPossibility ->
    (* If the context is bottom we don't check the join for the memory stack because it is useless, we already have an error or this is the firt call of raiseReturn
       This feature allows ASM->ASM calls: the memory stack is not empty when the RET instruction is called. *)
    raise (ReturnRaised(join returnCode returnCode', contextJoin returnContext returnContext', if returnContext' = AbstrContextBot then stack else joinMemoryStack stack stack'))
  | (_, returnCode', returnContext', stack') ->
    let _ = Stack.pop !callStack in
    Stack.push (cCall, join returnCode returnCode', contextJoin returnContext returnContext', joinMemoryStack stack stack') !callStack

(** Generic abstract interpretation of binary boolean operators
    @param c1 The possible context in which the first operand is satisfied
    @param op The binary boolean operator
    @param c2 The possible context in which the second operand is satisfied
    @return The context in which the whole boolean operation is satisfied *)
let abexpBoolBin c1 op c2 : abstrContext =
  match (c1, c2) with
  | (AbstrContextBot, _) | (_, AbstrContextBot) -> AbstrContextBot
  | (AbstrContext(_), AbstrContext(_)) ->
    match op with
    | And -> contextMeet c1 c2
    | Or -> contextJoin c1 c2


(* Generic calculus *)

(** Forward abstract semantics of arithmetic expressions
    @param a An arithmetic expression
    @param r The context in which the arithmetic expression is performed
    @raise UNDEFINEDVAR If a variable is not defined in the provided context
    @raise PTRINTMATCH If an operation is performed between a pointer and an int
    @return The abstract domain of the evaluation of the arithmetic expression *)
let rec faexp a r : abstrD =
  match r with
  | AbstrContextBot -> AbstrBot
  | AbstrContext(map) ->
    match a with
    | Cons(n) -> alpha (RealD([n]))
    | Interval(n1, n2) -> alpha (RealD([n1; n2]))
    | Var(vn) when Context.mem vn map -> Context.find vn map
    | Var(_) -> raiseException UNDEFINEDVAR !lastStat; AbstrBot
    | ArithArithUnaExpr(op, a1) -> faexpUna op (faexp a1 r)
    | ArithArithBinExpr(a1, op, a2) -> faexpBin (faexp a1 r) op (faexp a2 r)
    | Deref(a1) ->
      begin match faexp a1 r with
        | AbstrPtr(l) ->
          begin
            let faexp_deref (ptr : ptr_var) : abstrD =
              match ptr with
              | LastPtrVar(vn') when Context.mem vn' map -> Context.find vn' map
              | LastPtrVar(_) -> raiseException UNDEFINEDVAR !lastStat; AbstrBot
              | PtrVar(other) -> AbstrPtr([other])
            in
            List.fold_left meet abstrTop (List.map faexp_deref l)
          end
        | _ -> raiseException PTRINTMATCH !lastStat; AbstrBot
      end
    | Addr(vn) -> AbstrPtr([LastPtrVar(vn)])

(** Backward abstract semantics of arithmetic expressions
    @param a An arithmetic expression
    @param r The context in which the arithmetic expression is performed
    @param p The domain in which the evaluation of the arithmetic expression has to be
    @raise UNDEFINEDVAR If a variable is not defined in the provided context
    @return The new context in which the condition that the evaluation of a is in p is satisfied or is an over-approximation of it. *)
let rec baexp a r p : abstrContext =
  match a with
  | Cons(n) when isInAbstractDomain n p -> r
  | Cons(_) -> AbstrContextBot
  | Interval(n1, n2) when isInAbstractDomain n1 p && isInAbstractDomain n2 p -> r
  | Interval(_, _) -> AbstrContextBot
  | Var(vn) ->
    begin match r with
      | AbstrContext(map) when Context.mem vn map -> AbstrContext(Context.add vn (meet p (Context.find vn map)) map)
      | AbstrContext(map) -> raiseException UNDEFINEDVAR !lastStat; AbstrContext(Context.add vn p map)
      | AbstrContextBot -> AbstrContextBot
    end
  | ArithArithUnaExpr(op, a1) -> baexp a1 r (baexpUna op (faexp a1 r) p)
  | ArithArithBinExpr(a1, op, a2) ->
    let (a1', a2') = (faexp a1 r, faexp a2 r) in
    let (p1, p2) = baexpBin a1' op a2' p in
    (*print_endline (arith_expr2string a ^ " : I had a1 = " ^ abstrD2string a1' ^ " and a2 = " ^ abstrD2string a2' ^ ", I wanted p = " ^ abstrD2string p ^ ", I got p1 = " ^ abstrD2string p1 ^ " and p2 = " ^ abstrD2string p2);*)
    contextMeet (baexp a1 r p1) (baexp a2 r p2)
  | Deref(_) ->
    if meet (faexp a r) p <> AbstrBot
    then r
    else AbstrContextBot
  | Addr(vn) ->
    begin match r with
      | AbstrContextBot -> AbstrContextBot
      | AbstrContext(map) when Context.mem vn map ->
        if meet (AbstrPtr([LastPtrVar(vn)])) p <> AbstrBot
        then r
        else AbstrContextBot
      | AbstrContext(_) -> raiseException UNDEFINEDVAR !lastStat; r
    end


(** Generic abstract interpretation of boolean expression
    @param b The boolean expression
    @param r The context
    @return The context in which b can be true or an over-approximation of it *)
let rec abexp b r : abstrContext =
  match r with
  | AbstrContextBot -> AbstrContextBot
  | AbstrContext(_) ->
    match b with
    | True -> r
    | False -> AbstrContextBot
    | BoolArithBinExpr(a1, op, a2) ->
      (*print_endline ("\na1 op a2 = " ^ bool_expr2string b ^ ", faexp(a1)=" ^ abstrD2string (faexp a1 r) ^ ", faexp(a2)=" ^ abstrD2string (faexp a2 r));*)
      let (p1, p2) = abexpArithBin (faexp a1 r) op (faexp a2 r) in
      let c1 = baexp a1 r p1 in
      let c2 = baexp a2 r p2 in
      (*print_string "c1 = ";
        print_context c1;
        print_string "c2 = ";
        print_context c2;*)
      contextMeet c1 c2
    | BoolBinExpr(b1, op, b2) -> abexpBoolBin (abexp b1 r) op (abexp b2 r)
    | BoolUnaExpr(op, b1) ->
      match op with Not -> abexp (notBoolExpr b1) r

(** Add the abstract interpretation a list of variables by the list of variables of a C function's definition
    @param args_def The definition of the arguments of a C function
    @param args The list of the arithmetic expressions provided
    @param contextToEnhance The context to update
    @param context The context in which the arithmetic expression's will be evaluated
    @return The new context *)
let rec matchCArgs args_def args contextToEnhance context : abstrD Context.t =
  match (args_def, args) with
  | ([], []) -> contextToEnhance
  | ((_, var)::t1, a::t2) -> matchCArgs t1 t2 (Context.add var (faexp a context) contextToEnhance) context
  | _ -> failwith ("matchArgs: Wrong arguments given in statement " ^ match !lastStat with StatA(_,s,_) -> stat_a2string s "" | StatC(_,s,_) -> stat_c2string s "")

(** Adds the arguments from a memory stack to a context by a C function's definition
    @param context The context
    @param args_def_reversed The definition of the arguments that the C function is waiting for
    @param stack The memory stack
    @raise Failure If the memory stack is too tiny compared to the argument's list
    @return The pair made of the new context and of the new memory stack (without the arguments used) *)
let rec matchAsmArgs context args_def stack : abstrD Context.t * memoryStack =
  match (args_def, stack) with
  | ([], _) -> (context, stack)
  | ((_, vn)::t, h::stack') -> matchAsmArgs (Context.add vn h context) t stack'
  | (_, []) -> failwith "matchAsmArgs: Number of arguments of the C function doesn't match the size of the memory stack!"

(** Add registers to the context 
    @param pre_loc The location before the ASM block
    @param entry_loc The location at the beginning of the ASM block
    @param j The environment
    @return The new environment with the registers set at location entry_loc *)
let prelude_asm pre_loc entryLoc j : abstrLocContext =
  replaceContext entryLoc (getContext pre_loc j |> fst) j
  |> addVarToContext entryLoc eax_reg abstrTop
  |> addVarToContext entryLoc ebx_reg abstrTop
  |> addVarToContext entryLoc ecx_reg abstrTop
  |> addVarToContext entryLoc edx_reg abstrTop

(** Reset the value of all registers except EAX which is replaced by a specified value
    @param context The context
    @param eax_value The new value of EAX
    @return The new context *)
let reset_registers context eax_value : abstrContext =
  match context with
  | AbstrContext(map) ->
    let map = Context.add eax_reg eax_value map in
    let map = Context.add ebx_reg abstrTop map in
    let map = Context.add ecx_reg abstrTop map in
    let map = Context.add edx_reg abstrTop map in
    AbstrContext(map)
  | _ -> failwith "rest_registers: Can't reset an empty context!"

(** Replace the registers of a context by the registers of an other one
    @param c1 The context in which the registers will be changed
    @param c2 The context from which the registers will be get
    @return The new context made of c1 with registers of c2 *)
let setRegisters c1 c2 : abstrContext =
  match (c1, c2) with
  | (AbstrContext(map1), AbstrContext(map2)) ->
    let map1 = if Context.mem eax_reg map2 then Context.add eax_reg (Context.find eax_reg map2) map1 else map1 in
    let map1 = if Context.mem ebx_reg map2 then Context.add ebx_reg (Context.find ebx_reg map2) map1 else map1 in
    let map1 = if Context.mem ecx_reg map2 then Context.add ecx_reg (Context.find ecx_reg map2) map1 else map1 in
    let map1 = if Context.mem edx_reg map2 then Context.add edx_reg (Context.find edx_reg map2) map1 else map1 in
    AbstrContext(map1)
  | (_, _) -> c1

(** Generic forward non-relational abstract interpretation of ASM statements
    @param stat The statement which is currently read
    @param f A reference to the set of the C functions
    @param callStack The call stack
    @param stack The memory stack
    @param j The environment
    @return A pair made of the new environment after the read of stat and of the new memory stack *)
let rec aPostAsm (l1, stat, l2) f callStack stack j : abstrLocContext * memoryStack =
  lastStat := StatA(l1, stat, l2);
  match j with
  | AbstrEnvironmentBot -> (AbstrEnvironmentBot, stack)
  | GoForward(goal, context, environment) when l1 = goal -> aPostAsm (l1, stat, l2) f callStack stack (replaceContext goal context environment)
  | GoForward(_,_,_) -> (j, stack)
  | AbstrEnvironment(_) ->
    match (l1, stat, l2) with
    (* ### MOV ### *)
    | (Loc_a(_), Mov(ae1, ae2), Loc_a(_)) ->
      begin match ae1 with
        | AsmReg(r) ->
          let result = getAsmValue l1 ae2 j in
          let j = replaceContext l2 (getContext l1 j |> fst) j in
          (updateVarInContext l2 (getRegNameVar r) result j, stack)
        | AsmVar(vn) ->
          let result = getAsmValue l1 ae2 j in
          let j = replaceContext l2 (getContext l1 j |> fst) j in
          (updateVarInContext l2 vn result j, stack)
        | _ -> failwith "aPostAsm: I can't MOV something to an absolute address!"
      end
    (* ### JMP ### *)
    | (Loc_a(_), Jmp(ae), Loc_a(_)) ->
      let goal =
        match ae with
        | AsmLblLoc(goal) -> goal
        | AsmLbl(_) -> failwith "aPostAsm: An ASM statement contains labels what is prohibited during the execution!"
        | AsmVar(vn) ->
          begin match getVar l1 vn j with
            | AbstrLoc(goal) -> goal
            | _ -> failwith "aPostAsm: I found a variable which is not an AbstrLoc one in a Jmp statement!"
          end
        | AsmReg(reg) ->
          begin match getVar l1 (getRegNameVar reg) j with
            | AbstrLoc(goal) -> goal
            | _ -> failwith "aPostAsm: I found a register which is not an AbstrLoc one in a Jmp statement!"
          end
        | _ -> failwith ("aPostAsm: I tried to jump to a weird location : " ^ asm_expr2string ae)
      in
      let lastFun = getLastFunction l1 f in
      if isInBranch l1 lastFun j
      then raise GoBackward
      else
        begin
          let realLastFun = getLastFunction goal f in
          if lastFun = realLastFun
          then
            begin match lastFun with
              | Function(_,_,_, block) ->
                let _ = aPostCBlock block f callStack stack (GoForward(goal, getContext l1 j |> fst, j)) in
                failwith "aPostASM: No ret/return found after the JMP in the same block but before this statement!"
              | FunctionWithoutLoc(_,_,_,_) -> failwith "aPostASM: I found a FunctionWithoutLoc in a located program!"
            end
          else
            begin
              begin match realLastFun with
                | Function(_,_,_, block) ->
                  let _ = aPostCBlock block f callStack stack (GoForward(goal, removeCVariables (getContext l1 j |> fst), j)) in
                  failwith "aPostASM: No ret/return found after the JMP in an other block!"
                | FunctionWithoutLoc(_,_,_,_) -> failwith "aPostASM: I found a FunctionWithoutLoc in a located program!"
              end
            end
        end
    (* ### CALL ### *)
    | (Loc_a(_), Call(ae), Loc_a(_)) ->
      let entryPoint =
        match ae with
        | AsmReg(reg) ->
          begin match getVar l1 (getRegNameVar reg) j with
            | AbstrLoc(goal) -> goal
            | _ -> failwith "aPostASM: This registry doesn't contain a location!"
          end
        | AsmVar(vn) ->
          begin match getVar l1 vn j with
            | AbstrLoc(goal) -> goal
            | _ -> failwith "aPostASM: This variable doesn't contain a location!"
          end
        | AsmLblLoc(goal) -> goal
        | _ -> failwith "aPostASM: This asm expression doesn't contain a location!"
      in
      begin match getLastFunction entryPoint f with
        | FunctionWithoutLoc(_,_,_,_) -> failwith "aPostASM: I found a FunctionWithoutLoc in a located program!"
        | Function(_,_, args, (l_block, block)) ->
          match entryPoint with
          | Loc_c(_) -> (* ASM->C *)
            initFunction true callStack;
            let context = getContext (Loc_c(getLocationNumber l_block - 1)) j |> fst in
            begin match context with
              | AbstrContext(map) ->
                let (map', stack') = matchAsmArgs map args stack in
                let context' = AbstrContext(map') in
                let newEnv = AbstrEnvironment(Environment.empty) in
                let newEnv = replaceContext entryPoint (AbstrContext(map)) newEnv in
                let newEnv = replaceContext l_block context' newEnv in
                begin
                  try
                    let _ = aPostCBlock (l_block, block) f callStack stack' newEnv in
                    failwith "aPostASM: Call ASM->C failed because of the lack of a return"
                  with ReturnRaised(result,_, _) ->
                    let _ = Stack.pop !callStack in
                    (replaceContext l2 (reset_registers (getContext l1 j |> fst) result) j, stack')
                end
              | _ -> failwith "aPostASM: The context cannot be empty at the beginning of a function!"
            end
          | Loc_a(_) -> (* ASM->ASM *)
            initFunction false callStack;
            let lastContext = getContext l1 j |> fst in
            let lastContext = setRegisters lastContext (getContext l1 j |> fst) in
            begin
              try
                let _ = aPostCBlock (l_block, block) f callStack ((AbstrLoc(l2))::stack) (GoForward(entryPoint, lastContext, j)) in
                failwith "aPostASM: Call ASM->ASM failed because of the lack of a return!"
              with ReturnRaised(_, contextReturned, memoryStackReturned) ->
                let _ = Stack.pop !callStack in
                match memoryStackReturned with
                | (AbstrLoc(goal))::newStack when goal = l2 -> (replaceContext l2 contextReturned j, newStack)
                | (AbstrLoc(goal))::newStack ->
                  begin
                    try
                      let _ = aPostAsm (l1, Jmp(AsmLblLoc(goal)), l2) f callStack newStack j in
                      failwith "aPostASM: I should have raise a ReturnRaised exception !" (* Check abstrTop which could be returned *)
                    with ReturnRaised(codeReturned, contextReturned, memoryStackReturned) ->
                      raise (ReturnRaised(codeReturned, contextReturned, memoryStackReturned))
                  end
                | _ -> raiseException WRONGSTACKOFFSET !lastStat; (replaceContext l2 contextReturned j, memoryStackReturned)
            end
      end
    (* ### RET ### *)
    | (Loc_a(_), Ret, Loc_a(_)) ->
      raiseReturn false false (getVar l1 eax_reg j) (getContext l1 j |> fst) callStack stack;
      failwith "aPostAsm: Not supposed to happen, should have raised a returnRaised exception!"
    | (Loc_a(_), Push(ae), Loc_a(_)) ->
      (replaceContext l2 (getContext l1 j |> fst) j, (getAsmValue l1 ae j)::stack)
    (* ### POP ### *)
    | (Loc_a(_), Pop(ae), Loc_a(_)) ->
      let (result, newStack) =
        match stack with
        | [] -> raiseException WRONGSTACKOFFSET !lastStat; (abstrTop, [])
        | h::t -> (h, t)
      in
      let j = replaceContext l2 (getContext l1 j |> fst) j in
      let newEnvironment =
        match ae with
        | AsmReg(reg) -> updateVarInContext l2 (getRegNameVar reg) result j
        | AsmVar(vn) -> updateVarInContext l2 vn result j
        | _ -> failwith ("aPostAsm: I can't set the value of the ASM expression " ^ asm_expr2string ae)
      in
      (newEnvironment, newStack)
    (* ### OTHERS ### *)
    | (Loc_a(_), Label(_), Loc_a(_)) -> failwith "aPostAsm: An ASM statement contains labels what is prohibited during the execution!"
    | (Loc_c(_), s, _) | (_, s, Loc_c(_)) -> failwith ("aPostAsm: The statement " ^ stat_a2string s "" ^ " has invalid locations!")

(** Generic forward non-relational abstract interpretation of ASM blocks
    @param pre_loc The entry location of [block]
    @param block The block of ASM statement to read
    @param f A reference to the set of C functions
    @param callStack The call stack
    @param stack The memory stack
    @param j The environment
    @return A pair made of the new environment and of the new memory stack *)
and aPostAsmBlock (pre_loc, block) f callStack stack j : abstrLocContext * memoryStack =
  match block with
  | [] ->
    begin match j with
      | GoForward(goal, context, environment) when pre_loc = goal ->  (replaceContext goal context environment, stack)
      | _ -> (j, stack)
    end
  | (stat, post_loc)::t ->
    let (j1, stack1) = aPostAsm (pre_loc, stat, post_loc) f callStack stack j in
    aPostAsmBlock (post_loc, t) f callStack stack1 j1

(** Generic forward non-relational abstract interpretation of C blocks
    @param block The block of C statements
    @param f A reference to the C functions
    @param callStack The call stack
    @param lastAsmStack The stack state of the last ASM statement
    @param j The environment
    @return The pair made of the new environment after the execution of the block of C statements and the new memory stack *)
and aPostCBlock (pre_loc, program : loc_stat_c) f callStack lastAsmStack j : abstrLocContext * memoryStack =
  match j with
  | AbstrEnvironmentBot -> (AbstrEnvironmentBot, lastAsmStack)
  | _ ->
    match program with
    | [] ->
      (* We can have a GoForward which points to a C location in C branch! *)
      begin match j with
        | GoForward(goal, context, environment) when pre_loc = goal -> (replaceContext goal context environment, lastAsmStack)
        | _ -> (j, lastAsmStack)
      end
    | (stat, post_loc)::t ->
      let (j', newAsmStack) = aPostC (pre_loc, stat, post_loc) f callStack lastAsmStack j in
      aPostCBlock (post_loc, t) f callStack newAsmStack j'

(** Generic forward non-relational abstract interpretation of C statements
    @param stat The C statement
    @param f A reference to the C functions
    @param callStack The call stack
    @param lastAsmStack The stack state of the last ASM statement
    @param j The environment
    @return The pair made of the new environment after the excetion of the C statement and of the new memory stack *)
and aPostC (l1, stat, l2 : loc * stat_c * loc) f callStack lastAsmStack j : abstrLocContext * memoryStack =
  lastStat := StatC(l1, stat, l2);
  match j with
  | AbstrEnvironmentBot -> (AbstrEnvironmentBot, lastAsmStack)
  | GoForward(goal, context, environment) ->
    if goal = l1
    then aPostC (l1, stat, l2) f callStack lastAsmStack (replaceContext goal context environment)
    else
      let goalNumber = getLocationNumber goal in
      begin match stat with
        | Branch(_, (l1', block1), (l2', block2)) ->
          if getLocationNumber l2' > goalNumber
          then 
            begin
              let (j', newStack) = aPostCBlock (l1', block1) f callStack lastAsmStack j in
              let lastLoc = lastLocOfProgram (l1', block1) in
              (j'
               |> replaceContext l2 (getContext lastLoc j' |> fst)
               |> removeStatementContextBlock block1 l1'
              , newStack)
            end
          else if getLocationNumber l2 > goalNumber
          then
            begin
              let (j', newStack) = aPostCBlock (l2', block2) f callStack lastAsmStack j in
              let lastLoc = lastLocOfProgram (l2', block2) in
              (j'
               |> replaceContext l2 (getContext lastLoc j' |> fst)
               |> removeStatementContextBlock block2 l2'
              , newStack)
            end
          else (j, lastAsmStack)
        | While(b, (l1', block1)) ->
          if getLocationNumber l2 > goalNumber
          then
            begin
              let afterS = lastLocOfProgram (l1', block1) in
              let aPostB (j, lastStack) =
                match abexp b (getContext l1 j |> fst) with 
                | AbstrContext(map) -> (replaceContext l1' (AbstrContext(map)) j, lastStack)
                | AbstrContextBot -> (AbstrEnvironmentBot, lastStack)
              in
              let aPostNotB (j, lastStack) = (replaceContext l2 (abexp (notBoolExpr b) (getContext l1 j |> fst)) j, lastStack) in
              let aPostR (j, lastStack) = (replaceContext l1 (getContext afterS j |> fst) j, lastStack) in (* Updates the beginning of the loop to try an other iteration *)
              let aPostS (j, lastStack) = aPostCBlock (l1', block1) f callStack lastStack j in (* Performs an iteration of the loop *)
              begin
                let (finalJ, finalStack) = (j, lastAsmStack) |> aPostS |> aPostR |> lfp (fun x -> x |> aPostB |> aPostS |> aPostR) |> aPostNotB in
                (finalJ |> removeStatementContextBlock block1 l1', finalStack)
              end
            end
          else (j, lastAsmStack)
        | Asm((l1', block1)) ->
          if getLocationNumber l2 > goalNumber
          then
            begin
              let (j', memoryStack') = aPostAsmBlock (l1', block1) f callStack lastAsmStack j in
              let lastLoc = lastLocOfProgram (l1', block1) in
              (replaceContext l2 (postlude_asm (getContext lastLoc j' |> fst) memoryStack') j', memoryStack')
            end
          else (j, lastAsmStack)
        | _ -> (j, lastAsmStack)
      end
  | AbstrEnvironment(_) ->
    let (jl, j) = getContext l1 j in
    match jl with
    | AbstrContextBot -> (replaceContext l2 AbstrContextBot j, lastAsmStack)
    | AbstrContext(_) ->
      match (l1, stat, l2) with
      (* ### DECLARE ### *)
      | (Loc_c(_), Declare(_, vn), Loc_c(_)) ->
        let j = replaceContext l2 jl j in
        (addVarToContext l2 vn abstrTop j, lastAsmStack)
      (* ### ASSIGN ### *)
      | (Loc_c(_), Assign(vn, a), Loc_c(_)) ->
        let j = replaceContext l2 jl j in
        (updateVarInContext l2 vn (faexp a jl) j, lastAsmStack)
      (* ### DECLARE_ASSIGN *)
      | (Loc_c(_), DeclareAssign(_, vn, a), Loc_c(_)) ->
        let j = replaceContext l2 jl j in
        (addVarToContext l2 vn (faexp a jl) j, lastAsmStack)
      (* ### BRANCH ### *)
      | (Loc_c(_), Branch(b, (Loc_c(lt), statS), (Loc_c(lf), statF)), Loc_c(_)) ->
        begin
          let lastLS = lastLocOfProgram (Loc_c(lt), statS) in
          let lastLF = lastLocOfProgram (Loc_c(lf), statF) in
          let j' = replaceContext (Loc_c(lt)) (abexp b jl) j in
          let j' = replaceContext (Loc_c(lf)) (abexp (notBoolExpr b) jl) j' in
          try
            let ((j'', stack1), result1, context1, statFReturn) =
              try (aPostCBlock (Loc_c(lf), statF) f callStack lastAsmStack j', AbstrBot, AbstrContextBot, false)
              with ReturnRaised(result, context, stack) -> ((j', stack), result, context, true)
            in
            let ((j'', stack2), result2, context2, statSReturn) =
              try (aPostCBlock (Loc_c(lt), statS) f callStack lastAsmStack j'', AbstrBot, AbstrContextBot, false)
              with ReturnRaised(result, context, stack) -> ((j'', stack), result, context, true)
            in
            let (j'', statSReturn) =
              if getContext (Loc_c(lt)) j'' |> fst = AbstrContextBot
              then (replaceContext lastLS AbstrContextBot j'', true)
              else (j'', statSReturn)
            in
            let (j'', statFReturn) =
              if getContext (Loc_c(lf)) j'' |> fst = AbstrContextBot
              then (replaceContext lastLF AbstrContextBot j'', true)
              else (j'', statFReturn)
            in
            if not statFReturn && not statSReturn
            then begin
              let j''lt = getContext lastLS j'' |> fst in
              let j''lf = getContext lastLF j'' |> fst in
              (replaceContext l2 (contextJoin j''lt j''lf) j''
               |> removeStatementContextBlock statS (Loc_c(lt)) |> removeStatementContextBlock statF (Loc_c(lf)),
               joinMemoryStack stack1 stack2)
            end
            else begin
              if statFReturn && statSReturn
              then begin
                raiseReturn true true result1 context1 callStack stack1;
                raiseReturn true false result2 context2 callStack stack2;
                failwith "aPostC: Not supposed to happen, should have raised ReturnRaised!"
              end else if statFReturn
              then begin
                raiseReturn true true result1 context1 callStack stack1;
                (replaceContext l2 (getContext lastLS j'' |> fst) j'' |> removeStatementContextBlock statS (Loc_c(lt)), stack2)
              end else
                begin
                  raiseReturn true true result2 context2 callStack stack2;
                  (replaceContext l2 (getContext lastLF j'' |> fst) j'' |> removeStatementContextBlock statF (Loc_c(lf)), stack1)
                end
            end
          with GoBackward ->
            begin
              match getLastFunction l1 f with
              | Function(_,_,_, block) ->
                begin
                  let lastLoc = lastLocOfProgram block in
                  (* I reset the memory at this statement to prevent other GoBackward on this branch! *)
                  let j' = replaceContext l1 AbstrContextBot j' in
                  let (returnCode1, context1, stack1) =
                    try
                      let (finalEnv, newStack) = aPostCBlock block f callStack lastAsmStack (GoForward((Loc_c(lt)), getContext (Loc_c(lt)) j' |> fst, j'))
                      in (abstrTop, getContext lastLoc finalEnv |> fst, newStack)
                    with ReturnRaised(returnCode, context, _) -> (returnCode, context, lastAsmStack)
                  in
                  let (returnCode2, context2, stack2) =
                    try
                      let (finalEnv, newStack) = aPostCBlock block f callStack lastAsmStack (GoForward((Loc_c(lf)), getContext (Loc_c(lf)) j' |> fst, j'))
                      in (abstrTop, getContext lastLoc finalEnv |> fst, newStack)
                    with ReturnRaised(returnCode, context, _) -> (returnCode, context, lastAsmStack)
                  in
                  raise (ReturnRaised(join returnCode1 returnCode2, contextJoin context1 context2, joinMemoryStack stack1 stack2))
                end
              | FunctionWithoutLoc(_,_,_,_) -> failwith "aPostC: I have a FunctionWithoutLoc in a located program!"
            end
        end
      (* ### ASM ### *)
      | (Loc_c(_), Asm(_,[]), Loc_c(_)) -> (replaceContext l2 (getContext l1 j |> fst) j, lastAsmStack)
      | (Loc_c(_), Asm(l_block, block), Loc_c(_)) ->
        let lastLoc = lastLocOfProgram (l_block, block) in
        let (j', stack) = j |> prelude_asm l1 l_block |> aPostAsmBlock (l_block, block) f callStack [] in
        (replaceContext l2 (postlude_asm (getContext lastLoc j' |> fst) stack) j', stack)
      (* ### PTRASSIGN ### *)
      | (Loc_c(_), PtrAssign(ptr, a), Loc_c(_)) ->
        begin
          let v = faexp a jl in
          let newContext =
            match jl with
            | AbstrContext(map) ->
              let rec derefInfos (ptr : ptr_var) : int * name_var =
                match ptr with
                | PtrVar(ptr') -> let (nbrDeref, lastNameVar) = derefInfos ptr' in (nbrDeref+1, lastNameVar)
                | LastPtrVar(vn) -> (1, vn)
              in
              let (nbrDeref, lastNameVar) = derefInfos ptr in
              let rec unfoldPtrVar (remainingNbrDeref : int) (ptr : ptr_var) : name_var list =
                match ptr with
                | LastPtrVar(vn) when remainingNbrDeref = 1 -> [vn]
                | PtrVar(ptr') when remainingNbrDeref > 1 -> unfoldPtrVar (remainingNbrDeref-1) ptr'
                | LastPtrVar(vn) when remainingNbrDeref > 1 && Context.mem vn map -> unfoldPtrAbstrD (remainingNbrDeref-1) (Context.find vn map)
                | _ -> raiseException PTRINTMATCH !lastStat; []
              and unfoldPtrAbstrD (remainingNbrDeref : int) (a : abstrD) : name_var list =
                match a with
                | AbstrPtr(h::ptrList) when remainingNbrDeref > 0 -> (unfoldPtrVar remainingNbrDeref h) @ (unfoldPtrAbstrD remainingNbrDeref (AbstrPtr(ptrList)))
                | AbstrPtr([]) when remainingNbrDeref > 0 -> []
                | _ -> raiseException PTRINTMATCH !lastStat; []
              in
              let vn_list =
                if Context.mem lastNameVar map
                then unfoldPtrAbstrD nbrDeref (Context.find lastNameVar map)
                else (raiseException PTRINTMATCH !lastStat; [])
              in
              let rec updateVars (l : name_var list) : abstrContext =
                match l with
                | h::t when Context.mem h map -> contextJoin (AbstrContext(Context.add h v map)) (updateVars t)
                | [] -> jl
                | _::t -> raiseException PTRINTMATCH !lastStat; updateVars t
              in
              updateVars vn_list
            | _ -> raiseException UNDEFINEDVAR !lastStat; jl
          in (replaceContext l2 newContext j, lastAsmStack)
        end
      (* ### CALLASSIGNC ### *)
      | (Loc_c(_), CallAssignC(var, fun_name, args), Loc_c(_)) ->
        begin
          let (newContext, newStack) = match jl with
            | AbstrContext(map) when Context.mem fun_name map && Context.mem var map ->
              begin match Context.find fun_name map with
                | AbstrLoc(entryPoint) when Environment.mem entryPoint !f ->
                  begin match Environment.find entryPoint !f with
                    | Function(_, fun_name', args_def, (l, block)) when fun_name = fun_name' && List.length args_def = List.length args ->
                      let lastFunContext = getContext entryPoint j |> fst in
                      let funContext = AbstrContext(matchCArgs args_def args (match lastFunContext with AbstrContext(map) -> map | _ -> Context.empty) jl) in
                      let funEnv = replaceContext l funContext (AbstrEnvironment(Environment.empty)) in
                      let funEnv = replaceContext entryPoint lastFunContext funEnv in
                      initFunction true callStack;
                      let (result, newStack) =
                        try let (_, newStack) = aPostCBlock (l, block) f callStack lastAsmStack funEnv in (abstrTop, newStack)
                        with ReturnRaised(result, _, newStack) -> (result, newStack)
                      in
                      let _ = Stack.pop !callStack in
                      (AbstrContext(Context.add var result map), newStack)
                    | _ -> raiseException UNDEFINEDVAR !lastStat; (jl, lastAsmStack)
                  end
                | _ -> raiseException UNDEFINEDVAR !lastStat; (jl, lastAsmStack)
              end
            | _ -> raiseException UNDEFINEDVAR !lastStat; (jl, lastAsmStack)
          in
          (replaceContext l2 newContext j, newStack)
        end
      (* ### CALLASSIGNASM ### *)
      | (Loc_c(_), CallAssignAsm(var, fun_loc, args), Loc_c(_)) ->
        begin
          let (newContext, newStack) = match jl with
            | AbstrContext(map) when Context.mem var map ->
              begin match getLastFunction fun_loc f with
                | Function(_,_,_,(l, block)) ->
                  let entryPoint = Loc_c(getLocationNumber l - 1) in
                  let j' = prelude_asm entryPoint fun_loc j in
                  let lastFunContext = getContext fun_loc j' |> fst in
                  let rec calcArgs (args : arith_expr list) (result : abstrD list) : abstrD list =
                    match args with
                    | [] -> result
                    | h::t -> calcArgs t ((faexp h jl)::result)
                  in
                  let funEnv = GoForward(fun_loc, lastFunContext, j) in
                  initFunction false callStack;
                  let (result, newStack) =
                    try let (_, newStack) = aPostCBlock (l, block) f callStack ((AbstrLoc(l2))::(calcArgs args lastAsmStack)) funEnv in (abstrTop, newStack)
                    with
                    | ReturnRaised(result,_, (AbstrLoc(goal))::newStack) when goal = l2 && newStack = lastAsmStack -> (result, newStack)
                    | ReturnRaised(result,_, newStack) -> raiseException WRONGSTACKOFFSET !lastStat; (result, newStack)
                  in
                  let _ = Stack.pop !callStack in
                  (AbstrContext(Context.add var result map), newStack)
                | _ -> failwith "aPostC: The ASM function is not in a C function!"
              end
            | _ -> raiseException UNDEFINEDVAR !lastStat; (jl, lastAsmStack)
          in
          (replaceContext l2 newContext j, newStack)
        end
      (* ### RETURN ### *)
      | (Loc_c(_), Return(a), Loc_c(_)) ->
        print_endline "hello";
        raiseReturn true false (faexp a jl) jl callStack lastAsmStack;
        failwith "aPostC: Not supposed to happen, should have raised a returnRaised exception!"
      (* ### WHILE ### *)
      | (Loc_c(_), While(b, (Loc_c(lt), statS)), Loc_c(_)) ->
        let afterS = lastLocOfProgram (Loc_c(lt), statS) in
        let aPostB (j, lastStack) =
          match abexp b (getContext l1 j |> fst) with 
          | AbstrContext(map) -> (replaceContext (Loc_c(lt)) (AbstrContext(map)) j, lastStack)
          | AbstrContextBot -> (AbstrEnvironmentBot, lastStack)
        in
        let aPostNotB (j, lastStack) = (replaceContext l2 (abexp (notBoolExpr b) (getContext l1 j |> fst)) j, lastStack) in
        let aPostR (j, lastStack) = (replaceContext l1 (getContext afterS j |> fst) j, lastStack) in (* Updates the beginning of the loop to try an other iteration *)
        let aPostS (j, lastStack) = aPostCBlock (Loc_c(lt), statS) f callStack lastStack j in (* Performs an iteration of the loop *)
        begin
          let (finalJ, finalStack) = (j, lastAsmStack) |> lfp (fun x -> x |> aPostB |> aPostS |> aPostR) |> aPostNotB in
          (finalJ |> removeStatementContextBlock statS (Loc_c(lt)), finalStack)
        end
      (* ### OTHERS ### *)
      | (_, BranchWithoutLoc(_,_,_), _) -> failwith "aPostC: BranchWithoutLoc found in the program!"
      | (_, WhileWithoutLoc(_, _), _) -> failwith "aPostC: WhileWithoutLoc found in the programe"
      | (_, AsmWithoutLoc(_), _) -> failwith "aPostC: AsmWithoutLoc found in the program!"
      | (_, Branch(_,_,_), _) | (_, While(_, _), _) -> failwith ("aPostC: The statement at location " ^ loc2string l1 ^ " has invalid locations!")
      | (Loc_a(_), s, _) | (_, s, Loc_a(_)) -> failwith ("aPostC: The statement " ^ stat_c2string s "" ^ " has invalid locations!")

(** Generic forward non-relational abstract interpretation of global statement
    @param pre_loc The location before the global statement
    @param stat The global statement
    @param f A reference to the set of C functions
    @param j The environement
    @return The new environment after the execution of the global statement *)
let aPostGlobalC pre_loc stat post_loc f j : abstrLocContext =
  match (pre_loc, stat, post_loc) with
  | (Loc_c(_), Function(_, vn, _, _), Loc_c(_)) ->
    let j = addVarToContext pre_loc vn (AbstrLoc(pre_loc)) j in
    f := Environment.add pre_loc stat !f;
    replaceContext post_loc (getContext pre_loc j |> fst) j
  | (Loc_c(_), FunctionWithoutLoc(_,_,_,_), Loc_c(_)) -> failwith "aPostGlobalC: FunctionWithoutLoc found in the program!"
  | (Loc_a(_), _, _) | (_, _, Loc_a(_)) -> failwith ("aPostGlobalC: The statement at location " ^ loc2string pre_loc ^ " has invalid locations!")

(** Generic foward non-relational abstract interpretation of a block of global statements
    @param pre_loc The location before the first global statement of the block
    @param program The block of global statements
    @param f A reference to the set of C functions
    @param j The environment
    @return The new environment after the execution of the block of global statements *)
let rec aPostProgram pre_loc program f j : abstrLocContext =
  match program with
  | [] -> j
  | (stat, post_loc)::t -> aPostGlobalC pre_loc stat post_loc f j |> aPostProgram post_loc t f