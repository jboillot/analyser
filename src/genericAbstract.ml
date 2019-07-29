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
open SpecialVariables

let var_id = ref 0

(* Generic methods *)

let areEnvironmentEqual e1 e2 : bool =
  Environment.compare compare !e1 !e2 = 0

let wideningOnEnvironments e1 e2 : abstrEnvironment ref =
  (* To be quicker in some cases. If they are equal I don't have to give another reference. *)
  if !e1 = !e2
  then e1
  else
    let environmentWideningMerge (_ : int) (a1 : abstrD option) (a2 : abstrD option) : abstrD option =
      match (a1, a2) with
      | (None, _) | (_, None) -> None
      | (Some(a1'), Some(a2')) -> Some(widening a1' a2')
    in ref (Environment.merge environmentWideningMerge !e1 !e2)

let environmentJoin e1 e2 : abstrEnvironment ref =
  (* To be quicker in some cases. If they are equal I don't have to give another reference. *)
  if !e1 = !e2
  then e1
  else
    let environmentJoinMerge (_ : int) (a1 : abstrD option) (a2 : abstrD option) : abstrD option =
      match (a1, a2) with
      | (None, _) | (_, None) -> None (* If a variable exists in a branch and not in another, it won't exist in the final environment *)
      | (Some(a1'), Some(a2')) -> Some(join a1' a2')
    in ref (Environment.merge environmentJoinMerge !e1 !e2)

let contextMerge c1 c2 : abstrContext =
  let contextMergeAux (_ : name_var) (id1 : int option) (id2 : int option) : int option =
    match (id1, id2) with
    | (Some(id1'), Some(id2')) when id1' = id2' -> Some(id1')
    | _ -> None
  in
  match (c1, c2) with
  | (AbstrContext(map1), AbstrContext(map2)) -> AbstrContext(Context.merge contextMergeAux map1 map2)
  | _ -> AbstrContextBot

let forgetNewVariables ~lastContext ~newContext ~env : unit =
  let forgetNewVariablesMerge (_ : name_var) (id1 : int option) (id2 : int option) : int option =
    match (id1, id2) with
    | (Some(id1'), Some(id2')) when id1' <> id2' ->
      env := Environment.remove id2' !env;
      None
    | (None, Some(id2')) ->
      env := Environment.remove id2' !env;
      None
    | _ -> None
  in
  match (lastContext, newContext) with
  | (AbstrContext(map1), AbstrContext(map2)) -> let _ = Context.merge forgetNewVariablesMerge map1 map2 in ()
  | _ -> failwith "forgetNewVariables: I can't forget variables from an AbstrContextBot or a GoForward context!"

let environmentMeet e1 e2 : abstrEnvironment ref =
  let environmentMeetMerge (_ : int) (a1 : abstrD option) (a2 : abstrD option) : abstrD option =
    match (a1, a2) with
    | (None, _) | (_, None) -> None
    | (Some(a1'), Some(a2')) -> Some(meet a1' a2')
  in
  if !e1 = !e2
  then e1 (* If they are equal I don't have to give another reference. *)
  else ref (Environment.merge environmentMeetMerge !e1 !e2)

let getVar ~context ~vn ~env : abstrD =
  match context with
  | AbstrContext(mapContext) when Context.mem vn mapContext && Environment.mem (Context.find vn mapContext) !env ->
    Environment.find (Context.find vn mapContext) !env
  | _ -> raiseException UNDEFINEDVAR !lastStat; abstrTop

let rec lfp ~env ~e1 (context, lastStack) : memoryStack =
  let lastEnv = ref (!env) in
  let (nextContext, nextStack) = e1 (context, lastStack) in
  match nextContext with
  | AbstrContextBot -> env := !lastEnv; lastStack
  | _ when !Options.withoutWidening1 ->
    if areEnvironmentEqual lastEnv env && lastStack = nextStack
    then lastStack
    else
      let () = env := !(environmentJoin lastEnv env) in
      lfp ~env:env ~e1:e1 (context, nextStack)
  | _ when !Options.withoutWidening2 ->
    if areEnvironmentEqual lastEnv env && lastStack = nextStack
    then lastStack
    else
      let finalStack = lfp ~env:env ~e1:e1 (context, nextStack) in
      let () = env := !(environmentJoin lastEnv env) in
      finalStack
  | _ ->
    env := !(wideningOnEnvironments lastEnv env);
    if areEnvironmentEqual lastEnv env && lastStack = nextStack
    then lastStack
    else lfp ~env:env ~e1:e1 (context, nextStack)

let rec addVarToContext ~vn ~value ~env context : abstrContext =
  match context with
  | AbstrContext(map) when not (Context.mem vn map) ->
    env := Environment.add !var_id value !env;
    let result = AbstrContext(Context.add vn !var_id map) in
    incr var_id;
    result
  | AbstrContext(_) -> raiseException ALREADYDEFINEDVAR !lastStat; updateVarInContext ~vn:vn ~value:value ~context:context ~env:env; context
  | _ -> failwith "addVarToContext: I can't add a variable to a GoForward or an AbstrContextBot context!"

and updateVarInContext ~vn ~value ~context ~env : unit =
  match context with
  | AbstrContext(map) when Context.mem vn map ->
    env := Environment.add (Context.find vn map) value !env
  | AbstrContext(_) -> raiseException UNDEFINEDVAR !lastStat
  | _ -> failwith "addVarToContext: I can't update a variable in a GoForward or an AbstrContextBot context!"

let getAsmValue ~ae ~context ~env : abstrD =
  match ae with
  | AsmReg(r') -> getVar ~context:context ~vn:(getRegNameVar r') ~env:env
  | AsmVar(vn) -> getVar ~context:context ~vn:vn ~env:env
  | AsmCst(cst) -> alpha (RealD([cst]))
  | AsmLbl(_) -> failwith "getAsmValue: Not supposed to have labels in located programs!"
  | AsmLblLoc(loc) -> AbstrLoc(loc)
  | AsmPtr(ae') ->
    match ae' with
    | AsmReg(r') ->
      begin match getVar ~context:context ~vn:(getRegNameVar r') ~env:env with
        | AbstrPtr(l) -> IdSet.fold (fun id next -> join next (Environment.find id !env)) l AbstrBot
        | _ -> raiseException PTRINTMATCH !lastStat; abstrTop
      end
    | AsmVar(vn) ->
      begin match getVar ~context:context ~vn:vn ~env:env with
        | AbstrPtr(l) -> IdSet.fold (fun id next -> join next (Environment.find id !env)) l AbstrBot
        | _ -> raiseException PTRINTMATCH !lastStat; abstrTop
      end
    | _ -> failwith "getAsmValue: Invalid pointer of ASM expression!"

let getLastFunction ~l ~f : global_statement * abstrContext =
  let funLoc = ref (Loc_c(-1)) in
  let funResult = ref (FunctionWithoutLoc(Int_c, "This function is an error! Not supposed to happen", [], []), AbstrContextBot) in
  let getLastFunctionIterator (l' : loc) (lastFun, lastContext) : unit =
    if getLocationNumber l' <= getLocationNumber l && getLocationNumber l' > getLocationNumber !funLoc
    then (funLoc := l'; funResult := (lastFun, lastContext))
  in
  LocMap.iter getLastFunctionIterator !f;
  if !funLoc <> (Loc_c(-1))
  then !funResult
  else failwith ("getLastFunction: No function found before the location " ^ loc2string l ^ "!")

let rec joinMemoryStack ~s1 ~s2 : memoryStack =
  match (s1, s2) with
  | ([], []) -> []
  | (h1::t1, h2::t2) -> (join h1 h2)::(joinMemoryStack ~s1:t1 ~s2:t2)
  | _ -> raiseException WRONGSTACKOFFSET !lastStat; []

let initFunction ~cCall ~callStack : unit =
  Stack.push (cCall, AbstrBot, ref Environment.empty, AbstrContextBot, []) !callStack

let postlude_asm ~context ~env : abstrContext =
  match context with
  | AbstrContextBot | GoForward(_,_) -> context
  | AbstrContext(map) ->
    env :=
      !env
      |> Environment.remove (Context.find eax_reg map)
      |> Environment.remove (Context.find ebx_reg map)
      |> Environment.remove (Context.find ecx_reg map)
      |> Environment.remove (Context.find edx_reg map);
    AbstrContext(
      map
      |> Context.remove eax_reg
      |> Context.remove ebx_reg
      |> Context.remove ecx_reg
      |> Context.remove edx_reg
    )

let raiseReturn ~cCall ~addReturnPossibility ~returnCode ~returnContext ~returnEnv ~callStack ~stack : unit =
  match Stack.pop !callStack with
  | exception Stack.Empty -> failwith "raiseReturn: Return raised but the call stack is empty!"
  | (cCall',_,_,_,_) when cCall <> cCall' -> failwith "raiseReturn: You can't return a C function in ASM and vice versa!"
  | (_, _, _, AbstrContextBot, _) ->
    (* If the context is bottom we don't check the join for the memory stack because it is useless, we already have an error or this is the firt call of raiseReturn
           This feature allows ASM->ASM calls: the memory stack is not empty when the RET instruction is called. *)
    (* I can give directly the reference to the environment because it won't be used anymore after *)
    Stack.push (cCall, returnCode, returnEnv, returnContext, stack) !callStack;
    if not addReturnPossibility then raise (ReturnRaised(returnCode, returnEnv, returnContext, stack))
  | (_, returnCode', returnEnv', returnContext', stack') ->
    let newReturnCode = join returnCode returnCode' in
    let newEnv = environmentJoin returnEnv returnEnv' in
    let newContext = contextMerge returnContext returnContext' in
    let newStack = joinMemoryStack ~s1:stack ~s2:stack' in
    Stack.push (cCall, newReturnCode, newEnv, newContext, newStack) !callStack;
    if not addReturnPossibility then raise (ReturnRaised(newReturnCode, newEnv, newContext, newStack))

let abexpBoolBin (env1, context1IsBot) op (env2, context2IsBot) : (abstrEnvironment ref * bool) =
  match op with
  | And -> (environmentMeet env1 env2, context1IsBot && context2IsBot)
  | Or -> (environmentJoin env1 env2, context2IsBot || context2IsBot)


(* Generic calculus *)

let rec faexp ~a ~context ~env : abstrD =
  match context with
  | AbstrContextBot -> AbstrBot
  | GoForward(_,_) -> failwith "faexp: I can't handle GoForward contexts!"
  | AbstrContext(map) ->
    match a with
    | Cons(n) -> alpha (RealD([n]))
    | Interval(n1, n2) -> alpha (RealD([n1; n2]))
    | Var(vn) when Context.mem vn map -> Environment.find (Context.find vn map) !env
    | Var(_) -> raiseException UNDEFINEDVAR !lastStat; AbstrBot
    | ArithArithUnaExpr(op, a1) -> faexpUna op (faexp ~a:a1 ~context:context ~env:env)
    | ArithArithBinExpr(a1, op, a2) -> faexpBin (faexp ~a:a1 ~context:context ~env:env) op (faexp ~a:a2 ~context:context ~env:env)
    | Deref(a1) ->
      begin match faexp ~a:a1 ~context:context ~env:env with
        | AbstrPtr(l) ->
          IdSet.fold (fun id last -> meet last (Environment.find id !env)) l abstrTop
        | _ -> raiseException PTRINTMATCH !lastStat; AbstrBot
      end
    | Addr(vn) when Context.mem vn map -> AbstrPtr(IdSet.add (Context.find vn map) IdSet.empty)
    | Addr(_) -> raiseException UNDEFINEDVAR !lastStat; AbstrBot
    | TernaryExpr(b, a1, a2) ->
      let env1 = ref (!env) in
      let env2 = ref (!env) in
      let isbImpossible = abexp ~b:b ~context:context ~env:env1 in
      let isNotbImpossible = abexp ~b:(notBoolExpr b) ~context:context ~env:env1 in
      if not isbImpossible && not isNotbImpossible then
        let result = join (faexp ~a:a1 ~context:context ~env:env1) (faexp ~a:a2 ~context:context ~env:env2) in
        let () = env := !(environmentJoin env1 env2) in
        result
      else if not isbImpossible then
        let result = faexp ~a:a2 ~context:context ~env:env2 in
        let () = env := !env2 in
        result
      else if not isNotbImpossible then
        let result = faexp ~a:a1 ~context:context ~env:env1 in
        let () = env := !env1 in
        result
      else
        AbstrBot

and baexp ~a ~context ~p ~env : bool =
  match a with
  | Cons(n) when isInAbstractDomain n p -> false
  | Cons(_) -> true
  | Interval(n1, n2) when isInAbstractDomain n1 p && isInAbstractDomain n2 p -> false
  | Interval(_, _) -> true
  | Var(vn) ->
    begin match context with
      | AbstrContext(map) when Context.mem vn map ->
        let vn_id = Context.find vn map in
        let possibleDomain = meet p (Environment.find vn_id !env) in
        if possibleDomain = AbstrBot
        then true
        else let () = env := Environment.add vn_id possibleDomain !env in false
      | AbstrContext(_) -> raiseException UNDEFINEDVAR !lastStat; false
      | _ -> false
    end
  | ArithArithUnaExpr(op, a1) -> baexp ~a:a1 ~context:context ~p:(baexpUna op (faexp ~a:a1 ~context:context ~env:env) p) ~env:env
  | ArithArithBinExpr(a1, op, a2) ->
    let env1 = ref (!env) in
    let env2 = ref (!env) in
    let (a1', a2') = (faexp ~a:a1 ~context:context ~env:env1, faexp ~a:a2 ~context:context ~env:env2) in
    let (p1, p2) = baexpBin a1' op a2' p in
    (*print_endline (arith_expr2string a ^ " : I had a1 = " ^ abstrD2string a1' ^ " and a2 = " ^ abstrD2string a2' ^ ", I wanted p = " ^ abstrD2string p ^ ", I got p1 = " ^ abstrD2string p1 ^ " and p2 = " ^ abstrD2string p2);*)
    if (baexp ~a:a1 ~context:context ~p:p1 ~env:env1) || (baexp ~a:a2 ~context:context ~p:p2 ~env:env2)
    then true
    else let () = env := !(environmentMeet env1 env2) in false
  |  Deref(_) ->
    meet (faexp ~a:a ~context:context ~env:env) p = AbstrBot
  | Addr(vn) ->
    begin match context with
      | AbstrContextBot -> true
      | GoForward(_,_) -> failwith "baexp: I can't handle GoForward contexts!"
      | AbstrContext(map) when Context.mem vn map ->
        meet (AbstrPtr(IdSet.add (Context.find vn map) IdSet.empty)) p = AbstrBot
      | AbstrContext(_) -> raiseException UNDEFINEDVAR !lastStat; false
    end
  | TernaryExpr(b, a1, a2) ->
    let env1 = ref (!env) in
    let env2 = ref (!env) in
    let p1 = if abexp ~b:b ~context:context ~env:env1 then AbstrBot else faexp ~a:a1 ~context:context ~env:env1 in
    let p2 = if abexp ~b:(notBoolExpr b) ~context:context ~env:env2 then AbstrBot else faexp ~a:a2 ~context:context ~env:env2 in
    meet p1 p = AbstrBot && meet p2 p = AbstrBot


and abexp ~b ~context ~env : bool =
  match context with
  | AbstrContextBot -> true
  | GoForward(_,_) -> failwith "abexp: I can't handle GoForward contexts!"
  | AbstrContext(_) ->
    match b with
    | True -> false
    | False -> true
    | BoolArithBinExpr(a1, op, a2) ->
      (*print_endline ("\na1 op a2 = " ^ bool_expr2string b ^ ", faexp(a1)=" ^ abstrD2string (faexp a1 r) ^ ", faexp(a2)=" ^ abstrD2string (faexp a2 r));*)
      let (p1, p2) = abexpArithBin (faexp ~a:a1 ~context:context ~env:env) op (faexp ~a:a2 ~context:context ~env:env) in
      baexp ~a:a1 ~context:context ~p:p1 ~env:env || baexp ~a:a2 ~context:context ~p:p2 ~env:env
    | BoolBinExpr(b1, op, b2) ->
      let env1 = ref (!env) in
      let env2 = ref (!env) in
      let (newEnv, contextBot) = abexpBoolBin (env1, abexp ~b:b1 ~context:context ~env:env1) op (env2, abexp ~b:b2 ~context:context ~env:env2) in
      env := !newEnv;
      contextBot
    | BoolUnaExpr(op, b1) ->
      match op with Not -> abexp ~b:(notBoolExpr b1) ~context:context ~env:env

let rec matchCArgs ~args_def ~args ~contextToEnhance ~context ~env : abstrContext =
  match (args_def, args) with
  | ([], []) -> contextToEnhance
  | ((_, var)::t1, a::t2) ->
    let v = faexp ~a:a ~context:context ~env:env in
    let newContext = addVarToContext ~vn:var ~value:v ~env:env contextToEnhance in
    matchCArgs ~args_def:t1 ~args:t2 ~contextToEnhance:newContext ~context:context ~env:env
  | _ -> failwith ("matchArgs: Wrong arguments given in statement " ^ match !lastStat with StatA(_,s,_) -> stat_a2string s "" | StatC(_,s,_) -> stat_c2string s "")

let rec matchAsmArgs ~context ~args_def ~stack ~env : abstrContext * memoryStack =
  match (args_def, stack) with
  | ([], _) -> (context, stack)
  | ((_, vn)::t, h::stack') ->
    let newContext = addVarToContext ~vn:vn ~value:h ~env:env context in
    matchAsmArgs ~context:newContext ~args_def:t ~stack:stack' ~env:env
  | (_, []) -> failwith "matchAsmArgs: Number of arguments of the C function doesn't match the size of the memory stack!"

let prelude_asm ~context ~env : abstrContext =
  context
  |> addVarToContext ~vn:eax_reg ~value:abstrTop ~env:env
  |> addVarToContext ~vn:ebx_reg ~value:abstrTop ~env:env
  |> addVarToContext ~vn:ecx_reg ~value:abstrTop ~env:env
  |> addVarToContext ~vn:edx_reg ~value:abstrTop ~env:env

let reset_registers ~context ~env ~eax_value : unit =
  match context with
  | AbstrContext(map) ->
    env :=
      !env
      |> Environment.add (Context.find eax_reg map) eax_value
      |> Environment.add (Context.find ebx_reg map) abstrTop
      |> Environment.add (Context.find ecx_reg map) abstrTop
      |> Environment.add (Context.find edx_reg map) abstrTop
  | _ -> failwith "reset_registers: Can't reset an empty context!"

let setRegisters ~realContext ~contextWithRegs ~env : unit =
  updateVarInContext ~vn:eax_reg ~value:(getVar ~context:contextWithRegs ~vn:eax_reg ~env:env) ~context:realContext ~env:env;
  updateVarInContext ~vn:ebx_reg ~value:(getVar ~context:contextWithRegs ~vn:ebx_reg ~env:env) ~context:realContext ~env:env;
  updateVarInContext ~vn:ecx_reg ~value:(getVar ~context:contextWithRegs ~vn:ecx_reg ~env:env) ~context:realContext ~env:env;
  updateVarInContext ~vn:edx_reg ~value:(getVar ~context:contextWithRegs ~vn:edx_reg ~env:env) ~context:realContext ~env:env

let rec aPostAsm (l1, stat, l2) ~shouldGoBackOnJmp ~f ~callStack ~stack ~env ~context : abstrContext * memoryStack =
  lastStat := StatA(l1, stat, l2);
  match context with
  | AbstrContextBot -> (AbstrContextBot, stack)
  | GoForward(goal, newContext) when l1 = goal -> aPostAsm (l1, stat, l2) ~shouldGoBackOnJmp:shouldGoBackOnJmp ~f:f ~callStack:callStack ~stack:stack ~env:env ~context:newContext
  | GoForward(_,_) -> (context, stack)
  | AbstrContext(_) ->
    match (l1, stat, l2) with
    (* ### MOV ### *)
    | (Loc_a(_), Mov(ae1, ae2), Loc_a(_)) ->
      begin match ae1 with
        | AsmReg(r) ->
          let result = getAsmValue ~ae:ae2 ~context:context ~env:env in
          updateVarInContext~vn:(getRegNameVar r) ~value:result ~context:context ~env:env
        | AsmVar(vn) ->
          let result = getAsmValue ~ae:ae2 ~context:context ~env:env in
          updateVarInContext ~vn:vn ~value:result ~context:context ~env:env
        | _ -> failwith "aPostAsm: I can't MOV something to an absolute address!"
      end;
      (context, stack)
    (* ### JMP ### *)
    | (Loc_a(_), Jmp(ae), Loc_a(_)) ->
      let goal =
        match ae with
        | AsmLblLoc(goal) -> goal
        | AsmLbl(_) -> failwith "aPostAsm: An ASM statement contains labels what is prohibited during the execution!"
        | AsmVar(vn) ->
          begin match getVar ~context:context ~vn:vn ~env:env with
            | AbstrLoc(goal) -> goal
            | _ -> failwith "aPostAsm: I found a variable which is not an AbstrLoc one in a Jmp statement!"
          end
        | AsmReg(reg) ->
          begin match getVar ~context:context ~vn:(getRegNameVar reg) ~env:env with
            | AbstrLoc(goal) -> goal
            | _ -> failwith "aPostAsm: I found a register which is not an AbstrLoc one in a Jmp statement!"
          end
        | _ -> failwith ("aPostAsm: I tried to jump to a weird location : " ^ asm_expr2string ae)
      in
      let (lastFun,_) = getLastFunction ~l:l1 ~f:f in
      if shouldGoBackOnJmp
      then raise GoBackward
      else
        begin
          let (realLastFun,_) = getLastFunction ~l:goal ~f:f in
          (* I just check if the arguments are the same. If so, I don't have to remove the C variables from the context even if the function is not the same. *)
          let sameArgs =
            match (realLastFun, lastFun) with
            | (Function(_,_,realArgs,_), Function(_,_,lastArgs,_)) when realArgs = lastArgs -> true
            | _ -> false
          in
          begin match realLastFun with
            | Function(_,_,_, block) ->
              let (newContext, newStack) = aPostCBlock block ~shouldGoBackOnJmp:false ~f:f ~callStack:callStack ~lastAsmStack:stack ~env:env ~context:(GoForward(goal, if sameArgs then context else removeCVariables context)) in
              begin match Stack.top !callStack with
                | (true,_,_,_,_) ->
                  raiseReturn ~cCall:true ~addReturnPossibility:false ~returnCode:abstrTop ~returnContext:newContext ~returnEnv:env ~callStack:callStack ~stack:newStack;
                  failwith "aPostC: Not supposed to happen!"
                | _ -> failwith "aPostASM: No ret found after the JMP in the same block!"
              end
            | FunctionWithoutLoc(_,_,_,_) -> failwith "aPostASM: I found a FunctionWithoutLoc in a located program!"
          end
        end
    (* ### CALL ### *)
    | (Loc_a(_), Call(ae), Loc_a(_)) ->
      let entryPoint =
        match ae with
        | AsmReg(reg) ->
          begin match getVar ~context:context ~vn:(getRegNameVar reg) ~env:env with
            | AbstrLoc(goal) -> goal
            | _ -> failwith "aPostASM: This registry doesn't contain a location!"
          end
        | AsmVar(vn) ->
          begin match getVar ~context:context ~vn:vn ~env:env with
            | AbstrLoc(goal) -> goal
            | _ -> failwith "aPostASM: This variable doesn't contain a location!"
          end
        | AsmLblLoc(goal) -> goal
        | _ -> failwith "aPostASM: This asm expression doesn't contain a location!"
      in
      begin match getLastFunction ~l:entryPoint ~f:f with
        | (FunctionWithoutLoc(_,_,_,_),_) -> failwith "aPostASM: I found a FunctionWithoutLoc in a located program!"
        | (Function(_,_, args, (l_block, block)), contextAtFun) ->
          match entryPoint with
          | Loc_c(_) -> (* ASM->C *)
            initFunction ~cCall:true ~callStack:callStack;
            begin
              let (contextAtFunWithArgs, stack') = matchAsmArgs ~context:contextAtFun ~args_def:args ~stack:stack ~env:env in
              try
                let (newContext, newStack) = aPostCBlock (l_block, block) ~shouldGoBackOnJmp:false ~f:f ~callStack:callStack ~lastAsmStack:[AbstrLoc(l2)] ~env:env ~context:contextAtFunWithArgs in
                raiseReturn ~cCall:true ~addReturnPossibility:false ~returnCode:abstrTop ~returnContext:newContext ~returnEnv:env ~callStack:callStack ~stack:newStack;
                failwith "aPostASM: Not supposed to happen!"
              with ReturnRaised(result, newEnv, newContext, newStack) ->
                env := !newEnv;
                let _ = Stack.pop !callStack in
                if newStack <> [AbstrLoc(l2)] then raiseException WRONGSTACKOFFSET !lastStat;
                reset_registers ~context:context ~env:env ~eax_value:result;
                forgetNewVariables ~lastContext:context ~newContext:newContext ~env:env;
                (context, stack')
            end
          | Loc_a(_) -> (* ASM->ASM *)
            initFunction ~cCall:false ~callStack:callStack;
            let contextAtFunWithRegs = prelude_asm ~context:contextAtFun ~env:env in
            setRegisters ~realContext:contextAtFunWithRegs ~contextWithRegs:context ~env:env;
            begin
              try
                let _ = aPostCBlock (l_block, block) ~shouldGoBackOnJmp:false ~f:f ~callStack:callStack ~lastAsmStack:((AbstrLoc(l2))::stack) ~env:env ~context:(GoForward(entryPoint, contextAtFunWithRegs)) in
                failwith "aPostASM: Call ASM->ASM failed because of the lack of a return!"
              with ReturnRaised(_, newEnv, newContext, memoryStackReturned) ->
                env := !newEnv;
                let _ = Stack.pop !callStack in
                setRegisters ~realContext:context ~contextWithRegs:newContext ~env:env;
                forgetNewVariables ~lastContext:context ~newContext:newContext ~env:env;
                match memoryStackReturned with
                | (AbstrLoc(goal))::newStack when goal = l2 -> (context, newStack)
                | (AbstrLoc(goal))::newStack ->
                  begin
                    try
                      let _ = aPostAsm (l1, Jmp(AsmLblLoc(goal)), l2) ~shouldGoBackOnJmp:false ~f:f ~callStack:callStack ~stack:newStack ~env:env ~context:context in
                      failwith "aPostASM: I should have raise a ReturnRaised exception !"
                    with ReturnRaised(codeReturned, newEnv,_, memoryStackReturned) ->
                      raise (ReturnRaised(codeReturned,newEnv, context, memoryStackReturned))
                  end
                | _ -> raiseException WRONGSTACKOFFSET !lastStat; (context, memoryStackReturned)
            end
      end
    (* ### RET ### *)
    | (Loc_a(_), Ret, Loc_a(_)) ->
      let returnCode = getVar ~context:context ~vn:eax_reg ~env:env in
      raiseReturn ~cCall:false ~addReturnPossibility:false ~returnCode:returnCode ~returnContext:context ~returnEnv:env ~callStack:callStack ~stack:stack;
      failwith "aPostAsm: Not supposed to happen, should have raised a returnRaised exception!"
    | (Loc_a(_), Push(ae), Loc_a(_)) ->
      (context, (getAsmValue ~ae:ae ~context:context ~env:env)::stack)
    (* ### POP ### *)
    | (Loc_a(_), Pop(ae), Loc_a(_)) ->
      let (result, newStack) =
        match stack with
        | [] -> raiseException WRONGSTACKOFFSET !lastStat; (abstrTop, [])
        | h::t -> (h, t)
      in
      begin
        match ae with
        | AsmReg(reg) -> updateVarInContext ~vn:(getRegNameVar reg) ~value:result ~context:context ~env:env
        | AsmVar(vn) -> updateVarInContext ~vn:vn ~value:result ~context:context ~env:env
        | _ -> failwith ("aPostAsm: I can't set the value of the ASM expression " ^ asm_expr2string ae)
      end;
      (context, newStack)
    (* ### OTHERS ### *)
    | (Loc_a(_), Label(_), Loc_a(_)) -> failwith "aPostAsm: An ASM statement contains labels what is prohibited during the execution!"
    | (Loc_c(_), s, _) | (_, s, Loc_c(_)) -> failwith ("aPostAsm: The statement " ^ stat_a2string s "" ^ " has invalid locations!")

and aPostAsmBlock (pre_loc, block) ~shouldGoBackOnJmp ~f ~callStack ~stack ~env context : abstrContext * memoryStack =
  match block with
  | [] ->
    begin match context with
      | GoForward(goal, newContext) when pre_loc = goal ->  (newContext, stack)
      | _ -> (context, stack)
    end
  | (stat, post_loc)::t ->
    let (newContext, stack') = aPostAsm (pre_loc, stat, post_loc) ~shouldGoBackOnJmp:shouldGoBackOnJmp ~f:f ~callStack:callStack ~stack:stack ~env:env ~context:context in
    aPostAsmBlock (post_loc, t) ~shouldGoBackOnJmp:shouldGoBackOnJmp ~f:f ~callStack:callStack ~stack:stack' ~env:env newContext

and aPostCBlock (pre_loc, program : loc_stat_c) ~shouldGoBackOnJmp ~f ~callStack ~lastAsmStack ~env ~context : abstrContext * memoryStack =
  match context with
  | AbstrContextBot -> (AbstrContextBot, lastAsmStack)
  | GoForward(goal, newContext) when program = [] && pre_loc = goal -> (newContext, lastAsmStack)
  | _ ->
    match program with
    | [] -> (context, lastAsmStack)
    | (stat, post_loc)::t ->
      let (newContext, newAsmStack) = aPostC (pre_loc, stat, post_loc) ~shouldGoBackOnJmp:shouldGoBackOnJmp ~f:f ~callStack:callStack ~lastAsmStack:lastAsmStack ~env:env ~context:context in
      aPostCBlock (post_loc, t) ~shouldGoBackOnJmp:shouldGoBackOnJmp ~f:f ~callStack:callStack ~lastAsmStack:newAsmStack ~env:env ~context:newContext

and aPostC (l1, stat, l2 : loc * stat_c * loc) ~shouldGoBackOnJmp ~f ~callStack ~lastAsmStack ~env ~context : abstrContext * memoryStack =
  (* print_endline ("C statement read: " ^ stat_c2string stat ""); *)
  lastStat := StatC(l1, stat, l2);
  match context with
  | AbstrContextBot -> (AbstrContextBot, lastAsmStack)
  | GoForward(goal, newContext) ->
    if goal = l1
    then aPostC (l1, stat, l2) ~shouldGoBackOnJmp:shouldGoBackOnJmp ~f:f ~callStack:callStack ~lastAsmStack:lastAsmStack ~env:env ~context:newContext
    else
      let goalNumber = getLocationNumber goal in
      begin match stat with
        | Branch(_, (l1', block1), (l2', block2)) ->
          if getLocationNumber l2' > goalNumber
          then aPostCBlock (l1', block1) ~shouldGoBackOnJmp:shouldGoBackOnJmp ~f:f ~callStack:callStack ~lastAsmStack:lastAsmStack ~env:env ~context:context
          else if getLocationNumber l2 > goalNumber
          then aPostCBlock (l2', block2) ~shouldGoBackOnJmp:shouldGoBackOnJmp ~f:f ~callStack:callStack ~lastAsmStack:lastAsmStack ~env:env ~context:context
          else (context, lastAsmStack)
        | While(b, (l1', block1)) ->
          if getLocationNumber l2 > goalNumber
          then
            begin
              let aPostB (context, lastStack) =
                (* abexp changes only the environment, not the context! *)
                if abexp ~b:b ~context:context ~env:env
                then (AbstrContextBot, lastStack)
                else (context, lastStack)
              in
              let aPostS (context, lastStack) =
                (* Performs an iteration of the loop *)
                aPostCBlock (l1', block1) ~shouldGoBackOnJmp:shouldGoBackOnJmp ~f:f ~callStack:callStack ~lastAsmStack:lastStack ~env:env ~context:context
              in
              let aPostNotB (context, lastStack) =
                if abexp ~b:(notBoolExpr b) ~context:context ~env:env
                then (AbstrContextBot, lastStack)
                else (context, lastStack)
              in
              (context, lastAsmStack) |> aPostS |> lfp ~env:env ~e1:(fun x -> x |> aPostB |> aPostS) |> (fun newStack -> (newContext, newStack)) |> aPostNotB
            end
          else (context, lastAsmStack)
        | Asm((l1', block1)) ->
          if getLocationNumber l2 > goalNumber
          then
            begin
              let (newContext, memoryStack') = aPostAsmBlock (l1', block1) ~shouldGoBackOnJmp:shouldGoBackOnJmp ~f:f ~callStack:callStack ~stack:lastAsmStack ~env:env context in
              (postlude_asm ~context:newContext ~env:env, memoryStack')
            end
          else (context, lastAsmStack)
        | _ -> (context, lastAsmStack)
      end
  | AbstrContext(_) ->
    match (l1, stat, l2) with
    (* ### DECLARE ### *)
    | (Loc_c(_), Declare(_, vn), Loc_c(_)) ->
      (addVarToContext ~vn:vn ~value:abstrTop ~env:env context, lastAsmStack)
    (* ### ASSIGN ### *)
    | (Loc_c(_), Assign(vn, a), Loc_c(_)) ->
      updateVarInContext ~vn:vn ~value:(faexp ~a:a ~context:context ~env:env) ~context:context ~env:env;
      (context, lastAsmStack)
    (* ### DECLARE_ASSIGN *)
    | (Loc_c(_), DeclareAssign(_, vn, a), Loc_c(_)) ->
      (addVarToContext ~vn:vn ~value:(faexp ~a:a ~context:context ~env:env) ~env:env context, lastAsmStack)
    (* ### BRANCH ### *)
    | (Loc_c(_), Branch(b, (Loc_c(lt), statS), (Loc_c(lf), statF)), Loc_c(_)) ->
      begin
        let envS = ref (!env) in
        let envF = ref (!env) in
        let cS = if abexp ~b:b ~context:context ~env:envS then AbstrContextBot else context in
        let cF = if abexp ~b:(notBoolExpr b) ~context:context ~env:envF then AbstrContextBot else context in
        let lastCallStack = !callStack in (* To reset the callStack if we go backward *)
        try
          let ((_, stackF), statFReturn) =
            try (aPostCBlock (Loc_c(lf), statF) ~shouldGoBackOnJmp:true ~f:f ~callStack:callStack ~lastAsmStack:lastAsmStack ~env:envF ~context:cF, cF = AbstrContextBot)
            with ReturnRaised(_,_,_,stack) -> ((cF, stack), true)
          in
          let ((_, stackS), statSReturn) =
            try (aPostCBlock (Loc_c(lt), statS) ~shouldGoBackOnJmp:true ~f:f ~callStack:callStack ~lastAsmStack:lastAsmStack ~env:envS ~context:cS, cS = AbstrContextBot)
            with ReturnRaised(_,_,_,stack) -> ((cS, stack), true)
          in
          if not statFReturn && not statSReturn then
            let () = env := !(environmentJoin envF envS) in
            (context, joinMemoryStack ~s1:stackF ~s2:stackS)
          else if statFReturn && statSReturn then
            match Stack.top !callStack with (_,returnCode, newEnv, newContext, newStack) ->
              raise (ReturnRaised(returnCode, newEnv, newContext, newStack))
          else if statFReturn then
            let () = env := !envS in
            (context, stackS)
          else
            let () = env := !envF in
            (context, stackF)
        with GoBackward ->
          begin
            match getLastFunction ~l:l1 ~f:f with
            | (Function(_,_,_, block),_) ->
              begin
                callStack := lastCallStack;
                let env1 = ref (!env) in
                let env2 = ref (!env) in
                let cS = if abexp ~b:b ~context:context ~env:env1 then AbstrContextBot else context in
                let cF = if abexp ~b:(notBoolExpr b) ~context:context ~env:env2 then AbstrContextBot else context in
                let cCall = match Stack.top !callStack with (cCall,_,_,_,_) -> cCall in
                let () = try
                    if cS <> AbstrContextBot then
                      let (_, newStack) = aPostCBlock block ~shouldGoBackOnJmp:false ~f:f ~callStack:callStack ~lastAsmStack:lastAsmStack ~env:env1 ~context:(GoForward((Loc_c(lt)), cS))
                      in raiseReturn ~cCall:cCall ~addReturnPossibility:true ~returnCode:abstrTop ~returnContext:cS ~returnEnv:env1 ~callStack:callStack ~stack:newStack
                  with ReturnRaised(_,_,_,_) -> ()
                in
                let () = try
                    if cF <> AbstrContextBot then
                      let (_, newStack) = aPostCBlock block ~shouldGoBackOnJmp:false ~f:f ~callStack:callStack ~lastAsmStack:lastAsmStack ~env:env2 ~context:(GoForward((Loc_c(lf)), cF))
                      in raiseReturn ~cCall:cCall ~addReturnPossibility:true ~returnCode:abstrTop ~returnContext:cF ~returnEnv:env2 ~callStack:callStack ~stack:newStack
                  with ReturnRaised(_,_,_,_) -> ()
                in
                match Stack.top !callStack with (_,returnCode, newEnv, newContext, newStack) ->
                  raise (ReturnRaised(returnCode, newEnv, newContext, newStack))
              end
            | (FunctionWithoutLoc(_,_,_,_),_) -> failwith "aPostC: I have a FunctionWithoutLoc in a located program!"
          end
      end
    (* ### ASM ### *)
    | (Loc_c(_), Asm(_, []), Loc_c(_)) -> (context, lastAsmStack) (* I added this case in order to avoid prelude and postlude to ASM which are useless *)
    | (Loc_c(_), Asm(l_block, block), Loc_c(_)) ->
      let (newContext, newStack) = prelude_asm ~context:context ~env:env |> aPostAsmBlock (l_block, block) ~shouldGoBackOnJmp:shouldGoBackOnJmp ~f:f ~callStack:callStack ~stack:lastAsmStack ~env:env in
      (postlude_asm ~context:newContext ~env:env, newStack)
    (* ### PTRASSIGN ### *)
    | (Loc_c(_), PtrAssign(ptr, a), Loc_c(_)) ->
      begin
        let v = faexp ~a:a ~context:context ~env:env in
        match context with
        | AbstrContext(map) ->
          let rec derefInfos (ptr : ptr_var) : int * name_var =
            match ptr with
            | PtrVar(ptr') -> let (nbrDeref, lastNameVar) = derefInfos ptr' in (nbrDeref+1, lastNameVar)
            | LastPtrVar(vn) -> (1, vn)
          in
          let (nbrDeref, lastNameVar) = derefInfos ptr in
          let rec unfoldPtrVar (remainingNbrDeref : int) (id : int) : IdSet.t =
            if remainingNbrDeref = 0
            then IdSet.add id IdSet.empty
            else
              match Environment.find id !env with
              | AbstrPtr(l) -> IdSet.fold (fun id' last -> IdSet.union last (unfoldPtrVar (remainingNbrDeref-1) id')) l IdSet.empty
              | _ -> raiseException PTRINTMATCH !lastStat; IdSet.empty
          in
          if Context.mem lastNameVar map
          then
            let pointedVars = unfoldPtrVar nbrDeref (Context.find lastNameVar map) in
            let rec updateVars (l : int list) : unit =
              match l with
              | h::t -> env := Environment.add h (join v (Environment.find h !env)) !env; updateVars t
              | [] -> ()
            in
            match IdSet.elements pointedVars with
            | [] -> ()
            | [h] -> env := Environment.add h v !env
            | t -> updateVars t
          else
            raiseException UNDEFINEDVAR !lastStat
        | _ -> raiseException UNDEFINEDVAR !lastStat
      end;
      (context, lastAsmStack)
    (* ### CALLASSIGNC ### *)
    | (Loc_c(_), CallAssignC(var, fun_name, args), Loc_c(_)) ->
      begin match context with
        | AbstrContext(map) when Context.mem fun_name map && Context.mem var map ->
          begin match Environment.find (Context.find fun_name map) !env with
            | AbstrLoc(entryPoint) when LocMap.mem entryPoint !f ->
              begin match LocMap.find entryPoint !f with
                | (Function(_, fun_name', args_def, (l, block)), funContext) when fun_name = fun_name' && List.length args_def = List.length args ->
                  let funContext = matchCArgs ~args_def:args_def ~args:args ~contextToEnhance:funContext ~context:context ~env:env in
                  initFunction ~cCall:true ~callStack:callStack;
                  let result =
                    try
                      let (newContext, newStack) = aPostCBlock (l, block) ~shouldGoBackOnJmp:false ~f:f ~callStack:callStack ~lastAsmStack:[AbstrLoc(l2)] ~env:env ~context:funContext in
                      raiseReturn ~cCall:true ~addReturnPossibility:false ~returnCode:abstrTop ~returnContext:newContext ~returnEnv:env ~callStack:callStack ~stack:newStack;
                      failwith "aPostC: Not supposed to happen"
                    with ReturnRaised(result, newEnv, newContext, newStack) ->
                      env := !newEnv;
                      if newStack <> [AbstrLoc(l2)] then raiseException WRONGSTACKOFFSET !lastStat;
                      forgetNewVariables ~lastContext:context ~newContext:newContext ~env:env;
                      result
                  in
                  let _ = Stack.pop !callStack in
                  updateVarInContext ~vn:var ~value:result ~context:context ~env:env;
                  (context, lastAsmStack)
                | _ -> raiseException UNDEFINEDVAR !lastStat; (context, lastAsmStack)
              end
            | _ -> raiseException UNDEFINEDVAR !lastStat; (context, lastAsmStack)
          end
        | _ -> raiseException UNDEFINEDVAR !lastStat; (context, lastAsmStack)
      end
    (* ### CALLASSIGNASM ### *)
    | (Loc_c(_), CallAssignAsm(var, fun_loc, args), Loc_c(_)) ->
      begin match context with
        | AbstrContext(map) when Context.mem var map ->
          begin match getLastFunction ~l:fun_loc ~f:f with
            | (Function(_,_,_,(l, block)), funContext) ->
              let funContextWithRegs = prelude_asm ~context:funContext ~env:env in
              let rec calcArgs (args : arith_expr list) (result : abstrD list) : abstrD list =
                match args with
                | [] -> result
                | h::t -> calcArgs t ((faexp ~a:h ~context:context ~env:env)::result)
              in
              initFunction ~cCall:false ~callStack:callStack;
              let result =
                try
                  let (newContext, newStack) = aPostCBlock (l, block) ~shouldGoBackOnJmp:false ~f:f ~callStack:callStack ~lastAsmStack:((AbstrLoc(l2))::(calcArgs args lastAsmStack)) ~env:env ~context:(GoForward(fun_loc, funContextWithRegs)) in
                  raiseReturn ~cCall:false ~addReturnPossibility:false ~returnCode:abstrTop ~returnContext:newContext ~returnEnv:env ~callStack:callStack ~stack:newStack;
                  failwith "aPostC: Not supposed to happen"
                with
                | ReturnRaised(result, newEnv, newContext, newStack) ->
                  env := !newEnv;
                  if List.length newStack <> 2 || List.hd newStack <> AbstrLoc(l2) then raiseException WRONGSTACKOFFSET !lastStat;
                  forgetNewVariables ~lastContext:context ~newContext:newContext ~env:env;
                  result
              in
              let _ = Stack.pop !callStack in
              updateVarInContext ~vn:var ~value:result ~context:context ~env:env;
              (context, lastAsmStack)
            | _ -> failwith "aPostC: The ASM function is not in a C function!"
          end
        | _ -> raiseException UNDEFINEDVAR !lastStat; (context, lastAsmStack)
      end
    (* ### RETURN ### *)
    | (Loc_c(_), Return(a), Loc_c(_)) ->
      raiseReturn ~cCall:true ~addReturnPossibility:false ~returnCode:(faexp ~a:a ~context:context ~env:env) ~returnContext:context ~returnEnv:env ~callStack:callStack ~stack:lastAsmStack;
      failwith "aPostC: Not supposed to happen, should have raised a returnRaised exception!"
    (* ### WHILE ### *)
    | (Loc_c(_), While(b, (Loc_c(lt), statS)), Loc_c(_)) ->
      let aPostB (context, lastStack) =
        if abexp ~b:b ~context:context ~env:env
        then (AbstrContextBot, lastStack)
        else (context, lastStack)
      in
      let aPostS (context, lastStack) =
        aPostCBlock (Loc_c(lt), statS) ~shouldGoBackOnJmp:shouldGoBackOnJmp ~f:f ~callStack:callStack ~lastAsmStack:lastStack ~env:env ~context:context (* Performs an iteration of the loop *)
      in
      let aPostNotB (context, lastStack) =
        if abexp ~b:(notBoolExpr b) ~context:context ~env:env
        then (AbstrContextBot, lastStack)
        else (context, lastStack)
      in
      (context, lastAsmStack) |> lfp ~env:env ~e1:(fun x -> x |> aPostB |> aPostS) |> (fun newStack -> (context, newStack)) |> aPostNotB
    (* ### OTHERS ### *)
    | (_, BranchWithoutLoc(_,_,_), _) -> failwith "aPostC: BranchWithoutLoc found in the program!"
    | (_, WhileWithoutLoc(_, _), _) -> failwith "aPostC: WhileWithoutLoc found in the programe"
    | (_, AsmWithoutLoc(_), _) -> failwith "aPostC: AsmWithoutLoc found in the program!"
    | (_, Branch(_,_,_), _) | (_, While(_, _), _) -> failwith ("aPostC: The statement at location " ^ loc2string l1 ^ " has invalid locations!")
    | (Loc_a(_), s, _) | (_, s, Loc_a(_)) -> failwith ("aPostC: The statement " ^ stat_c2string s "" ^ " has invalid locations!")

let aPostGlobalC ~pre_loc ~stat ~post_loc ~f ~env context : abstrContext =
  match (pre_loc, stat, post_loc) with
  | (Loc_c(_), Function(_, vn, _, _), Loc_c(_)) ->
    let newContext = addVarToContext ~vn:vn ~value:(AbstrLoc(pre_loc)) ~env:env context in
    f := LocMap.add pre_loc (stat, newContext) !f;
    newContext
  | (Loc_c(_), FunctionWithoutLoc(_,_,_,_), Loc_c(_)) -> failwith "aPostGlobalC: FunctionWithoutLoc found in the program!"
  | (Loc_a(_), _, _) | (_, _, Loc_a(_)) -> failwith ("aPostGlobalC: The statement at location " ^ loc2string pre_loc ^ " has invalid locations!")

let rec aPostProgram ~pre_loc ~program ~f ~env context : abstrContext =
  match program with
  | [] -> context
  | (stat, post_loc)::t -> context |> aPostGlobalC ~pre_loc:pre_loc ~stat:stat ~post_loc:post_loc ~f:f ~env:env |> aPostProgram ~pre_loc:post_loc ~program:t ~f:f ~env:env