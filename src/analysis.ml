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

open GenericAbstract
open Types
open Exceptions
open Debug

let lastStat = (StatC(Loc_c(-1), Declare(Int_c, "END_OF_PROGRAM"), Loc_c(-1)))

let analyseProgram (l, program) : unit =
  exceptions := [];
  let env = ref Environment.empty in
  let f = ref LocMap.empty in
  let lastContext = aPostProgram ~pre_loc:l ~program:program ~f:f ~env:env (AbstrContext(Context.empty)) in
  let (endContext, returnCode) = 
    match lastContext with
    | AbstrContext(mapContext) when Context.mem "main" mapContext ->
      begin match Environment.find (Context.find "main" mapContext) !env with
        | AbstrLoc(entryPoint) when LocMap.mem entryPoint !f ->
          begin match LocMap.find entryPoint !f with
            | (Function(Int_c, "main", [], (l, stats)), entryContext) ->
              begin
                let callStack = ref (Stack.create ()) in
                try
                  initFunction ~cCall:true ~callStack:callStack;
                  let (endContext, finalStack) = aPostCBlock (l, stats) ~shouldGoBackOnJmp:false ~f:f ~callStack:callStack ~lastAsmStack:[AbstrLoc(Loc_c(-1))] ~env:env ~context:entryContext in
                  raiseReturn ~cCall:true ~addReturnPossibility:false ~returnCode:abstrTop ~returnContext:endContext ~returnEnv:env ~callStack:callStack ~stack:finalStack;
                  failwith "analyseProgram: Not supposed to happen!"
                with ReturnRaised(result, endEnv, endContext, stack) ->
                  env := !endEnv;
                  if stack <> [AbstrLoc(Loc_c(-1))] then raiseException WRONGSTACKOFFSET lastStat;
                  if Stack.length !callStack <> 1 then raiseException NONEMPTYCALLSTACK lastStat;
                  (endContext, result)
              end
            | _ -> failwith "analyseProgram: invalid main function!"
          end
        | _ -> failwith "analyseProgram: no main function found!"
      end
    | _ -> failwith "analyseProgram: no main function found!"
  in
  print_endline ("\n### Final size of the environment: " ^ string_of_int (Environment.cardinal !env));
  print_endline "\n### Final context:";
  print_context endContext env;
  print_endline "\n### Return code:";
  print_endline (abstrD2string returnCode);
  print_endline "\n### Exceptions:";
  handleExceptions ();
  exceptions := [];
  match endContext with
  | AbstrContextBot | GoForward(_,_) -> ()
  | AbstrContext(map) ->
    if Environment.cardinal !env <> Context.cardinal map
    then Printf.eprintf "Error: Their is a lack of memory: the environment contains variables which are not referenced!\n"
