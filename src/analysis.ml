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
open Utils

(** Analyse a program statically with abstract interpretation *)

(** Analyse a program and write results of this analysis or exceptions encountered
    @param l The initial location of [program]
    @param program The program already located to analyse
    @return unit *)
let analyseProgram (l, program) : unit =
  exceptions := [];
  let j = replaceContext (Loc_c(0)) (AbstrContext(Context.empty)) (AbstrEnvironment(Environment.empty)) in
  let f = ref Environment.empty in
  let j' = aPostProgram l program f j in
  let lastLoc = lastLocOfProgram (l, program) in
  let (jl, returnCode, stack) = match j' with
    | AbstrEnvironment(mapEnv) when Environment.mem lastLoc mapEnv ->
      begin match Environment.find lastLoc mapEnv with
        | AbstrContext(mapContext) when Context.mem "main" mapContext ->
          begin match Context.find "main" mapContext with
            | AbstrLoc(entryPoint) when Environment.mem entryPoint !f ->
              begin match Environment.find entryPoint !f with
                | Function(Int_c, "main", [], (l, stats)) ->
                  begin
                    try
                      let callStack = ref (Stack.create ()) in
                      initFunction true callStack;
                      let (j'', finalStack) = aPostCBlock (l, stats) f callStack [] (replaceContext l (getContext entryPoint j' |> fst) j') in
                      let lastLoc = lastLocOfProgram (l, stats) in
                      if not (Stack.is_empty !callStack) then raiseException NONEMPTYCALLSTACK (StatC(Loc_c(-1), Declare(Int_c, "END_OF_PROGRAM"), Loc_c(-1)));
                      (getContext lastLoc j'' |> fst, abstrTop, finalStack)
                    with ReturnRaised(result, jl, stack) -> (jl, result, stack)
                  end
                | _ -> failwith "analyseProgram: invalid main function!"
              end
            | _ -> failwith "analyseProgram: no main function found!"
          end
        | _ -> failwith "analyseProgram: no main function found!"
      end
    | _ -> failwith "analyseProgram: No main function found!"
  in
  print_endline "\n### Final context:";
  print_context jl;
  print_endline "\n### Final memory stack:";
  print_memoryStack stack;
  print_endline "\n### Return code:";
  print_endline (abstrD2string returnCode);
  print_endline "\n### Exceptions:";
  handleExceptions ();
  exceptions := []
