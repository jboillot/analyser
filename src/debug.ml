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
open SortedList
open Options

(** Module which simplifies print of debug *)

(** Deserialize type
    @return The deserialized string of the type providen
    @param t C type of variable *)
let rec type2string t : string =
  match t with
  | Int_c -> "int"
  | Ptr(t1) -> (type2string t1) ^ "*"

(** Remove sometimes some parenthesis
    @param a An arithmetic expression
    @return Are parenthesis import around the expression *)
let arith_expr_needs_parenthesis a : bool =
  match a with
  | Var(_) | Deref(_) | Addr(_) | Cons(_) | Interval(_,_) -> false
  | _ -> true

(** Deserialize arithmetic expressions
    @param a The arithmetic expression
    @return The deserialized string of the arithmetic expression providen *)
let rec arith_expr2string a : string =
  match a with
  | Var(var) -> var
  | Deref(a1) ->
    if arith_expr_needs_parenthesis a1
    then "*(" ^ arith_expr2string a1 ^ ")"
    else "*" ^ arith_expr2string a1
  | Addr(var) -> "&" ^ var
  | Cons(c) -> Z.to_string c
  | Interval(n1, n2) -> "[" ^ Z.to_string n1 ^ ", " ^ Z.to_string n2 ^ "]"
  | ArithArithBinExpr(a1, op, a2) ->
    begin if arith_expr_needs_parenthesis a1
      then "(" ^ arith_expr2string a1 ^ ")"
      else arith_expr2string a1
    end
    ^ begin match op with | Plus -> " + " | Minus -> " - " | Times -> " * " | Div -> " / " end
    ^ begin if arith_expr_needs_parenthesis a1
      then "(" ^ arith_expr2string a2 ^ ")"
      else arith_expr2string a2
    end
  | ArithArithUnaExpr(op, a1) ->
    begin match op with MinusUna -> "-" | BitwiseNot -> "~" end
    ^ begin if arith_expr_needs_parenthesis a1
      then "(" ^ arith_expr2string a1 ^ ")"
      else arith_expr2string a1
    end

(** Deserialize boolean expressions
    @param b The boolean expression
    @return The deserialized string of the boolean expression providen *)
let rec bool_expr2string b : string =
  match b with
  | True -> "true"
  | False -> "false"
  | BoolBinExpr(b1, op, b2) ->
    "(" ^ bool_expr2string b1 ^ ") "
    ^ begin match op with And -> " && " | Or -> " || " end
    ^ " (" ^ bool_expr2string b2 ^ ")"
  | BoolUnaExpr(op, b1) -> 
    begin match op with Not -> "!" end ^ "(" ^ bool_expr2string b1 ^ ")"
  | BoolArithBinExpr(a1, op, a2) -> 
    begin if arith_expr_needs_parenthesis a1
      then "(" ^ arith_expr2string a1 ^ ")"
      else arith_expr2string a1
    end
    ^ begin match op with Eq -> " == " | Neq -> " != " | Lt -> " < " | Gt -> " > " | Le -> " <= " | Ge -> " >= " end
    ^ begin if arith_expr_needs_parenthesis a2
      then "(" ^ arith_expr2string a2 ^ ")"
      else arith_expr2string a2
    end

(** Deserialize location
    @param l The location
    @return The deserialized string of the location providen *)
let loc2string l : string =
  match l with
  | Loc_c(n) -> "l_c(" ^ string_of_int n ^ ")"
  | Loc_a(n) -> "l_a(" ^ string_of_int n ^ ")"

(** Deserialize arguments of a C function (which is a list of arithmetic expressions)
    @param args The arguments
    @return The deserialized string of the arguments providen *)
let rec args2string args : string =
  match args with
  | [] -> ""
  | [a] -> arith_expr2string a
  | a::t -> arith_expr2string a ^ ", " ^ args2string t

(** Deserialize register
    @param a The register
    @return The name of the register providen *)
let reg2string reg : string =
  match reg with
  | EAX -> "eax"
  | EBX -> "ebx"
  | ECX -> "ecx"
  | EDX -> "edx"

(** Deserialize a pointer variable
    @param v The pointer variable
    @return The deserialized string of the pointer variable providen *)
let rec ptrVar2string v : string =
  match v with
  | PtrVar(v') -> "*" ^ ptrVar2string v'
  | LastPtrVar(s) -> s

(** Deserialize ASM expression
    @param ae The ASM expression
    @return The deserialized string of the ASM expression providen *)
let rec asm_expr2string ae : string =
  match ae with
  | AsmReg(reg) -> reg2string reg
  | AsmPtr(ae') -> "[" ^ asm_expr2string ae' ^ "]"
  | AsmVar(vn) -> vn
  | AsmCst(c) -> Z.to_string c
  | AsmLbl(lbl) -> "@" ^ lbl
  | AsmLblLoc(loc) -> loc2string loc

(** Deserialize C statement
    @param stat The C statement
    @param indent The initial prefix of the line
    @return The deserialized string of the C statement providen wich begins by the [indent] *)
let rec stat_c2string stat indent : string =
  match stat with
  | Declare(t, var) -> indent ^ type2string t ^ " " ^ var ^ ";\n"
  | Assign(var, a) -> indent ^ var ^ " = " ^ arith_expr2string a ^ ";\n"
  | DeclareAssign(t, var, a) -> indent ^ type2string t ^ " " ^ var ^ " = " ^ arith_expr2string a ^ ";\n"
  | PtrAssign(ptr, a) -> indent ^ "*" ^ ptrVar2string ptr ^ " = " ^ arith_expr2string a ^ ";\n"
  | CallAssignC(var, fun_name, args) -> indent ^ var ^ " = " ^ fun_name ^ "(" ^ args2string args ^ ");\n"
  | CallAssignAsm(var, loc, args) -> indent ^ var ^ " = " ^ loc2string loc ^ "(" ^ args2string args ^ ");\n"
  | Branch(b, s1, (_, [])) -> indent ^ "if (" ^ bool_expr2string b ^ ") {\n" ^ block_c2string s1 ("\t" ^ indent) ^ indent ^ "}\n"
  | Branch(b, s1, s2) -> indent ^ "if (" ^ bool_expr2string b ^ ") {\n" ^ block_c2string s1 ("\t" ^ indent) ^ indent ^ "} else {\n" ^ block_c2string s2 ("\t" ^ indent) ^ indent ^ "}\n"
  | Asm(l, asm_stats) -> indent ^ "asm {\n" ^ (if !withoutLocs then "" else loc2string l) ^ "\t" ^ stat_a_block2string asm_stats ("\t" ^ indent) ^ indent ^ "};\n"
  | While(b, s1) -> indent ^ "while (" ^ bool_expr2string b ^ ") {\n" ^ block_c2string s1 ("\t" ^ indent) ^ indent ^ "}\n"
  | Return(a) -> indent ^ "return " ^ arith_expr2string a ^ ";\n"
  | BranchWithoutLoc(_, _, _) -> failwith "stat_c2string: Usage of BranchWithoutLoc in a located program!"
  | AsmWithoutLoc(_) -> failwith "stat_c2string: Usage of AsmWithoutLoc in a located program!"
  | WhileWithoutLoc(_, _) -> failwith "stat_c2string: Usage of WhileWithoutLoc in a located program!"

(** Deserialize ASM statements
    @param stat The ASM statement
    @param indent The initial prefix of the line
    @return The deserialized string of the ASM statement providen which begins by the [indent] *)
and stat_a2string stat indent : string =
  match stat with
  | Label(ln) -> ln ^ ":\n"
  | Mov(ae1, ae2) -> indent ^ "MOV " ^ asm_expr2string ae1 ^ ", " ^ asm_expr2string ae2 ^ "\n"
  | Jmp(ae) -> indent ^ "JMP " ^ asm_expr2string ae ^ "\n"
  | Call(ae) -> indent ^ "CALL " ^ asm_expr2string ae ^ "\n"
  | Ret -> indent ^ "RET\n"
  | Push(ae) -> indent ^ "PUSH " ^ asm_expr2string ae ^ "\n"
  | Pop(ae) -> indent ^ "POP " ^ asm_expr2string ae ^ "\n"

(** Deserialize block of ASM statements
    @param block The block of ASM statements
    @param indent The initial indent of each line
    @return The deserialized string of the block of ASM statements providen which begins by the [indent] *)
and stat_a_block2string stat_list indent : string =
  match stat_list with
  | [] -> ""
  | (stat, l)::t -> (stat_a2string stat indent) ^ (if !withoutLocs then "" else loc2string l ^ "\t") ^ (stat_a_block2string t indent)

(** Deserialize block of C statements
    @param stat The block of C statements
    @return The deserialized string of the block of C statements providen which begins by the [indent] *)
and block_c2string block_c indent : string =
  let rec block_c_cleaned2string (program_cleaned : (stat_c * loc) list) : string =
    match program_cleaned with
    | [] -> ""
    | (stat, l)::t -> (stat_c2string stat indent) ^ (if !withoutLocs then "" else loc2string l ^ "\t") ^ (block_c_cleaned2string t)
  in (if !withoutLocs then "" else loc2string (fst block_c) ^ "\t") ^ block_c_cleaned2string (snd block_c)

(** Deserialize definition of arguments of a C function
    @param args The definition of the arguments
    @return The deserialized string of the arguments providen *)
let rec args_def2string args : string =
  match args with
  | [] -> ""
  | [t, vn] -> type2string t ^ " " ^ vn
  | (t, vn)::l -> type2string t ^ " " ^ vn ^ ", " ^ args_def2string l

(** Deserialize global statements
    @param stat The global statement
    @return The deserialized string of the global statement *)
let global_statement2string stat : string =
  match stat with
  | Function(t, vn, args, block_c) -> type2string t ^ " " ^ vn ^ "(" ^ args_def2string args ^ ") {\n" ^ block_c2string block_c "\t" ^ "}\n\n"
  | FunctionWithoutLoc(_,_,_,_) -> failwith "program: Usage of global_statement2string in a located program!"

(** Deserialize located programs
    @param l The entry point of [program]
    @param program The C program with inline assembly
    @return The deserialized string of the program *)
let program2string (l, program_cleaned) : string =
  let rec program_cleaned2string (program_cleaned : (global_statement * loc) list) : string =
    match program_cleaned with
    | [] -> ""
    | (stat, l)::t -> global_statement2string stat ^ (if !withoutLocs then "" else loc2string l ^ "\t") ^ program_cleaned2string t
  in (if !withoutLocs then "" else loc2string l ^ "\t") ^ program_cleaned2string program_cleaned

(** Deserialize abstract pointers
    @param l The abstract pointer
    @return The deserialized string of the abstract domains *)
let rec abstrPtr2string l : string =
  match l with
  | [] -> "]"
  | [h] -> ptrVar2string h ^ "]"
  | h::t -> ptrVar2string h ^ ", " ^ abstrPtr2string t

(** Deserialize an abstract domain
    @param value The abstract domain
    @return The deserialized string of the abstract domain *)
let abstrD2string value : string =
  match value with
  | AbstrBot -> "∅"
  | top when top = abstrTop -> "⊤"
  | AbstrD(n1, n2) when Z.equal n1 n2 -> "{" ^ Z.to_string n1 ^ "}"
  | AbstrD(n1, n2) -> 
    "["
    ^ (if n1 = abstrMin then "-∞" else Z.to_string n1) ^ ", "
    ^ (if n2 = abstrMax then "+∞" else Z.to_string n2) ^ "]"
  | AbstrPtr(l) -> "[" ^ abstrPtr2string l
  | AbstrLoc(l) -> loc2string l

(** Print a deserialized version of a context
    @param context The context
    @return unit *)
let print_context context : unit =
  let print_context_line (key : name_var) (value : abstrD) =
    print_endline ("\t" ^ key ^ " " ^ abstrD2string value)
  in
  match context with
  | AbstrContextBot -> print_endline "\t⊥"
  | AbstrContext(map) -> Context.iter print_context_line map

(** Print a deserialized version of a memory stack
    @param stack The memory stack
    @return unit *)
let rec print_memoryStack stack : unit =
  match stack with
  | [] -> print_endline "∅"
  | h::t -> print_string (abstrD2string h ^ "::"); print_memoryStack t

(** Print a deserialized version of an environment
    @param env The environment
    @return unit *)
let rec print_environment env : unit =
  let print_environment_line (key : loc) (value : abstrContext) =
    print_string (loc2string key ^ ": \n");
    print_context value
  in
  match env with
  | AbstrEnvironmentBot -> print_endline "EnvBot"
  | AbstrEnvironment(map) -> Environment.iter print_environment_line map
  | GoForward(goal, context, environment) -> print_endline ("\tI'm going to " ^ loc2string goal ^ " with context :"); print_context context; print_endline ", environment:"; print_environment environment

(** Print a deserialized version of a call stack
    @param stack The reference to a call stack
    @return unit *)
let print_callStack stack : unit =
  let print_callStackElement (cCall, returnCode, returnContext, memoryStack : bool * abstrD * abstrContext * memoryStack) : unit =
    print_endline ("cCall = " ^ string_of_bool cCall ^ ", returnCode = " ^ abstrD2string returnCode ^ ", returnContext = ");
    print_context returnContext;
    print_endline "memoryStack = ";
    print_memoryStack memoryStack
  in
  Stack.iter print_callStackElement !stack