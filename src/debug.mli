open Types

(** Module which simplifies print of debug *)

(** Deserialize type
    - [t] C type of variable
      @return The deserialized string of the type providen *)
val type2string : type_c -> string

(** Remove sometimes some parenthesis
    - [a] An arithmetic expression
      @return Are parenthesis important around the [a] *)
val arith_expr_needs_parenthesis : arith_expr -> bool

(** Deserialize arithmetic expressions
    - [a] The arithmetic expression
      @return The deserialized string of the arithmetic expression providen *)
val arith_expr2string : arith_expr -> string

(** Deserialize boolean expressions
    - [b] The boolean expression
      @return The deserialized string of the boolean expression providen *)
val bool_expr2string : bool_expr -> string

(** Deserialize location
    - [l] The location
      @return The deserialized string of the location providen *)
val loc2string : loc -> string

(** Deserialize arguments of a C function (which is a list of arithmetic expressions)
    - [args] The arguments
      @return The deserialized string of the arguments providen *)
val args2string : arith_expr list -> string

(** Deserialize register
    - [reg] The register
      @return The name of the register providen *)
val reg2string : reg32 -> string

(** Deserialize a pointer variable
    - [v] The pointer variable
      @return The deserialized string of the pointer variable providen *)
val ptrVar2string : ptr_var -> string

(** Deserialize ASM expression
    - [ae] The ASM expression
      @return The deserialized string of the ASM expression providen *)
val asm_expr2string : asm_expr -> string

(** Deserialize C statement
    - [stat] The C statement
    - [indent] The initial prefix of the line
      @return The deserialized string of the C statement providen wich begins by the [indent] *)
val stat_c2string : stat_c -> string -> string

(** Deserialize ASM statements
    - [stat] The ASM statement
    - [indent] The initial prefix of the line
      @return The deserialized string of the ASM statement providen which begins by the [indent] *)
val stat_a2string : stat_a -> string -> string

(** Deserialize block of ASM statements
    - [block] The block of ASM statements
    - [indent] The initial indent of each line
      @return The deserialized string of the block of ASM statements providen which begins by the [indent] *)
val stat_a_block2string : (stat_a * loc) list -> string -> string

(** Deserialize block of C statements
    - [stat] The block of C statements
      @return The deserialized string of the block of C statements providen which begins by the [indent] *)
val block_c2string : loc_stat_c -> string -> string

(** Deserialize definition of arguments of a C function
    - [args] The definition of the arguments
      @return The deserialized string of the arguments providen *)
val args_def2string : arguments -> string

(** Deserialize global statements
    - [stat] The global statement
      @return The deserialized string of the global statement *)
val global_statement2string : global_statement -> string

(** Deserialize located programs
    - [l] The entry point of [program]
    - [program] The C program with inline assembly
      @return The deserialized string of the program *)
val program2string : program -> string

(** Deserialize an abstract domain
    - [value] The abstract domain
      @return The deserialized string of the abstract domain *)
val abstrD2string : abstrD -> string

(** Print a deserialized version of a context
    - [context] The context
    - [env] The environment
      @return unit *)
val print_context :
  abstrContext -> abstrEnvironment ref -> unit

(** Print a deserialized version of a memory stack
    - [stack] The memory stack
      @return unit *)
val print_memoryStack : memoryStack -> unit

(** Print a deserialized version of an environment
    - [env] The environment
      @return unit *)
val print_environment : abstrEnvironment ref -> unit

(** Print a deserialized version of a call stack
    - [stack] The reference to a call stack
    - [env] The environment
      @return unit *)
val print_callStack : callStack ref -> unit
