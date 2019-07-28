open Types

(** Static interpretation methods which are not dependant to the abstraction domain chosen. *)

(** The last id used in an environment *)
val var_id : int ref

(** Test equality of two environments
    - [e1] The first environment
    - [e2] The second environment
      @return Are [e1] and [e2] equal *)
val areEnvironmentEqual :
  abstrEnvironment ref -> abstrEnvironment ref -> bool

(** Apply widening on two environments by applying the widening to all the abstract domains of their ids. Delete the variables which exist only in one environment.
    - [e1] The first environment
    - [e2] The second environment
      @return The environment made of the application of the widening on [e1] and [e2] *)
val wideningOnEnvironments :
  abstrEnvironment ref ->
  abstrEnvironment ref -> abstrEnvironment ref

(** Joins two environment by joining all the ids. Delete the variables which exist only in one environment.
    - [e1] The first environment
    - [e2] The second environment
      @return The environment made of the join of [e1] and [e2] *)
val environmentJoin :
  abstrEnvironment ref ->
  abstrEnvironment ref -> abstrEnvironment ref

(** Merge two contexts by returning the variables which are both represented by the same ids.
    - [c1] The first context
    - [c2] The second context
      @return The resulting context *)
val contextMerge :
  abstrContext -> abstrContext -> abstrContext

(** Remove from the environment the variables which have been added in the [newContext].
    - [lastContext] The original context
    - [newContext] The new context with perhaps some variables which have been added or modified and which should be forgotten in the [env]
    - [env] The environment
      @return unit *)
val forgetNewVariables :
  lastContext:abstrContext ->
  newContext:abstrContext -> env:abstrEnvironment ref -> unit

(** Meet two environment by meeting all the ids. Delete the variables which exist only in one environment.
    - [e1] The first environment
    - [e2] The second environment
      @return The environment made of the meet of [e1] and [e2] *)
val environmentMeet :
  abstrEnvironment ref ->
  abstrEnvironment ref -> abstrEnvironment ref

(** Return a variable at a location in an environment
    - [context] The context in which [vn] is
    - [vn] The name of the variable
    - [env] The environment in which the variable will be searched
      @raise UNDEFINEDVAR if the variable is not declared at the location specified
      @return The abstract domain of the variable at the location specified *)
val getVar :
  context:abstrContext ->
  vn:name_var -> env:abstrEnvironment ref -> abstrD

(** Calculates the least fixpoint of [env] by the function [e1]
    - [env] The initial environment
    - [e1] The function to apply on the environment until we have a fixpoint (or an over-approximation of it if we use a widening function)
    - [context] The initial context
    - [lastStack] The initial memory stack
      @return The memory stack when the least fixpoint (or the over-approximation of it) has been found *)
val lfp :
  env:abstrEnvironment ref ->
  e1:(abstrContext * memoryStack -> abstrContext * memoryStack) ->
  (abstrContext * memoryStack) -> memoryStack

(** Adds an undefined variable to a context and returns the new context
    - [vn] The name of the variable
    - [value] The value of the variable that we want to set
    - [env] The environment
    - [context] The context
      @raise ALREADYDEFINED if the variable already exists in [context]
      @return The new context in which the value of the variable has been added *)
val addVarToContext :
  vn:name_var ->
  value:abstrD ->
  env:abstrEnvironment ref -> abstrContext -> abstrContext

(** Update an existing variable in a context and returns the new context
    - [vn] The name of the variable
    - [value] The value of the variable that we want to set
    - [context] The context
    - [env] The environment
      @raise UNDEFINEDVAR if the variable doesn't exist in [context]
      @return unit *)
val updateVarInContext :
  vn:name_var ->
  value:abstrD -> context:abstrContext -> env:abstrEnvironment ref -> unit

(** Get the abstract interpretation of an ASM expression
    - [ae] The ASM expression
    - [context] The context
    - [env] The environment
      @return The abstract interpretation of the [ae] in the provided [context] and [env] *)
val getAsmValue :
  ae:asm_expr ->
  context:abstrContext -> env:abstrEnvironment ref -> abstrD

(** Get the C function in which a location is
    - [l] The location
    - [f] The reference to the set of the C functions which are known
      @return The function as a global statement and the context associated to it *)
val getLastFunction :
  l:loc ->
  f:functionContext ref ->
  global_statement * abstrContext

(** Join the abstract values of two memory stacks
    - [s1] The first memory stack
    - [s2] The second memory stack
      @raise WRONGSTACKOFFSET If the stacks don't have the same length
      @return The memory stack in which each element is the join of the initials memory stacks *)
val joinMemoryStack :
  s1:memoryStack -> s2:memoryStack -> memoryStack

(** Initialize the call stack in order to prepare it before the call of a function
    - [cCall] Is it a call to a C function (respectively ASM function)
    - [callStack] A reference to the call stack
      @return unit *)
val initFunction :
  cCall:bool ->
  callStack:callStack ref -> unit

(** Remove the registers from a context and in the associated environment after an ASM block
    - [context] The context
      @return The new context without the registers' values *)
val postlude_asm :
  context:abstrContext -> env:abstrEnvironment ref -> abstrContext

(** Raise a return (called by return/ret statement), join the return code, the return context, the return environment and the memory stack with the last ones saved
    - [cCall] Is the statement which calls it a return (respectively ret)
    - [addReturnPossibility] Do we have to wait for another return/ret
    - [returnCode] The code that has been returned
    - [returnContext] The context that has been returned
    - [returnEnv] The environment that has been returned
    - [callStack] A reference to the actual call stack
    - [stack] The memory stack to return
      @raise Failure If you try to return a C function in ASM and vice versa
      @return unit *)
val raiseReturn :
  cCall:bool ->
  addReturnPossibility:bool ->
  returnCode:abstrD ->
  returnContext:abstrContext ->
  returnEnv:abstrEnvironment ref ->
  callStack:callStack ref -> stack:memoryStack -> unit

(** Generic abstract interpretation of binary boolean operators
    - [env1] The possible environment in which the first operand is satisfied
    - [context1IsBot] Is the context associated to [env1] bottom
    - [op] The binary boolean operator
    - [env2] The possible environment in which the second operand is satisfied
    - [context2IsBot] Is the context associated to [env2] bottom
      @return The environment in which the whole boolean operation is satisfied and a boolean which represents if the resulting environment is bottom *)
val abexpBoolBin :
  abstrEnvironment ref * bool ->
  boolOpBoolBin ->
  abstrEnvironment ref * bool -> abstrEnvironment ref * bool

(** Forward abstract semantics of arithmetic expressions
    - [a] An arithmetic expression
    - [context] The context in which the arithmetic expression is performed
    - [env] The environment in which the arithmetic expression is performed, perhaps it will be modified
      @raise UNDEFINEDVAR If a variable is not defined in the provided context
      @raise PTRINTMATCH If an operation is performed between a pointer and an int
      @return The abstract domain of the evaluation of the arithmetic expression *)
val faexp :
  a:arith_expr ->
  context:abstrContext -> env:abstrEnvironment ref -> abstrD

(** Backward abstract semantics of arithmetic expressions
    - [a] An arithmetic expression
    - [context] The context in which the arithmetic expression is performed
    - [p] The domain in which the evaluation of the arithmetic expression has to be
    - [env] The environment in which the arithmetic expression is performed, perhaps it will be modified
      @raise UNDEFINEDVAR If a variable is not defined in the provided context
      @return Does the resulting context have to be bottom *)
val baexp :
  a:arith_expr ->
  context:abstrContext -> p:abstrD -> env:abstrEnvironment ref -> bool

(** Generic abstract interpretation of boolean expression
    - [b] The boolean expression
    - [context] The context
    - [env] The environment, perhaps it will be modified
      @return Does the resulting context have to be bottom *)
val abexp :
  b:bool_expr -> context:abstrContext -> env:abstrEnvironment ref -> bool

(** Add the abstract interpretation of a list of variables by the definition of a C function
    - [args_def] The definition of the arguments of a C function
    - [args] The list of the arithmetic expressions provided
    - [contextToEnhance] The context to update
    - [context] The context in which the arithmetic expression will be evaluated
    - [env] The environment in which the arithmetic expression will be evaluated and which will be enhanced with the new variables
      @raise Failure If [args_def] doesn't match [args]
      @return The new context made from [contextToEnhance] *)
val matchCArgs :
  args_def:arguments ->
  args:arith_expr list ->
  contextToEnhance:abstrContext ->
  context:abstrContext ->
  env:abstrEnvironment ref -> abstrContext

(** Adds the arguments from a memory stack to a context by a C function's definition
    - [context] The context
    - [args_def_reversed] The definition of the arguments that the C function is waiting for
    - [stack] The memory stack
    - [env] The environment
      @raise Failure If the memory stack is too tiny compared to the argument's list
      @return The pair made of the new context and of the new memory stack (without the arguments used) *)
val matchAsmArgs :
  context:abstrContext ->
  args_def:arguments ->
  stack:memoryStack ->
  env:abstrEnvironment ref ->
  abstrContext * memoryStack

(** Add registers to the context 
    - [context] The context before the ASM block
    - [env] The environment which will be modified
      @return The new context with the registers set *)
val prelude_asm :
  context:abstrContext ->
  env:abstrEnvironment ref -> abstrContext

(** Reset the value of all registers except EAX which is replaced by a specified value
    - [context] The context
    - [env] The environment which will be modified
    - [eax_value] The new value of EAX
      @return unit *)
val reset_registers :
  context:abstrContext ->
  env:abstrEnvironment ref -> eax_value:abstrD -> unit

(** Replace the registers of a context by the registers of another one
    - [c1] The context in which the registers will be changed
    - [c2] The context from which the registers will be get
    - [env] The environment which will be modified
      @return The new context made of [c1] with registers of [c2] *)
val setRegisters :
  realContext:abstrContext ->
  contextWithRegs:abstrContext -> env:abstrEnvironment ref -> unit

(** Generic forward non-relational abstract interpretation of ASM statements
    - [stat] The statement which is currently read
    - [shouldGoBackOnJmp] Is the statement read in a branch which has not been read before
    - [f] A reference to the set of the C functions
    - [callStack] The call stack
    - [stack] The memory stack
    - [env] The environment
    - [context] The context
      @return A pair made of the new context after the read of stat and of the new memory stack *)
val aPostAsm :
  loc * stat_a * loc ->
  shouldGoBackOnJmp:bool ->
  f:functionContext ref ->
  callStack:callStack ref ->
  stack:memoryStack ->
  env:abstrEnvironment ref ->
  context:abstrContext -> abstrContext * memoryStack

(** Generic forward non-relational abstract interpretation of ASM blocks
    - [pre_loc] The entry location of [block]
    - [block] The block of ASM statement to read
    - [shouldGoBackOnJmp] Is the statement read in a branch which has not been read before
    - [f] A reference to the set of C functions
    - [callStack] The call stack
    - [stack] The memory stack
    - [env] The environment
    - [context] The context
      @return A pair made of the new context and of the new memory stack *)
val aPostAsmBlock :
  loc * (stat_a * loc) list ->
  shouldGoBackOnJmp:bool ->
  f:functionContext ref ->
  callStack:callStack ref ->
  stack:memoryStack ->
  env:abstrEnvironment ref ->
  abstrContext -> abstrContext * memoryStack

(** Generic forward non-relational abstract interpretation of C blocks
    - [program] The block of C statements
    - [shouldGoBackOnJmp] Is the statement read in a branch which has not been read before
    - [f] A reference to the C functions
    - [callStack] The call stack
    - [lastAsmStack] The stack state of the last ASM statement
    - [env] The environment
    - [context] The context
      @return The pair made of the new context after the execution of the block of C statements and the new memory stack *)
val aPostCBlock :
  loc_stat_c ->
  shouldGoBackOnJmp:bool ->
  f:functionContext ref ->
  callStack:callStack ref ->
  lastAsmStack:memoryStack ->
  env:abstrEnvironment ref ->
  context:abstrContext -> abstrContext * memoryStack

(** Generic forward non-relational abstract interpretation of C statements
    - [stat] The C statement
    - [shouldGoBackOnJmp] Is the statement read in a branch which has not been read before
    - [f] A reference to the C functions
    - [callStack] The call stack
    - [lastAsmStack] The stack state of the last ASM statement
    - [env] The environment
    - [context] The context
      @return The pair made of the new context after the excetion of the C statement and of the new memory stack *)
val aPostC :
  loc * stat_c * loc ->
  shouldGoBackOnJmp:bool ->
  f:functionContext ref ->
  callStack:callStack ref ->
  lastAsmStack:memoryStack ->
  env:abstrEnvironment ref ->
  context:abstrContext -> abstrContext * memoryStack

(** Generic forward non-relational abstract interpretation of global statement
    - [pre_loc] The location before the global statement
    - [stat] The global statement
    - [post_loc] The location after the global statement
    - [f] A reference to the set of C functions
    - [env] The environement
    - [context] The context
      @return The new context after the execution of the global statement *)
val aPostGlobalC :
  pre_loc:loc ->
  stat:global_statement ->
  post_loc:loc ->
  f:functionContext ref ->
  env:abstrEnvironment ref ->
  abstrContext -> abstrContext

(** Generic foward non-relational abstract interpretation of a block of global statements
    - [pre_loc] The location before the first global statement of the block
    - [program] The block of global statements
    - [f] A reference to the set of C functions
    - [env] The environment
    - [context] The context
      @return The new context after the execution of the block of global statements *)
val aPostProgram :
  pre_loc:loc ->
  program:(global_statement * loc) list ->
  f:functionContext ref ->
  env:abstrEnvironment ref ->
  abstrContext -> abstrContext
