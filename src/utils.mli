open Types

(** Utilitary function in order to achieve static interpretation *)

(** Return the minimum and the maximum of a list. Primitive recursive function.
    - [l] The list of numbers
      @raise Failure If the list is empty
      @return The pair made of the minimum and the maximum of the elements of the list *)
val minmax : Z.t list -> Z.t * Z.t

(** Apply the not function to a boolean 
    - [b] The initial boolean expression
      @return The boolean expression on which we have apply a not *)
val notBoolExpr : bool_expr -> bool_expr

(** Adds locations to an unlocated program
    - [program] The unlocated program
      @return The program located *)
val addLocationsProgram : global_statement list -> program

(** Remove the labels of the program and replace them by the locations associated
    - [program] The located program
      @return The located program with the labels which have been replaced *)
val removeLabelsProgram :
  loc * (global_statement * loc) list -> program

(** Returns the inverse of the minimum where a et b are already inversed
    - [a] 1/[c]
    - [b] 1/[d]
      @return min(1/([c], [d])) *)
val invMin : Z.t -> Z.t -> Z.t

(** Returns the inverse of the maximum where a et b are already inversed
    - [a] 1/[c]
    - [b] 1/[d]
      @return max(1/([c], [d])) *)
val invMax : Z.t -> Z.t -> Z.t

(** Remove the C variables from a context. Used when we jump in another function and that we have no clue on the variable stack
    - [context] The last context
      @return The new context *)
val removeCVariables : abstrContext -> abstrContext

(** Return the number associated to a location
    - [l] The location
      @return The number associated *)
val getLocationNumber : loc -> int

(** Return the variable name of a register
    - [reg] The register
      @return Its name *)
val getRegNameVar : reg32 -> name_var
