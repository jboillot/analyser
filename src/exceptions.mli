open Types

(** Module which is used to raise or handle non-fatal exceptions *)

(** The possible types of the exceptions *)
type exception_type =
  | DIVIDE_BY_ZERO (** Division by zero *)
  | OVERFLOW (** Overflow *)
  | PTRINTMATCH (** Match of a pointer with something which is not a pointer *)
  | UNDEFINEDVAR (** A variable has not been found in a context *)
  | ALREADYDEFINEDVAR (** The variable already exists in a context *)
  | WRONGSTACKOFFSET (** The stack doesn't match a valid stack length *)
  | NONEMPTYCALLSTACK (** At the end of the program the call stack is not empty *)
  | LOCMATCH (** Match of a location with something which is not a location *)

(** The informations which will be stored about an exception *)
type except

(** The list of the current exceptions *)
val exceptions : except list ref

(** Give the description of each exception type
    - [t] The type of the exception
      @return The description of this exception *)
val exception_type2string : exception_type -> string

(** Handle an exception and print the associated error
    - [exception] The exception
      @return unit *)
val handleException : exception_type * generic_stat_loc -> unit

(** Raise an exception
    - [t] The type of the exception
    - [generic_stat] The located statement in which the exception has been raised
      @return unit *)
val raiseException : exception_type -> generic_stat_loc -> unit

(** Handle every exception in chronological order
    @return unit *)
val handleExceptions : unit -> unit
