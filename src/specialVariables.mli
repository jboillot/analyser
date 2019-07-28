(** Module to store the special variables which are used to store registers in contexts *)

(** Variable name of the register EAX *)
val eax_reg : string

(** Variable name of the register EBX *)
val ebx_reg : string

(** Variable name of the register ECX *)
val ecx_reg : string

(** Variable name of the register EDX *)
val edx_reg : string

(** Is a variable name a register's one
    - [vn] The name of the variable
      @return Is [vn] the name of a register *)
val is_register : string -> bool
