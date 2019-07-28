(** Module which contains the options given as parameter to the program *)

(** Do we have to print backtrace when an exception is encountered *)
val verbose: bool ref

(** Do we have to hide locations in the printed version of the program *)
val withoutLocs: bool ref

(** Do we have to remove the widening and to compute the lfp by enlarging abstract domains *)
val withoutWidening1: bool ref

(** Do we have to remove the widening and to compute the lfp by excluding cases and then join all the cases *)
val withoutWidening2: bool ref