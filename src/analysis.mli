open Types

(** Analyse a program statically with abstract interpretation *)

(** Analyse a program and write results of this analysis or exceptions encountered
    - [l] The initial location of [program]
    - [program] The program already located to analyse
      @return unit *)
val lastStat : generic_stat_loc

(** Analyse a program and write results of this analysis or exceptions encountered
    - [l] The initial location of [program]
    - [program] The program already located to analyse
      @return unit *)
val analyseProgram :
  loc * (global_statement * loc) list -> unit
