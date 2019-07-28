open Types

(** Methods which depend of the abstract domain. Foundations of the static interpretation. *)

(** Last statement read. Is used to give an explicit error with the statement concerned. *)
val lastStat : generic_stat_loc ref

(** Abstraction of a real element
    - [real_elem] The real element
      @return The abstract element which corresponds to the real one *)
val alpha : realD -> abstrD

(** Joins two abstract domains
    - [a1] The first abstract domain
    - [a2] The second abstract domain
      @return The join of the two abstract domains *)
val join : abstrD -> abstrD -> abstrD

(** Meets two abstract domains
    - [a1] The first abstract domain
    - [a2] The second abstract domain
      @return The meet of the two abstract domains *)
val meet : abstrD -> abstrD -> abstrD

(** Return true iff [n] is the abstract domain [p]. The concretization function appears here.
    - [n] A number (not really a real domain which is a set of numbers)
    - [p] The abstract domain
      @return Is the number in the abstract domain *)
val isInAbstractDomain : Z.t -> abstrD -> bool

(** Forward abstract semantics of arithmetic unary operators
    - [op] The arithmetic unary operator
    - [a] The abstract domain of the operand
      @return The abstract domain after the application of the operator on the abstract domain provided *)
val faexpUna : arithOpArithUna -> abstrD -> abstrD

(** Forward abstract semantics of arithmetic binary operators
    - [a1] The abstract domain of the first operand
    - [op] The arithmetic binary operator
    - [a2] The abstract domain of the second operand
      @return The abstract domain after the application of the operator on the two operands *)
val faexpBin :
  abstrD -> arithOpArithBin -> abstrD -> abstrD

(** Specific abstract interpretation of binary boolean operators with arithmetics operands
    - [a1] The abstract domain of the first operand
    - [op] The arithmetic to boolean binary operator
    - [a2] The abstract domain of the second operand
      @return The pair made of the abstract domain of both operand which respect the property, which is this boolean expression, or an over-approximation of that *)
val abexpArithBin :
  abstrD ->
  boolOpArithBin -> abstrD -> abstrD * abstrD

(** Backward abstract semantics of arithmetic unary operators
    - [op] The arithmetic unary operator
    - [a] The abstract domain of the operand
    - [p] The abstract domain in which we want the result of the operation
      @return The abstract domain of the operand which satisfied the fact that [op] [a] is in [p] or an over-approximation of it *)
val baexpUna :
  arithOpArithUna -> abstrD -> abstrD -> abstrD

(** Backward abstract semantics of arithmetic binary operators
    - [a1] The abstract domain of the first operand
    - [op] The arithmetic to boolean binary operator
    - [a2] The abstract domain of the second operand
    - [p] The abstract domain in which we want the result of the operation
      @return The pair made of the abstract domain for both operands which satistfied the fact that [a1] [op] [a2] is in [p] or an over-approximation of it *)
val baexpBin :
  abstrD ->
  arithOpArithBin ->
  abstrD -> abstrD -> abstrD * abstrD

(** The widening function which ensure termination of lfp
    - [a1] The first abstract domain
    - [a2] The second abstract domain
      @return The abstract domain which is the application of the widening on the abstract domains provided *)
val widening : abstrD -> abstrD -> abstrD
