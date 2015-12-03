(** Logical expressions in conjunctive normal form.  This module might be more
    efficient than the [Logic] module if SAT problem has to be solved
    frequently. *)

open Core.Std

(** Representation of a logical expression in CNF *)
type t

(** {2 Logical operators } *)

(** Disjunction.  May result in exponential growth. *)
val ( + ) : t -> t -> t

(** Conjunction *)
val ( * ) : t -> t -> t

(** Negation *)
val ( ~- ) : t -> t

(** Implication *)
val ( ==> ) : t -> t -> t

(** Converse implication *)
val ( <== ) : t -> t -> t

(** Equivalence *)
val ( <=> ) : t -> t -> t

(** Exclusive disjunction *)
val xor : t -> t -> t

(** Disjunction.  May result in exponential growth. *)
val sum : t list -> t

(** Conjunction *)
val product : t list -> t


(** {2 Basic functions } *)

(** An expression that evaluates to [false] *)
val mk_false : t

(** An expression that evaluates to [true] *)
val mk_true : t

(** Constructs a logical expression consisting of one variable *)
val mk_var : string -> t

(** Converts a string to an expression.  Returns [None] if the string is
    ill-formed.  The function is right-associative.  *)
val from_string : string -> t option

(** Converts an expression to a string *)
val to_string : t -> string

(** Converts an expression to a string with logic characters in unicode *)
val to_pretty_string : t -> string

(** Returns variables present in an expression *)
val vars : t -> String.Set.t

(** [is_ground t] returns [true] if the expression [t] does not contain
    variables and [false] otherwise *)
val is_ground : t -> bool

(** Converts an expression from [Logic.t] to CNF *)
val from_logic : Logic.t -> t

(** Converts an expression from CNF to [Logic.t] *)
val to_logic : t -> Logic.t

(** [evaluate m t] substitutes variables in [t] with values from [m] and
    evaluates the expression.  If there exist variables in [t] that are not
    present in [m], [None] is returned. *)
val evaluate : bool String.Map.t -> t -> bool option

(** [simplify t] simplifies [t] by removing meaningless tautologies and
    contradictions from the expression. *)
(*val simplify : t -> t*)

(** Transforms an expression to a simplified version of the DIMACS format,
    which is typically expected by SAT solvers as the input format.  Thu
    function returns a list representing the transformed expression and
    a mapping from integers in the list to variable names in the original
    expression. *)
val to_dimacs : t -> int list list * string Int.Map.t

