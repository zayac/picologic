(** Logical expressions in an arbitrary form *)

open Core.Std

(** AST representation *)
type t =
  | False
  | True
  | Not of t
  | Or of t * t
  | And of t * t
  | Var of string

val compare_t : t -> t -> int
val t_of_sexp : Sexplib.Sexp.t -> t
val sexp_of_t : t -> Sexplib.Sexp.t

include Comparable.S with type t := t

(** {2 Logical operators } *)

(** Disjunction *)
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

(** Disjunction *)
val sum : t list -> t

(** Conjunction *)
val product: t list -> t


(** {2 Basic functions } *)

(** Converts a string to an expression.  Returns [None] if the string is
    ill-formed.  The function is right-associative. *)
val from_string : string -> t option

(** Converts an expression to a string *)
val to_string : t -> string

(** Converts an expression to a string with logic unicode characters.  *)
val to_pretty_string : t -> string

(** [is_ground t] returns [true] if the expression [t] does not contain
    variables and [false] otherwise *)
val is_ground : t -> bool

(** [evaluate m t] substitutes variables in [t] with values from [m] and
    evaluates the expression.  If there exist variables in [t] that are not
    present in [m], [None] is returned. *)
val evaluate : bool String.Map.t -> t -> bool option

(** Reduces and simplifies an expression *)
val simplify : t -> t

(** Returns variables present in an expression *)
val vars : t -> String.Set.t

(** Transforms an expression to the negation normal form (NNF).  May result
    in exponential growth.  *)
val nnf : t -> t

(** Transforms an expression to the conjuctive normal form (CNF).  May result
    in exponential growth.  *)
val cnf : t -> t

(** Transforms an expression to a simplified version of the DIMACS format,
    which is typically expected by SAT solvers as the input format.  Thu
    function returns a list representing the transformed expression and
    a mapping from integers in the list to variable names in the original
    expression. *)
val to_dimacs : t -> int list list * string Int.Map.t
