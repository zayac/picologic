(** SAT solver *)

(** An interface for a DIMACS SAT solver.  The interface corresponds to the
    PicoSAT solver, however, other existing solvers should have a similar
    interface. *)
module type Solvable = sig
  (** The solver data structure *)
  type t

  (** Constructor *)
  val init : unit -> t

  (** Adds a literal of the next clause.  A zero terminates the clause. *)
  val add : t -> int -> int

  (** Call the main SAT routine.  An integer argument sets a limit on the number
      of decisions.  A negative decision sets no limit.  [sat] returns 10 if
      the constraints are [SATISFIABLE], 20 if the constraints are
      [UNSATISFIABLE] and other integers if the solution is [UNKNOWN]. *)
  val sat : t -> int -> int

(** After [sat] was called and returned [SATISFIABLE], then the satisfying
    assignment can be obtained by 'dereferencing' literals.  The return value is
    0 for an unknown value. *)
  val deref : t -> int -> int
end

(** A module that finds solutions to a SAT problem implemented as a functor. *)
module Solver :
  functor (S : Solvable) ->
  sig
    (** [solve l v c] returns a solution to the SAT probem [c], where [l] is
        a limit on the number of decisions and [v] is a number of variables.
        The function returns an empty list if the problem is UNSAT and [None] if
        the solution is unknown.  If the solution is found, the outer list
        always contains one element (list). *)
    val solve : ?limit:int -> int -> int list list -> int list list option

    (** [solve_all l v c] finds all solutions to the SAT problem if one exists.
        The interface is the same as for the function [solve]. *)
    val solve_all : ?limit:int -> int -> int list list -> int list list option

    (** Given a list of solutions [t] produced by [solve] or [solve_all]
        functions, [values l] returns a list of solutions in a form of a hash
        table.  Note that the variables in the hash are enumerated from 1. *)
    val values : int list list -> bool Core.Std.Int.Map.t list
  end
