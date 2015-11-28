(** OCaml bindings to the PicoSAT SAT solver.
   
    Compatible with API version 953. *)

type picosat = unit Ctypes.ptr
val picosat : picosat Ctypes.typ

(** Constructor *)
val init : unit -> picosat

(** Add a literal of the next clause.  A zero terminates the clause.  The
    solver is incremental.  Adding a new literal will reset the previous
    assignment.   The return value is the original clause index to which
    this literal respectively the trailing zero belong starting at 0. *)
val add : picosat -> int -> int

(** Call the main SAT routine.  A negative decision limit sets no limit on
    the number of decisions. *)
val sat : picosat -> int -> int

(** After [sat] was called and returned [Satisfiable], then
    the satisfying assignment can be obtained by 'dereferencing' literals.
    The return value is 0 for an unknown value. *)
val deref : picosat -> int -> int
