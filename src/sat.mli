

module type Solvable = sig
  type solver
  val init : unit -> solver
  val add : solver -> int -> int
  val sat : solver -> int -> int
  val deref : solver -> int -> int
end

module Solver :
  functor (S : Solvable) ->
  sig
    val solve : ?limit:int -> int -> int list list -> int list list option
    val solve_all : ?limit:int -> int -> int list list -> int list list option
  end
