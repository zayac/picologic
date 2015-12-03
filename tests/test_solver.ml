open OUnit2
open Core.Std
open Logic
open Cnf
open Sat

let call_logic_solver s =
  match Logic.from_string s with
  | None -> raise (Failure "wrong expression")
  | Some e ->
    let module PicoSolver = Solver(Picosat) in
    let dimacs, map = Logic.to_dimacs e in
    let solution = PicoSolver.solve_all (String.Set.length (Logic.vars e)) dimacs in
    match solution with
    | None -> raise (Failure "failed to find a solution")
    | Some sol -> sol

let call_cnf_solver s =
  match Cnf.from_string s with
  | None -> raise (Failure "wrong expression")
  | Some e ->
    let module PicoSolver = Solver(Picosat) in
    let dimacs, map = Cnf.to_dimacs e in
    let solution = PicoSolver.solve_all (String.Set.length (Cnf.vars e)) dimacs in
    match solution with
    | None -> raise (Failure "failed to find a solution")
    | Some sol -> sol

let test_sat_logic _ =
  assert_equal (call_logic_solver "(x + ~x * y)") [[-1; 1]; [1; -1]; [1; 1]]

let test_sat_cnf _ =
  assert_equal (call_cnf_solver "(x + ~x * y)") [[-1; 1]; [1; -1]; [1; 1]]

let suite = "Test Solver" >::: [
  "Test SAT with Logic module"    >:: test_sat_logic;
  "Test SAT with Cnf module"    >:: test_sat_cnf;
]

let _ =
  run_test_tt_main suite
