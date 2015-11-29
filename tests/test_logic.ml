
open OUnit2

(* Test that logical operators work *)
let test_ops _ =
  let open Logic in
  assert_equal
    ((Var "x") + ~-(Var "x") * (Var "y") + (True * False))
    (Or (Or (Var "x", And (Not (Var "x"), Var "y")), And (True, False)));
  assert_equal (Var "x" ==> (Var "y")) (Or (Not (Var "x"), Var "y"));
  assert_equal
    (Var "x" <=> (Var "y"))
    (Or (And (Var "x", Var "y"), And (Not (Var "x"), Not (Var "y"))));
  assert_equal
    (sum [Var "x"; True; Var "y"; False; Var "z"])
    (Or (Var "z", Or (False, Or (Var "y", Or (True, Var "x")))));
  assert_equal
    (product [Var "x"; True; Var "y"; False; Var "z"])
    (And (Var "z", And (False, And (Var "y", And (True, Var "x")))));
  assert_equal (sum []) False;
  assert_equal (product []) True

(* Test that string conversions work *)
let test_string_conv _ =
  let open Core.Std in
  let open Logic in
  let e = from_string "x + ~x * y + (1 * 0)" in
  assert_equal e
    (Some (Or (Var "x", Not (And (Var "x", Or (Var "y", And (True, False)))))));

  assert_equal
    (to_string (Or (Var "x", Not (And (Var "x", Or (Var "y", And (True, False)))))))
    "(x + ~(x * (y + (1 * 0))))";

  assert_equal
    (to_pretty_string (Or (Var "x", Not (And (Var "x", Or (Var "y", And (True, False)))))))
    "(x ∨ ¬(x ∧ (y ∨ (1 ∧ 0))))"

let test_is_ground _ =
  let open Core.Std in
  let open Logic in
  assert_equal (is_ground False) true;
  assert_equal (is_ground True) true;
  assert_equal (is_ground (Option.value_exn (from_string "x + 0"))) false;
  assert_equal (is_ground (Option.value_exn (from_string "x * 1"))) false;
  assert_equal (is_ground (Option.value_exn (from_string "~1"))) true

let suite = "Test Logic" >::: [
    "Test logical operators"     >:: test_ops;
    "Test string conversions"    >:: test_string_conv;
    "Test 'is_ground' function"  >:: test_is_ground;
  ]

let _ =
  run_test_tt_main suite
