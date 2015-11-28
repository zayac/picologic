open Core.Std

module T = struct
  type ident = string
  type t =
    | False
    | True
    | Not of t
    | Or of t * t
    | And of t * t
    | Var of string
  with sexp, compare
end
include T
include Comparable.Make(T)

let ( + ) t t' = Or (t, t')
let ( * ) t t' = And (t, t')
let ( ~- ) t = Not t
let ( ==> ) t t' = Or (Not t, t')
let ( <== ) t t' = Or (t, Not t')
let ( <=> ) t t' = Or (And (t, t'), And (Not t, Not t'))
let sum = function
  | [] -> False
  | hd :: [] -> hd
  | hd :: tl -> List.fold ~f:(fun acc x -> Or (x, acc)) ~init:hd tl
let product = function
  | [] -> True
  | hd :: [] -> hd
  | hd :: tl -> List.fold ~f:(fun acc x -> And (x, acc)) ~init:hd tl

let from_string s =
  let open Genlex in
  let lexer = make_lexer ["0"; "1"; "("; ")"; "+"; "~"; "*"; "->"; "<-";
                          "<->"] in
  let rec parse_expr = parser
    | [< n1 = parse_atom; n2 = parse_remainder n1 >] -> n2
  and parse_atom = parser
    | [< 'String s >] -> Var s
    | [< 'Kwd "0" >] -> False
    | [< 'Kwd "1" >] -> True
    | [< 'Kwd "~"; n = parse_expr >] -> ~-n
    | [< 'Kwd "("; n = parse_expr; 'Kwd ")" >] -> n
  and parse_remainder n1 = parser
    | [< 'Kwd  "*"; n2 = parse_expr >] -> n1 * n2
    | [< 'Kwd  "+"; n2 = parse_expr >] -> n1 + n2
    | [< 'Kwd  "->"; n2 = parse_expr >] -> n1 ==> n2
    | [< 'Kwd  "<-"; n2 = parse_expr >] -> n1 <== n2
    | [< 'Kwd  "<->"; n2 = parse_expr >] -> n1 <=> n2
    | [< >] -> n1 in
  parse_expr (lexer (Stream.of_string s))

let rec to_string_helper ?(sand="*") ?(sor="+") ?(snot="~") = function
  | False -> "0"
  | True -> "1"
  | Not t -> Printf.sprintf "%s%s" snot (to_string_helper t)
  | And (t, t') ->
    Printf.sprintf "(%s %s %s)" (to_string_helper t) sand (to_string_helper t')
  | Or (t, t') ->
    Printf.sprintf "(%s %s %s)" (to_string_helper t) sor (to_string_helper t')
  | Var s -> s

let to_string s = to_string_helper s

let to_pretty_string = to_string_helper ~sand:"∧" ~sor:"∨" ~snot:"¬"

let rec is_ground = function
  | False | True -> true
  | Not t -> is_ground t
  | Or (t, t')
  | And (t, t') -> is_ground t && is_ground t'
  | Var _ -> false

let rec evaluate bools = function
  | True -> Some true
  | False -> Some false
  | Var t -> String.Map.find bools t
  | Not t ->
    begin
      match evaluate bools t with
      | Some false -> Some true
      | Some true -> Some false
      | None -> None
    end
  | Or (t, t') ->
    begin
      match evaluate bools t, evaluate bools t' with
      | (None, _) | (_, None) -> None
      | (Some true, _) | (_, Some true) -> Some true
      | (Some false, Some false) -> Some false
    end
  | And (t, t') ->
    begin
      match evaluate bools t, evaluate bools t' with
      | (None, _) | (_, None) -> None
      | (Some true, Some true) -> Some true
      | (Some false, _) | (_, Some false) -> Some false
    end

let rec simplify = function
  | Not t ->
    begin
      match simplify t with
      | True -> False
      | False -> True
      | Not x -> x
      | x -> Not x
    end
  | Or (t, t') ->
    begin
      match simplify t, simplify t' with
      | True, _ -> True
      | _, True -> True
      | False, t
      | t, False -> t
      | x, x' when x = x' -> x
      | x, x' -> Or (x, x')
    end
  | And (t, t') ->
    begin
      match simplify t, simplify t' with
      | False, _ -> False
      | _, False -> False
      | True, t
      | t, True -> t
      | x, x' when x = x' -> x
      | x, x' -> And (x, x')
    end
  | t -> t

let rec vars = function
  | Var s -> String.Set.singleton s
  | True | False -> String.Set.empty
  | Not t -> vars t
  | And (t, t') | Or (t, t') ->
    String.Set.union (vars t) (vars t')

let rec nnf = function
  | Not (Not t) -> nnf t
  | And (t, t') -> And (nnf t, nnf t')
  | Not (And (t, t')) -> nnf (Or (Not t, Not t'))
  | Or (t, t') -> Or (nnf t, nnf t')
  | Not (Or (t, t')) -> nnf (And (Not t, Not t'))
  | t -> t

let rec cnf t =
  let rec dis t t' =
    match t, t' with
    | And (a, b), c -> And ((dis a c), (dis b c))
    | a, And (b, c) -> And ((dis a b), (dis a c))
    | a, b -> Or (a, b) in
  let t = simplify t in
  match t with
  | And (t, t') -> And (cnf t, cnf t')
  | Or (t, t') -> dis (cnf t) (cnf t')
  | t -> t

(** [disj_to_dimacs c m t] is a helper function that represents a disjunction
    [t] in the DIMACS format.  [c] is the next integer that is available for
    variable enumeration and [m] is a hash table that stores correspondence
    between variable names and integers that represent the variables in the
    DIMACS format. *)
let rec disj_to_dimacs counter map t =
  let add_variable s mult =
    match String.Map.find map s with
    | None ->
      let new_map = String.Map.add map ~key:s ~data:counter in
      Int.(counter + 1), new_map, [Int.(mult * counter)]
    | Some value ->
      counter, map, [Int.(mult * value)] in
  match t with
  | Var s -> add_variable s 1
  | Not (Var s) -> add_variable s Int.(~-1)
  | Or (t, t') ->
    let new_counter, new_map, l = disj_to_dimacs counter map t in
    let new_counter, new_map, l' = disj_to_dimacs new_counter new_map t' in
    new_counter, new_map, l @ l'
  | _ -> raise (Failure "CNF transformation went wrong")

let to_dimacs t =
  let t = cnf (simplify t) in
  let rec conj_to_dimacs counter map = function
    | True -> counter, map, []
    | False -> counter, map, [[]]
    | Var _ | Not (Var _) | Or _ as t ->
      let c, m, l = disj_to_dimacs counter map t in
      c, m, [l]
    | And (t, t') ->
      let new_counter, new_map, l = conj_to_dimacs counter map t in
      let new_counter, new_map, l' = conj_to_dimacs new_counter new_map t' in
      new_counter, new_map, l @ l'
  | _ -> raise (Failure "CNF transformation went wrong") in
  let _, map, lst = conj_to_dimacs 1 String.Map.empty t in
  let alist = String.Map.to_alist map in
  let alist = List.map alist ~f:(fun (x, y) -> (y, x)) in
  lst, Int.Map.of_alist_exn alist (* exception cannot be raised *)
