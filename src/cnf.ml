
open Core.Std

module CSet = Set.Poly

module Atom = struct
  module T = struct
    type t =
    | AVar of string
    | ANegVar of string
    with sexp, compare
  end
  include T
  include Comparable.Make(T)
end

module T = struct
  type t = Atom.Set.t CSet.t
  with sexp, compare
end
include T
include Comparable.Make(T)

let ( + ) t t' =
  CSet.fold t ~init:CSet.empty
    ~f:(fun acc set ->
      CSet.fold t' ~init:acc
        ~f:(fun acc set' ->
          CSet.add acc (Atom.Set.union set set')
      )
  )

let ( * ) = CSet.union

let ( ~- ) t =
  let open Atom in
  let neg = function
    | AVar s -> ANegVar s
    | ANegVar s -> AVar s in
  let lst = ref [] in
  CSet.iter t
    ~f:(fun disj ->
      let tmp = ref !lst in
      lst := Atom.Set.fold disj ~init:[]
        ~f:(fun acc el ->
          if List.is_empty !tmp then [neg el] :: acc
          else (List.map !tmp ~f:(fun l -> (neg el) :: l)) @ acc
        );
    );
  CSet.of_list (List.map ~f:Atom.Set.of_list !lst) 

let (==>) t t' = ~-t + t'

let (<==) t t' = t + ~-t'

let (<=>) t t' = (t * t') + (~-t * ~-t')

let xor t t' = (t * ~-t') + (~-t * t')

let mk_false = CSet.singleton Atom.Set.empty
let mk_true = CSet.empty
let mk_var s = CSet.singleton Atom.(Set.singleton (AVar s))

let rec sum = function
  | [] -> mk_false
  | hd :: [] -> hd
  | hd :: tl -> hd + (sum tl)

let rec product = function
  | [] -> mk_true
  | hd :: [] -> hd
  | hd :: tl -> hd * (product tl)

let rec to_string_helper ?(sand="*") ?(sor="+") ?(snot="~") t =
  let print t =
    let open Atom in
    match t with
    | ANegVar s -> Printf.sprintf "%s%s" snot s
    | AVar s -> s in
  let t = CSet.to_list t in
  let t =
    List.map t ~f:(fun s -> List.map (Atom.Set.to_list s) ~f:print) in
  let sep = Printf.sprintf " %s " sor in
  let s =
    List.map t ~f:(fun l -> "(" ^ (String.concat ~sep l) ^ ")") in
  let sep = Printf.sprintf " %s " sand in
  String.concat ~sep s

let to_string t = to_string_helper t

let to_pretty_string = to_string_helper ~sand:"∧" ~sor:"∨" ~snot:"¬"

let vars t =
  let open Atom in
  let add acc = function
  | AVar s | ANegVar s -> String.Set.add acc s in
  CSet.fold t ~init:String.Set.empty
    ~f:(fun acc el -> Atom.Set.fold ~init:acc ~f:add el)

let is_ground t = String.Set.is_empty (vars t)

let from_logic t =
  let open Logic in
  let t, map = to_dimacs t in
  let list_to_set =
    List.fold ~init:Atom.Set.empty
      ~f:(fun acc el ->
        let open Atom in
        match Int.Map.find map Int.(if el > 0 then el else ~-el) with
        | Some v ->
          if Int.(el > 0) then Atom.Set.add acc (AVar v)
          else Atom.Set.add acc (ANegVar v)
        | None ->
          raise (Failure "'to_dimacs' function from Logic module went wrong")
      ) in
  List.fold ~init:CSet.empty ~f:(fun acc el -> CSet.add acc (list_to_set el)) t
  
let to_logic =
  let open Atom in
  let open Logic in
  let atom_to_var = function
    | AVar s -> Var s
    | ANegVar s -> (Not (Var s)) in
  let set_to_logic =
    Atom.Set.fold ~init:False
      ~f:(fun acc el ->
        let var = atom_to_var el in
        if acc = False then var else acc + var) in
  CSet.fold ~init:True
    ~f:(fun acc el ->
      let res = set_to_logic el in
      if acc = True then res else acc + res
    )

let from_string t =
  match Logic.from_string t with
  | None -> None
  | Some logic -> Some (from_logic logic)

let evaluate map =
  let open Atom in
  let substitute map = function
    | AVar s -> String.Map.find map s
    | ANegVar s -> Option.map ~f:not (String.Map.find map s) in
  let select f acc el =
    match acc, f map el with
    | (None, _) | (_, None) -> None
    | Some x, Some y -> Some (x || y) in
  let eval_set _ =
    Atom.Set.fold ~init:(Some false) ~f:(select substitute) in
  CSet.fold ~init:(Some true) ~f:(select eval_set)

(** Gets a name of a variable from an expression of type [Atom] *)
let get_string = function
  | Atom.AVar s | Atom.ANegVar s -> s

let neg_atom = function
  | Atom.AVar s -> Atom.ANegVar s
  | Atom.ANegVar s -> Atom.AVar s

let simplify =
  let simplify_disj =
      Atom.Set.fold ~init:Atom.Set.empty
        ~f:(fun set el ->
          let neg_el = neg_atom el in
          if Atom.Set.mem set neg_el then Atom.Set.remove set neg_el
          else Atom.Set.add set el
        ) in
  let neg_disj = Atom.Set.map ~f:neg_atom in
  CSet.fold ~init:CSet.empty
    ~f:(fun set el ->
      let el = simplify_disj el in
      let neg_el = neg_disj el in
      if CSet.mem set neg_el then CSet.remove set neg_el
      else CSet.add set el
    )

let to_dimacs t = Logic.to_dimacs (to_logic t)
