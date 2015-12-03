
open Core.Std

module CSet = Set.Poly

module T = struct
  type t = bool String.Map.t CSet.t
  with sexp, compare
end
include T
include Comparable.Make(T)

let ( + ) t t' =
  CSet.fold t ~init:CSet.empty
    ~f:(fun acc set ->
      CSet.fold t' ~init:acc
        ~f:(fun acc set' ->
          let merged =
            String.Map.merge set set'
              ~f:(fun ~key -> function
                | `Both _ -> None
                | `Left v | `Right v -> Some v
              ) in
          CSet.add acc merged
      )
  )

let ( * ) = CSet.union

let ( ~- ) = CSet.map ~f:(String.Map.map ~f:not)

let (==>) t t' = ~-t + t'

let (<==) t t' = t + ~-t'

let (<=>) t t' = (t * t') + (~-t * ~-t')

let xor t t' = (t * ~-t') + (~-t * t')

let mk_false = CSet.singleton String.Map.empty
let mk_true = CSet.empty
let mk_var s = CSet.singleton (String.Map.singleton s true)

let rec sum = function
  | [] -> mk_false
  | hd :: [] -> hd
  | hd :: tl -> hd + (sum tl)

let rec product = function
  | [] -> mk_true
  | hd :: [] -> hd
  | hd :: tl -> hd * (product tl)

let rec to_string_helper ?(sand="*") ?(sor="+") ?(snot="~") t =
  let print (s, b) = if b then Printf.sprintf "%s%s" snot s else s in
  let t =
    CSet.map t ~f:(fun map -> List.map (String.Map.to_alist map) ~f:print) in
  let sep = Printf.sprintf " %s " sor in
  let s =
    CSet.map t ~f:(fun l -> "(" ^ (String.concat ~sep l) ^ ")") in
  let sep = Printf.sprintf " %s " sand in
  String.concat ~sep (CSet.to_list s)

let to_string t = to_string_helper t

let to_pretty_string = to_string_helper ~sand:"∧" ~sor:"∨" ~snot:"¬"

let vars t =
  CSet.fold t ~init:String.Set.empty
    ~f:(fun acc el ->
      String.Map.fold el ~init:acc
        ~f:(fun ~key ~data acc -> String.Set.add acc key)
    )

let is_ground t = String.Set.is_empty (vars t)

let from_logic t =
  let open Logic in
  let t, map = to_dimacs t in
  let list_to_set =
    List.fold ~init:String.Map.empty
      ~f:(fun acc el ->
        match Int.Map.find map Int.(if el > 0 then el else ~-el) with
        | Some v ->
          String.Map.change acc v
            (function
              | None -> Some Int.(el > 0)
              | Some _ -> None
            )
        | None ->
          raise (Failure "'to_dimacs' function from Logic module went wrong")
      ) in
  List.fold ~init:CSet.empty
    ~f:(fun acc el ->
      let map = list_to_set el in
      if String.Map.is_empty map then acc
      else CSet.add acc map
    ) t
  
let to_logic =
  let open Logic in
  let atom_to_var s = function
    | true -> Var s
    | false -> (Not (Var s)) in
  let map_to_logic =
    String.Map.fold ~init:False
      ~f:(fun ~key ~data acc ->
        let var = atom_to_var key data in
        if acc = False then var else acc + var) in
  CSet.fold ~init:True
    ~f:(fun acc el ->
      let res = map_to_logic el in
      if acc = True then res else acc + res
    )

let from_string t =
  match Logic.from_string t with
  | None -> None
  | Some logic -> Some (from_logic logic)

let evaluate var_map t =
  let select = function
    | (None, _) | (_, None) -> None
    | Some x, Some y -> Some (x || y) in
  let reduce_map ~key ~data acc =
    let value =
      Option.map (String.Map.find var_map key) ~f:(fun v -> Poly.(data = v)) in
    select (acc, value) in
  let eval_map acc map =
    let value = String.Map.fold ~init:(Some false) ~f:reduce_map map in
    select (acc, value) in
  CSet.fold ~init:(Some true) ~f:eval_map t

(*
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
*)

let to_dimacs t = Logic.to_dimacs (to_logic t)
