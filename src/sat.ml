open Core.Std

module type Solvable = sig
  type t
  val init : unit -> t
  val add : t -> int -> int
  val sat : t -> int -> int
  val deref : t -> int -> int
end

module Solver(S : Solvable) = struct
  let get_solution sat vars =
    let values = ref [] in
    for i = 1 to vars do
      let value = S.deref sat i in
      if Int.(value <> 0) then values := value :: !values
      else raise (Invalid_argument "number of variables is wrong")
    done;
    List.rev !values

  let solve_helper ?(limit=Int.(~-1)) x vars all_solutions =
    let sat = ref (S.init ()) in
    let x = List.map x ~f:(fun l -> l @ [0]) in
    List.iter x ~f:(fun l -> List.iter l ~f:(fun i -> ignore (S.add !sat i)));
    let result = ref [] in
    let unknown = ref false in
    let loop = ref true in
    while !loop do
      match S.sat !sat limit with
      | 10 -> (* SATISFIABLE *)
        let sol = get_solution !sat vars in
        result := sol :: !result;
        if all_solutions then
          let counter = ref 1 in
          List.iter sol
            ~f:(fun i ->
              ignore (S.add !sat Int.(~-1 * !counter * i));
              counter := Int.(!counter + 1)
            );
          ignore (S.add !sat 0)
        else
          loop := false
      | 20 -> (* UNSATISFIABLE *)
        loop := false
      | _ -> (* UNKNOWN *)
        unknown := true;
        loop := false
    done;
    if !unknown then None
    else Some !result

  let solve ?(limit=Int.(~-1)) vars l = solve_helper ~limit l vars false
  let solve_all ?(limit=Int.(~-1)) vars l = solve_helper ~limit l vars true

  let values =
    let open Core.Std in
    let produce_map =
      List.fold ~init:(Int.Map.empty, 1)
        ~f:(fun (map, key) el ->
          Int.(Map.add map ~key ~data:(el > 0), key + 1)
        ) in
    List.map ~f:(fun el -> let r, _ = produce_map el in r)

end
