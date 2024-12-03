open Core

let split_pairs line =
  let elems =
    line |> String.split ~on:' ' |> List.filter ~f:(Fn.non (String.equal ""))
  in
  match elems with
  | [ lhs; rhs ] -> (Int.of_string lhs, Int.of_string rhs)
  | _ -> failwith "Unexpected input"

let solve_part_1 input =
  let ls, rs = input |> List.map ~f:split_pairs |> List.unzip in
  List.zip_exn
    (List.sort ~compare:Int.compare ls)
    (List.sort ~compare:Int.compare rs)
  |> List.map ~f:(fun (l, r) -> abs (l - r))
  |> List.fold ~init:0 ~f:( + )

let group_by_count xs =
  let counts = Hashtbl.create (module Int) in
  let incr = function Some prev -> prev + 1 | None -> 1 in
  List.iter xs ~f:(fun x -> Hashtbl.update counts x ~f:incr);
  counts

let solve_part_2 input =
  let ls, rs = input |> List.map ~f:split_pairs |> List.unzip in
  let counts = group_by_count rs in
  List.fold ls ~init:0 ~f:(fun acc x ->
      let count = Hashtbl.find counts x |> Option.value ~default:0 in
      acc + (x * count))
