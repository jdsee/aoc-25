open Core

let red = "\x1b[31m"
let green = "\x1b[32m"
let blue = "\x1b[34m"
let yellow = "\x1b[33m"
let magenta = "\x1b[35m"
let cyan = "\x1b[36m"
let grey = "\x1b[90m"
let reset = "\x1b[0m"

let parse_patterns s =
  String.split ~on:',' s
  |> List.map ~f:String.strip
  |> Set.of_list (module String)

let count_feasible_combos ~patterns design =
  let total_len = String.length design in
  let max_pattern_len =
    Set.fold patterns ~init:0 ~f:(fun acc p -> max acc (String.length p))
  in
  Fmt.pr "@.@.%s%s %slen %i%s" blue design grey total_len reset;
  Fmt.pr "@.%s%a %smax-len %i%s" cyan (Fmt.Dump.list String.pp)
    (Set.to_list patterns) grey max_pattern_len reset;
  let can_construct recur design =
    if String.is_empty design then 1
    else
      String.fold_until design ~init:(0, "")
        ~f:(fun (acc, prev) c ->
          let chunk = prev ^ Char.to_string c in
          let chunk_len = String.length chunk in
          if chunk_len > max_pattern_len then Stop acc
          else if Set.mem patterns chunk then (
            let rem = String.drop_prefix design chunk_len in
            let pad = String.make (total_len - String.length design) '.' in
            Fmt.pr "@.%s%s%s%s%s%s" grey pad yellow chunk reset rem;
            let res = recur rem in
            Continue (acc + res, chunk))
          else Continue (acc, chunk))
        ~finish:(fun (acc, _) -> acc)
  in
  let can_construct_memo =
    Memo.recursive can_construct ~hashable:String.hashable
  in
  let feasible_combos = can_construct_memo design in
  Fmt.pr "@.%s🗹 found %i feasible combinations%s@." green feasible_combos reset;
  feasible_combos

let arrange_towels input =
  let patterns = List.hd_exn input |> parse_patterns in
  let designs = List.drop (List.tl_exn input) 1 in
  designs |> List.map ~f:(count_feasible_combos ~patterns)

let solve_part_1 input = arrange_towels input |> List.count ~f:Int.is_positive
let solve_part_2 input = arrange_towels input |> List.fold ~init:0 ~f:( + )
