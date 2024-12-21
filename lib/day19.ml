open Core

module Log = struct
  let red = "\x1b[31m"
  let green = "\x1b[32m"
  let blue = "\x1b[34m"
  let yellow = "\x1b[33m"
  let magenta = "\x1b[35m"
  let cyan = "\x1b[36m"
  let grey = "\x1b[90m"
  let reset = "\x1b[0m"

  let progress design i chunk chunk_len =
    let pad = String.make i '.' in
    let rem = String.drop_prefix design (i + chunk_len) in
    Fmt.pr "@.%s%s%s%s%s%s" grey pad yellow chunk reset rem

  let heading design patterns total_len max_pattern_len =
    Fmt.pr "@.@.%s%s %slen %i%s" blue design grey total_len reset;
    Fmt.pr "@.%s%a %smax-len %i%s" cyan (Fmt.Dump.list String.pp)
      (Set.to_list patterns) grey max_pattern_len reset

  let results count =
    Fmt.pr "@.%s🗹 found %i feasible combinations%s@." green count reset
end

let memoize f =
  let hashable =
    Hashtbl.Hashable.of_key
      (module struct
        type t = int * int [@@deriving compare, hash, sexp]
      end)
  in
  Memo.recursive f ~hashable

let parse_patterns s =
  String.split ~on:',' s
  |> List.map ~f:String.strip
  |> Set.of_list (module String)

let count_feasible_combos design ~patterns =
  let total_len = String.length design in
  let max_pattern_len =
    Set.fold patterns ~init:0 ~f:(fun acc p -> max acc (String.length p))
  in
  let count_memoized =
    memoize @@ fun recur (i, chunk_len) ->
    if i = total_len then 1
    else if chunk_len > max_pattern_len || i + chunk_len > total_len then 0
    else
      let chunk = String.sub design ~pos:i ~len:chunk_len in
      let acc = recur (i, chunk_len + 1) in
      if Set.mem patterns chunk then (
        Log.progress design i chunk chunk_len;
        acc + recur (i + chunk_len, 1))
      else acc
  in
  Log.heading design patterns total_len max_pattern_len;
  let count = count_memoized (0, 1) in
  Log.results count;
  count

let arrange_towels input =
  let patterns = List.hd_exn input |> parse_patterns in
  let designs = List.drop (List.tl_exn input) 1 in
  List.map designs ~f:(count_feasible_combos ~patterns)

let solve_part_1 input = arrange_towels input |> List.count ~f:Int.is_positive
let solve_part_2 input = arrange_towels input |> List.fold ~init:0 ~f:( + )
