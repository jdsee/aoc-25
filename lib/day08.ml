open Core

let collect_antenna_positions input =
  let antennas = Hashtbl.create (module Char) in
  List.iteri input ~f:(fun i line ->
      String.iteri line ~f:(fun j c ->
          if Char.(c <> '.') then
            Hashtbl.update antennas c ~f:(function
              | Some positions -> (i, j) :: positions
              | None -> [ (i, j) ])));
  antennas

module Antinode = struct
  type t = int * int
  (** [t] is [(y, x)], where [y] is the row index and [x] is the column index. *)

  let make ~lim_y ~lim_x y x =
    if y < 0 || y > lim_y || x < 0 || x > lim_x then None else Some (y, x)

  let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
end

let find_antinodes ~lim_y ~lim_x (y1, x1) (y2, x2) =
  let make = Antinode.make ~lim_y ~lim_x in
  let ay1 = y1 + (y1 - y2) in
  let ax1 = x1 + (x1 - x2) in
  let ay2 = y2 + (y2 - y1) in
  let ax2 = x2 + (x2 - x1) in
  List.filter_opt [ make ay1 ax1; make ay2 ax2 ]

let find_antinodes_harmonic ~lim_y ~lim_x ((y1, x1) as p1) (y2, x2) =
  let dy = y1 - y2 in
  let dx = x1 - x2 in
  let rec loop ~acc ~combine (y_prev, x_prev) =
    let y = combine y_prev dy in
    let x = combine x_prev dx in
    match Antinode.make ~lim_y ~lim_x y x with
    | Some p_next -> loop ~acc:(p_next :: acc) ~combine p_next
    | None -> acc
  in
  let collect_bidirectional p =
    let collected = loop ~combine:( + ) ~acc:[ p ] p in
    loop ~combine:( - ) ~acc:collected p
  in
  collect_bidirectional p1

let find_all_antinodes ~f positions =
  List.cartesian_product positions positions
  |> List.filter ~f:(fun ((y1, x1), (y2, x2)) -> y1 <> y2 || x1 <> x2)
  |> List.concat_map ~f:(Tuple2.uncurry f)

let solve_with ~f input =
  let lim_y = List.length input - 1 in
  let lim_x = String.length (List.hd_exn input) - 1 in
  input
  |> collect_antenna_positions
  |> Hashtbl.data
  |> List.concat_map ~f:(find_all_antinodes ~f:(f ~lim_y ~lim_x))
  |> List.dedup_and_sort ~compare:Antinode.compare
  |> List.length

let solve_part_1 = solve_with ~f:find_antinodes
let solve_part_2 = solve_with ~f:find_antinodes_harmonic
