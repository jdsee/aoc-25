open! Core

(* let astar start goal h = *)
(*   let candidates = Set.empty (module Char) in *)
(*   let parents = Map.empty (module Char) in *)
(*   (* lowest_cost_to_start *) *)
(*   let g_score = Map.empty (module Char) |> Map.set ~key:start ~data:0 in *)
(*   (* lowest_expected_total_cost *) *)
(*   let f_score = Map.empty (module Char) |> Map.set ~key:start ~data:(h start) in *)
(*   let loop candidates = *)
(*     (* find candidate with lowest f_score *) *)
(*     let curr = '?' in *)
(*     let curr_g_score = Map.find g_score curr in *)
(*     if Char.(curr = goal) then (* reconstruct path *) *)
(*       curr_g_score *)
(*     else ( *)
(*       (* get neighbours of curr *) *)
(*       [ (* neighbours *) ] *)
(*       |> List.iter ~f:(fun neighbor -> *)
(*              if curr_g_score < Map.find g_score neighbor then *)
(*                (* found better path -> record *) *)
(*                (* let _ = Map.set candidates ~key:neighbour ~data:curr in *) *)
(*                (* let _ = Map.set g_score ~key:neighbour ~data:curr_g_score in *) *)
(*                (* let _ = Map.set g_score ~key:neighbour ~data:curr_g_score in *) *)
(*                () *)
(*              else ()); *)
(*       None) *)
(*   in *)
(* () *)

(*

029A

A -> 0
0 -> 2
2 -> 9
9 -> A

<A^A^^>AvvvA

v<<A>>^A<A>A<AAv>A^A



*)

module Input_device = struct
  type t = Node of char | Edge of t * t
  type alt = { id : char; edges : alt list }

  let ex = Edge (Node 'A', Node '0')

  let ex_alt =
    let rec b0 = { id = '0'; edges = [ bA ] }
    and bA = { id = 'A'; edges = [ b0 ] } in
    bA
end

let parse_moves code =
  code
  |> String.fold ~init:('A', []) ~f:(fun (prev, acc) c -> (c, (prev, c) :: acc))
  |> Tuple2.get2
  |> List.rev

module Dir = struct
  type t = Left | Right | Down | Up

  let to_input = function Left -> '<' | Right -> '>' | Down -> 'v' | Up -> '^'
end

type button = { value : char }

let number_pad =
  [|
    [| '7'; '8'; '9' |];
    [| '4'; '5'; '6' |];
    [| '1'; '2'; '3' |];
    [| ' '; '0'; 'A' |];
  |]

let arrow_pad =
  [|
    [| ' '; '^'; 'A' |];
    [| '<'; 'v'; '>' |];
  |]
  [@@ocamlformat "disable"]

let possible_moves source = [ source ]

let shortest_input_seq source _target =
  (*
    - 
  *)
  source |> possible_moves

let solve_part_1 input =
  let _ =
    List.map input ~f:(fun code ->
        let moves = parse_moves code in
        Fmt.pr "@.%a" Fmt.Dump.(list (pair Char.pp Char.pp)) moves;
        42)
  in
  42

let solve_part_2 _input = 42

(*

A -- 3 -- 6 -- 9
|    |    |    |
0 -- 2 -- 5 -- 8
     |    |    |
     1 -- 4 -- 7



A 0 <
A 3 ^

9 [ 6 v ; 8 < ]
6 [ 5 < ; 3 v ]
3 [ A v ; 2 < ]

A -> 9
---
  9
  |
  [ 6 ; 8 ]
    |   |
    |   [ 5 ; 7 ; 9 ]
    |
    [ 5 ; 3]
*)

(* +---+---+---+
   | 7 | 8 | 9 |
   +---+---+---+
   | 4 | 5 | 6 |
   +---+---+---+
   | 1 | 2 | 3 |
   +---+---+---+
       | 0 | A |
       +---+---+

       +---+---+
       | ^ | A |
   +---+---+---+
   | < | v | > |
   +---+---+---+
*)
