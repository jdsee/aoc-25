open! Aoc
open Core
open Cmdliner

type params = { day : int [@doc "The Day to solve"] [@aka [ "d" ]] }
[@@deriving cmdliner, show]

module type Solution = sig
  val solve_part_1 : string list -> int
  val solve_part_2 : string list -> int
end

let solution_of_day : int -> (module Solution) = function
  | 1 -> (module Day01)
  | 2 -> (module Day02)
  | 8 -> (module Day08)
  | day -> failwithf "Day %d is not implemented" day ()

let solve day =
  let module S = (val solution_of_day day) in
  let test_data = In_channel.read_lines (Fmt.str "data/day0%d.test" day) in
  let main_data = In_channel.read_lines (Fmt.str "data/day0%d.main" day) in
  let test1 = S.solve_part_1 test_data in
  let main1 = S.solve_part_1 main_data in
  let test2 = S.solve_part_2 test_data in
  let main2 = S.solve_part_2 main_data in
  Fmt.pr "@.┌──────────────────────┐";
  Fmt.pr "@.│         Day %d        │" day;
  Fmt.pr "@.└──────────────────────┘@.";
  Fmt.pr "@. TEST";
  Fmt.pr "@.────────────────────────";
  Fmt.pr "@.   Part I:  %d" test1;
  Fmt.pr "@.   Part II: %d@." test2;
  Fmt.pr "@. MAIN";
  Fmt.pr "@.────────────────────────";
  Fmt.pr "@.   Part I:  %d" main1;
  Fmt.pr "@.   Part II: %d" main2;
  Fmt.pr "@."

let () =
  let f p = solve p.day in
  let info = Cmd.info (Sys.get_argv ()).(0) in
  let term = Term.(const f $ params_cmdliner_term ()) in
  let cmd = Cmd.v info term in
  exit (Cmd.eval cmd)
