open Aoc
open Core

let () =
  let test_data = In_channel.read_lines "data/day01.test" in
  let main_data = In_channel.read_lines "data/day01.main" in
  let part_one_test = Day01.solve_part_one test_data in
  let part_one_main = Day01.solve_part_one main_data in
  let part_two_test = Day01.solve_part_two test_data in
  let part_two_main = Day01.solve_part_two main_data in
  Fmt.pr "@.--- DAY 01 ---@.";
  Fmt.pr "@.# Part I";
  Fmt.pr "@.test: %d" part_one_test;
  Fmt.pr "@.main: %d" part_one_main;
  Fmt.pr "@.# Part II";
  Fmt.pr "@.test: %d" part_two_test;
  Fmt.pr "@.main: %d" part_two_main;
  Fmt.pr "@."
