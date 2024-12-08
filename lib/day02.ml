open Core

module Monotonicity = struct
  type t = Increasing | Decreasing | Unresolved

  let check m l r =
    match m with
    | Increasing -> l < r
    | Decreasing -> l > r
    | Unresolved -> l <> r
end

let report_of_string s =
  String.split ~on:' ' s
  |> List.filter ~f:(Fun.negate String.is_empty)
  |> List.map ~f:Int.of_string

let rec is_report_safe ?(m = Monotonicity.Unresolved) ?(errors = 0) ~eps levels
    =
  match levels with
  | l1 :: l2 :: rest -> (
      let check_for ?(errors = errors) m =
        let check_next = is_report_safe ~eps ~m in
        let safe = Monotonicity.check m l1 l2 && abs (l1 - l2) <= 3 in
        if safe then check_next (l2 :: rest) ~errors
        else if errors < eps then check_next (l1 :: rest) ~errors:(errors + 1)
        else false
      in
      match m with
      | Increasing | Decreasing -> check_for m
      | Unresolved ->
          if l1 < l2 then
            check_for Increasing || check_for Decreasing ~errors:(errors + 1)
          else check_for Decreasing || check_for Increasing ~errors:(errors + 1)
      )
  | [ _ ] | [] -> true

let count_safe_reports input ~eps =
  input |> List.map ~f:report_of_string |> List.count ~f:(is_report_safe ~eps)

let solve_part_1 = count_safe_reports ~eps:0
let solve_part_2 = count_safe_reports ~eps:1
