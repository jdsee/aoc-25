open! Core

module Parser = struct
  open Angstrom

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let args =
    sep_by1 (char ',') integer >>| function
    | [ a; b ] -> (a, b)
    | _ -> failwith "Error: Couldn't parse args"

  let parens p = char '(' *> p <* char ')'
  let mul = string "mul" *> parens args

  let ignore_prefix p =
    let rec loop () =
      peek_char >>= function
      | None -> failwith "Error: Reached EOF"
      | Some _ -> p >>| (fun x -> Some x) <|> any_char *> loop ()
    in
    loop ()

  let mul_ignore_prefix = ignore_prefix mul
  let all_valid_muls = many mul_ignore_prefix
  let parse = parse_string ~consume:All all_valid_muls
end

let solve_part_1 _input = 42
let solve_part_2 _input = 42
