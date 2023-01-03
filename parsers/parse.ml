open Base
open Caml.Format
open Angstrom

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false


let whitespace = take_while is_whitespace

let is_digit = function '0'..'9' -> true | _ -> false

let integer = take_while1 is_digit

let () =
  let result = Angstrom.parse_string (whitespace *> integer <* whitespace) ~consume:Angstrom.Consume.All "     12z34  " in 
  match result with
  | Result.Ok x -> printf "%s" x
  | _ -> printf "SOMETHING WENT WRONG\n"
;;
