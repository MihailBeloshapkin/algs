open Base
open Caml.Format
open Angstrom

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false
;;

let whitespace = take_while is_whitespace

let is_digit = function '0'..'9' -> true | _ -> false

let integer = take_while1 is_digit

let sign =
  peek_char
  >>= function
    | Some '-' -> advance 1 >>| fun () -> "-"
    | Some '+' -> advance 1 >>| fun () -> "+"
    | Some c when (is_digit c) -> return "+"
    | _ -> fail "Sign or digit expected"
;;

let dot =
  peek_char
  >>= function 
    | Some '.' -> advance 1 >>| fun () -> true
    | _ -> return false
;;

let number =
  sign
  >>= fun sign ->
  take_while1 is_digit
  >>= fun whole ->
  dot
  >>= function
  | false -> 
    return (float_of_string (sign ^ whole))
  | true -> take_while1 is_digit >>= fun part ->
    return (float_of_string (sign ^ whole ^ "." ^ part))

let () =
  let result = Angstrom.parse_string (whitespace *> number <* whitespace) ~consume:Angstrom.Consume.All " -1234.56789 " in 
  match result with
  | Result.Ok x -> printf "%f\n" x
  | _ -> printf "SOMETHING WENT WRONG\n"
;;
