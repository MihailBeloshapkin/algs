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


module NumParser = struct
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

end

module KV = struct
  let key = take_while1 (function 'a'..'z' -> true | _ -> false)

  let value = take_while1 (function 'a'..'z' -> true | '0'..'9' -> true | _ -> false)

  let kv =
    key <* whitespace
    >>= fun k -> value >>= fun v -> return (k, v)
end

let () =
  let result = Angstrom.parse_string (whitespace *> KV.kv <* whitespace) ~consume:Angstrom.Consume.All " token afg646yhdy6 " in 
  match result with
  | Result.Ok (k, v) -> printf "%s %s \n" k v
  | _ -> printf "SOMETHING WENT WRONG\n"
;;
