open Base
open Caml.Format
open Angstrom


type ident = string

type literal =
  | Int of int
  | Float of float
  | String of string

type arg =
  | Id of ident
  | Lit of literal

type binop =
  | Add
  | Sub
  | Mul
  | Div

type exps =
  | Exp_fun of string * ident list * exps
  | Exp_letbinding of ident * exps
  | Exp_ident of ident
  | Exp_literal of literal
  | Exp_seq of exps * exps
  | Exp_apply of ident * exps list  
  | Exp_unit

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
      return (sign ^ whole)
    | true -> take_while1 is_digit >>= fun part ->
      return (sign ^ whole ^ "." ^ part)
end

module KV = struct
  let key = take_while1 (function 'a'..'z' -> true | _ -> false)

  let value = take_while1 (function 'a'..'z' -> true | '0'..'9' -> true | _ -> false)

  let kv =
    key <* whitespace
    >>= fun k -> value >>= fun v -> return (k, v)
end

module SimpleLangParser = struct
  let keywords_list = ["let"; "in"]

  let is_kw = List.fold

  let token_separator = take_while (is_whitespace)

  let as_token p = token_separator *> p <* token_separator


  let is_letter = function
    | 'a'..'z' -> true
    | 'A'..'Z' -> true
    | _ -> false
  ;;

  let is_digit = function
    | '0'..'9' -> true
    | _ -> false
  ;;

    
  let space = take_while is_whitespace
  let space1 = take_while1 is_whitespace
  let token s = space *> string s


    (* CHANGED!!!!!! *)
  module Literals = struct
    let int_token = space *> (take_while1 is_digit) >>= fun res -> return @@ Exp_literal (Int (int_of_string res))

    let float_token = space *> NumParser.number >>= fun res -> return @@ Exp_literal (Float (float_of_string res))

    let string_token  = 
      space *> char '"' *> take_while (fun c -> not (Char.equal c '"')) <* char '"'
      >>= fun res -> return @@ Exp_literal (String ("\"" ^ res ^ "\"")) 
  end

  module BinOperators = struct
    let asoc0_t = choice [ string "&&"; string "*"; string "/" ] |> as_token
    let asoc1_t = choice [ string "||"; string "+"; string "-" ] |> as_token
  
    let asoc2_t =
      choice [ string "=="; string "!="; string ">="; string "<="; string ">"; string "<" ]
      |> as_token
    ;;
  
    let binops = choice [ asoc0_t; asoc1_t; asoc2_t ]

    let ar_operators =
      choice [ token "+"; token "-"; token "/"; token "*" ]
  end

  let new_ident =
    space
    *> (take_while1 is_letter)
    >>= fun str -> 
      if String.equal str "in" then fail "Keyword in the wrong place of program" else return str
    (* *> lift2
          (fun c oth -> Char.to_string c ^ oth)
          (satisfy is_letter)*)
  ;;

  let int_number = take_while1 is_digit >>= fun s -> return @@ int_of_string s


  let parse_declaration expr =
    as_token
    @@ lift3
        (fun a b c-> printf "%s" b; a, b, c)
        (token "let" (* *> option false (token "rec" >>| fun _ -> true)*))
        (space1 *> new_ident >>| (fun x -> x))
        (lift2 (fun q w -> ()) (space *> many new_ident <* token "=") expr)
  ;;

  let parse_let e = lift2 (fun (_, i, _) b -> printf "%s" i; i) (parse_declaration e) (token "in" *> space1 *> e)

  let rec link_exps e_list =
    match e_list with
    | e :: []  -> e
    | h :: t -> Exp_seq (h, (link_exps t))
    | _ -> failwith "empty list"
  ;;

  let let_constructor name arg_list body =
    match arg_list with
    | [] -> Exp_letbinding (name, body)
    | _ -> 
      printf "Size: %i" (List.length arg_list);
      List.iter ~f:(printf "\n%s \n") arg_list;
      failwith "error!!!!"
  ;;

  let literals =
    choice
    [
      Literals.float_token
    ; Literals.int_token
    ; Literals.string_token
    ]



  let arithm_parser =
    let c = choice [(new_ident >>= fun res -> return @@ Exp_ident res); literals] in 
    lift3
      (fun arg1 operator arg2 -> Exp_apply (operator, [arg1; arg2]))
      (space *> c <* space)
      (BinOperators.ar_operators)
      (space *> c <* space)
  ;;

  let arg_of_application =
    choice
    [
      (new_ident >>= fun res -> return @@ Exp_ident res)
    ; Literals.int_token
    ]
  
  let appl =
    lift2 
      (fun id li -> 
        Exp_apply (id, li))
      (new_ident)
      ((many (space1 *> arg_of_application)) <* space)
  ;;

  let decl =
    lift3 
      (fun a b c -> let_constructor a b c)
      ((token "let") *> space1 *> new_ident) 
      (many (space1 *> new_ident))
      (space *> token "=" *> space *> (choice [arithm_parser; literals; appl]) <* space <* token "in")
  ;;

  let base =
    choice [ arithm_parser; decl; appl ]

  let p = 
    many 
      (base <* char '\n' <|> base <* space1 <|> base) 
      >>= fun res -> return @@ link_exps res

  let p1 = appl

  let exp_seq_parser = 
    fix (fun expr -> parse_let expr) 
end

module Priner = struct
  let print_let = function
    | (name, Int i) -> printf "Name: %s; Val: %i\n" name i
    | (name, Float i) -> printf "Name: %s; Val: %f\n" name i
    | (name, String i) -> printf "Name: %s; Val: %s\n" name i
  ;;

  let print_literal = function
    | Int i -> printf "(Int: %i)" i
    | Float f -> printf "(Float: %f)" f
    | String s -> printf "(String: %s)" s
  ;;

  let rec print_ast = function
    | Exp_letbinding (id, value) -> 
      printf "(LetB: Name=%s value=" id;
      print_ast value;
      printf ")"
    | Exp_literal l -> print_literal l
    | Exp_ident i -> printf "(Ident: %s)" i
    | Exp_seq (e1, e2) -> 
      printf "Seq (";
      print_ast e1;
      print_ast e2;
      printf ")";
    | Exp_apply (name, arg_list) -> printf "(Apply: name=%s Args:" name; List.iter ~f:(print_ast) arg_list; printf ")"
    | _ -> printf "Unrecognised Ast Node"
  ;;
end

let () =
  let result = 
    Angstrom.parse_string 
      (SimpleLangParser.p) 
      ~consume:Angstrom.Consume.All 
      "let a = 10 in let b = 20 in a + b"
      (*"let name = 30 + 1 in \n let eman = 12.5 in \n let s = \"asdf\" in "*) 
  in 
  match result with
  | Result.Ok res -> Priner.print_ast res
  (*
  | Result.Ok [(Exp_letbinding (name, Int value))] -> 
    printf "%s \n" name;
    printf "%i \n" value;
  (*  List.iter ~f:(printf "%s ") il 
  | Result.Ok (Exp_letbinding (name, Float value)) -> 
    printf "%s \n" name;
    printf "%f \n" value;*)
  | Result.Ok [(Exp_letbinding (name, String value)); (Exp_letbinding (name1, Int value1))] -> 
      printf "%s \n" name;
      printf "%s \n" value;
  | Result.Ok r -> 
    List.iter ~f:(fun l -> match l with | Exp_letbinding (name, value) -> Priner.print_let (name, value) | _ -> ()) r;
    printf "OK: %i" (List.length r)*)
  | Result.Error s -> printf "SOMETHING WENT WRONG: %s\n" s
;;
