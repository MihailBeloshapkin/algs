open Base
open Caml.Format

type color = Red | Black

type red_black_tree =
  | Node of int * color * red_black_tree * red_black_tree
  | Nil
  | Empty

let add_node tree new_value =
  match tree with
  | Empty -> Node(new_value, Black, Nil, Nil)
  | Node(value, col, left, right) -> Empty
  | _ -> failwith "exceptions"
;;


let () =
  let open Btree in
  (*let l = [1;2; 7; 8] in
  let (f, m, s) = split_node_list l in
  List.iter ~f:(printf "%i ") f;
  printf "\n";
  printf "%i" m;
  printf "\n";
  List.iter ~f:(printf "%i ") s;*)
  let empty = Leaf([]) in 
  let t = 
    empty
    |> add_value 1
    |> add_value 3
    |> add_value 4
    |> add_value 5
    |> add_value 7
    |> add_value 8
    |> add_value 9  
  in
  print_tree t
;;