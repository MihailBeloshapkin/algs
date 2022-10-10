open Base
open Caml.Format

type tree =
  | Node of int * tree * tree
  | Empty
 
let rec add new_value = function
  | Node (value, left, right) when new_value > value -> 
    Node (value, left, add new_value right)
  | Node (value, left, right) when new_value < value ->
    Node (value, add new_value left, right)
  | Empty -> Node (new_value, Empty, Empty)
  | _ as t -> t
;;

let delete_max_in_subtree current_node =
  let rec find_max current =
    match current with
    | Node (value,  _, Empty) -> value
    | Node (_,  _, right) -> find_max right
    | Empty -> failwith "Empty"
  in
  let rec delete current =
    match current with
    | Node (_,  left, Empty) -> left
    | Node (value, left, right) -> Node (value, left, delete right)
    | _ -> failwith "Incorrect sub tree"
  in
  find_max current_node, delete current_node
;;

let rec delete node_to_delete = function
    | Node (value, l, r) when value <> node_to_delete ->
      if node_to_delete > value
      then Node (value, l, delete node_to_delete r)
      else Node (value, delete node_to_delete l, r)
    | Node (_, left, right) ->
      let result =
        match left with
        | Empty -> right
        | _ ->
          let max_in_left_sub_tree, new_left = delete_max_in_subtree left in
          Node (max_in_left_sub_tree, new_left, right)
      in
      result
    | _ -> Empty
;;

(* Display *)
let rec output tree =
  match tree with
  | Empty -> ()
  | Node (value,  left, right) ->
    printf "( %i " value;
    output left;
    output right;
    printf ")";
;;

let () =
  Empty
  |> add 1
  |> add 7
  |> add 5
  |> add 9
  |> add 10
  |> add 8
  |> delete 9
  |> output
;;