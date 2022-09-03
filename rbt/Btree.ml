open Base
open Caml.Format

type btree =
  | Node of int list * btree list
  | Leaf of int list

let level = 3
let max_list_length = 3
let split_list list =
  let n = List.length list in
  () 
;;

let find_next_node value_list child_list value = 
  let next = List.find ~f:(fun x -> List.hd_exn x > value && x |> List.rev |> List.hd_exn < value) child_list in
  ()
;;

let insert_to_list insert_value list =
  let rec sub l =
    match l with
    | h :: t when insert_value < h -> insert_value :: h :: t
    | h :: t -> h :: sub t
    | [] -> [insert_value]
  in
  sub list
;;

let get_values = function
  | Node(l, _) | Leaf(l) -> l

let insert_btree_to_btree_list new_btree list =
  let last_element = new_btree |> get_values |> List.last_exn in
  let rec sub l =
    match l with
    | head :: tail when last_element < (head |> get_values |> List.hd_exn) -> new_btree :: head :: tail
    | head :: tail -> head :: sub tail
    | _ -> [new_btree]
  in sub list
;;

let rec get_first_k_els k list = match list with
  | h :: t -> if k = 1 then [h] else h :: get_first_k_els (k - 1) t
  | [] -> failwith "Error"
;;

let split_node_list list =
  let half_length = List.length list / 2 in
  let first_part = get_first_k_els half_length list in
  let second_part = list |> List.rev |> get_first_k_els (if List.length list % 2 = 1 then half_length else half_length - 1) |> List.rev in
  let middle_element = List.nth_exn list (List.length list / 2) in
  (first_part, middle_element, second_part)
;;

type return_value =
  | NewNode of btree
  | Overflow of btree * int * btree 

let rec add value tree =
  let rec sub_split acc values nexts =
    match (values, nexts) with
    | (h1 :: _, h2 :: t2) when value < h1-> (acc, h2, t2)
    | (_ :: t1, h2 :: t2) -> sub_split (acc @ [h2]) t1 t2
    | ([], [last]) -> (acc, last, [])
    | _ -> failwith "Incorrect tree node"
  in
  match tree with
  | Node(values, next_nodes) -> 
    let (first_part, next, second_part) = sub_split [] values next_nodes in
    let new_value = add value next in
    let result =
      match new_value with
      | NewNode n ->
        NewNode (Node(values, first_part @ [n] @ second_part))
      | Overflow(left, new_value_to_insert, right) -> 
        let new_value_list = insert_to_list new_value_to_insert values in
        let new_next_list = 
          (first_part @ second_part) 
          |> insert_btree_to_btree_list left 
          |> insert_btree_to_btree_list right 
        in
        if List.length new_value_list > max_list_length then
          let (first_part, middle, second_part) = split_node_list new_value_list in
          let first_next_part = next_nodes |> get_first_k_els (List.length first_part + 1) in
          let second_next_part = next_nodes |> List.rev |> get_first_k_els (List.length second_part + 1) |> List.rev in
          Overflow(Node(first_part, first_next_part), middle, Node(second_part, second_next_part))    
        else
          let updated_node = Node(insert_to_list new_value_to_insert values, new_next_list) in
          NewNode(updated_node)
    in result
  | Leaf(values) ->
    let new_value_list = insert_to_list value values in
    if List.length new_value_list > max_list_length then 
      let (first_part, middle, second_part) = split_node_list new_value_list in
      Overflow(Leaf(first_part), middle, Leaf(second_part))
    else 
      NewNode(Leaf(insert_to_list value values))
;;

let add_value new_value tree =
  let result = add new_value tree in
  match result with
  | NewNode(n) -> n
  | Overflow(left, middle, right) -> Node([middle], [left; right])
;;

let rec print_tree tree =
  match tree with
  | Node(values, next) ->
    printf "( ";
    List.iter ~f:(printf "%i ") values;
    List.iter ~f:print_tree next;
    printf " )"
  | Leaf(values) ->
    printf "[";
    List.iter ~f:(printf "%i ") values;
    printf "]"
;; 


let balance tree =
  ()
;;

let rec add_node new_node tree = 
  match tree with
  | Node(l, [])
  | Leaf(l) when List.length l < max_list_length -> Leaf(insert_to_list new_node l)
  | Node(l, next_node_list) ->
    let rec find_next_node current_nodes next_nodes =
      match (current_nodes, next_nodes) with
      | (h1 :: t1, h2 :: t2) when new_node < h1 -> h2
      | _ -> failwith "a"
    in

    let next_index = List.foldi ~f:(fun i acc x -> if x < new_node && acc < 0 then i else acc) ~init:(-1) l in
    Node([], [])

  | _ -> Node([], [])
;;

(*
let () =
  let l = [1; 3; 6; 8] in
  let new_l = insert_to_list 7 l in
  List.iter ~f:(printf "%i ") new_l;
;;*)