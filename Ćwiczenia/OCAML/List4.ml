(* LISTA 4 *)

(* 

  #use "/Users/alexrogozinski/visualStudioCode/OCAML/Ćwiczenia/OCAML/List4.ml";;

*)

(* -- ZADANIE 1 --------------------------------------------------------------------------------------------- *)

(* 

a) let f1 x y z = x y z;;

  x: 'a -> 'b -> 'c
  f1: ('a -> 'b -> 'c) -> 'a -> 'b -> 'c

b) let f2 x y = function z -> x::y;;

  y: 'a list
  x: 'a
  z: 'b -> 'a list 
  f2: ('a -> 'a list) -> 'b- > 'a list

*)

(* -- ZADANIE 2 --------------------------------------------------------------------------------------------- *)

let someFunction x = failwith "Error."

(* -- ZADANIE 3 --------------------------------------------------------------------------------------------- *)

(* Definicja drzewa binarnego z wykładu: *)
type 'a bt = Empty | Node of 'a * 'a bt * 'a bt

let tt = Node(1,
              Node(2,
                  Node(4,
                      Empty,
                      Empty 
                      ),
                  Empty 
                  ),
              Node(3,
                  Node(5,
                      Empty,
                      Node(6,
                          Empty,
                          Empty 
                          )
                      ), 
                  Empty
                  )
              )

(* Wersja nieoptymalna  - O(n^2) *)
let rec breadthBT tree =
  match tree with
  | Empty -> []
  | Node(value, left, right) -> value :: breadthBT left @ breadthBT right

(* Wersja optymalna     - O(n) *)
let breadthBT' tree =
  let rec helperFunc = function
    | (Empty, list) -> list
    | (Node(value, left, right), list) -> value :: helperFunc (left, helperFunc (right, list))
  in
  helperFunc (tree, [])

(* 

breadthBT tt;;
breadthBT' tt;;

*)

(* -- ZADANIE 4 --------------------------------------------------------------------------------------------- *)

(*  

a) - długośc sciezki wewnętrznej - O(n)

*)

let internalPathLength tree =
  let rec helperFunc tree depth =
    match tree with
    | Empty -> 0
    | Node(_, left, right) -> depth + helperFunc left (depth + 1) + helperFunc right (depth + 1)
  in
  helperFunc tree 0  

(* 

b) - długośc sciezki zewnętrznej O(n)

*) 

let externalPathLength tree =
  let rec helperFunc tree depth =
    match tree with
    | Empty -> depth
    | Node(_, left, right) -> helperFunc left (depth + 1) + helperFunc right (depth + 1)
  in
  helperFunc tree 0  

(* 

internalPathLength tt;;
extrernalPathLength tt;;

*)

(* -- ZADANIE 5 --------------------------------------------------------------------------------------------- *)

type 'a graph = Graph of ('a -> 'a list)

(* Graf z wykładu: *)
let g = Graph (function
  | 0 -> [3]
  | 1 -> [0;2;4]
  | 2 -> [1]
  | 3 -> []
  | 4 -> [0;2]
  | n -> failwith ("Graph g: node "^string_of_int n^" doesn't exist")
)

(* O(n + m); n, m - ilośc wierzchołkow i krawędzi *)
let depthSearch (Graph succ) startNode =
  let rec search visited queue =
    match queue with
    | [] -> List.rev visited
    | listHead :: listTail -> 
        if (List.mem listHead visited) then search visited listTail
        else search (listHead :: visited) ((succ listHead) @ listTail)
  in
  search [] [startNode]

(* 

depthSearch g 4;;

*)  
