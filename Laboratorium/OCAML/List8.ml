(* LISTA 8 *)

(*  

  #use "/Users/alexrogozinski/visualStudioCode/OCAML/Laboratorium/OCAML/List8.ml";;

*)

(* -- ZADANIE 3 --------------------------------------------------------------------------- *)

type 'a bt =                                                                                  (* Definicja typu drzewa *)
  | Empty
  | Node of 'a * 'a bt * 'a bt


let isEqual tree1 tree2 =                                                                     (* Funkcja sprawdzająca, czy drzewa są rowne *)
  tree1 = tree2

let t1 = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
let t2 = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
let t3 = Node(1, Node(4, Empty, Empty), Node(3, Empty, Empty))
  
(*
  isEqual t1 t2;;     Wynik: true 
  isEqual t1 t3;;     Wynik: false 
  isEqual t1 t1;;     Wynik: true
*)  

let rec isSubtree tree1 tree2 =                                                               (* Funkcja sprawdzająca, czy drzewo jest poddrzewem drugiego drzewa *)
  match tree2 with                                                                            (* Dopasuj drugie drzewo do wzorca *)
  | Empty -> false                                                                            (* Jeśli jest puste, zwroc fałsz *)
  | Node (_, left, right) ->                                                                  (* W przeciwnym wypadku, *)
    isEqual tree1 tree2 || isSubtree tree1 left || isSubtree tree1 right                      (* Sprawdz czy drzewa są rowne lub czy drzewo jest poddrzewem lewego lub prawego poddrzewa drugiego drzewa *)


let t4 = Node(2, Empty, Empty)
let t5 = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
    
(*
  isSubtree t4 t5;;   Wynik: true 
  isSubtree t5 t4;;   Wynik: false
  isSubtree t5 t5;;   Wynik: true
*)
   

let rec countSubtrees tree1 tree2 =                                                           (* Funkcja obliczająca ile razy drzewo jest poddrzewem drugiego danego drzewa *)
  match tree2 with                                                                            (* Dopasuj drugie drzewo do wzorca *)
  | Empty -> 0                                                                                (* Jeśli drzewo jest puste, zwroc 0 *)
  | Node (_, left, right) ->                                                                  (* W przeciwnym wypadku, *)
    let count =                                                                               (* Niech count będzie licznikiem wystąpien poddrzewa *)
      if (isEqual tree1 tree2) then 1                                                         (* Jeśli drzewa są rowne, zwroc 1 *)
      else 0                                                                                  (* W przeciwnym wypadku, zwroc 0 *)
    in
    count + countSubtrees tree1 left + countSubtrees tree1 right                              (* Dodaj aktualną wartośc licznika wystąpien do wynikow wywołan funkcji dla lewego i prawego podrzewa drugiego drzewa *)


let t6 = Node(2, Empty, Empty)
let t7 = Node(1, Node (2, Empty, Empty), Node(3, Node(2, Empty, Empty), Empty))


(*
  countSubtrees t6 t7;;   Wynik: 2
  countSubtrees t6 t6;;   Wynik: 1
  countSubtrees t7 t6;;   Wynik: 0
*)    
