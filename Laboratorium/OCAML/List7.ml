(* LISTA 7 *)

(*  

  #use "/Users/alexrogozinski/visualStudioCode/OCAML/Laboratorium/OCAML/List7.ml";;

*)

(* -- ZADANIE 3 --------------------------------------------------------------------------- *)

type 'a drzewo = 
  | Lisc of 'a
  | Wezel of 'a drzewo * 'a drzewo


let rec isSubtree tree1 tree2 =
  match (tree1, tree2) with                                                                   (* Dopasuj parę drzew do wzorca *)
  | (Lisc leaf1, Lisc leaf2) -> leaf1 = leaf2                                                 (* Jeśli obydwa drzewa to liście, przyrownaj je *)
  | (Wezel (left1, right1), Wezel (left2, right2)) ->                                         (* Jeśli obydwa drzewa to węzły, *)
    (isSubtree left1 tree2) || (isSubtree left2 tree2) ||                                     (* Wywołaj funkcję dla pierwszego lewego poddrzewa, drugiego lewego poddrzewa *)
    (isSubtree left1 left2 && isSubtree right1 right2)                                        (* oraz dla wszystkich poddrzew *)
  | (_, Wezel (left, right)) ->                                                               (* Jeśli drzewo, do ktorego przyrownujemy jest węzłem, *)
    isSubtree tree1 left || isSubtree tree1 right                                             (* Wywołaj funkcję dla jego lewego i prawego poddrzewa *)
  | _ -> false                                                                                (* W kazdym innym przypadku, zwroc fałsz *)


  let tree1 = Wezel (Lisc 1, Lisc 2)
  let tree2 = Wezel (Lisc 16, Lisc 2)
  let tree3 = Wezel (Wezel (Lisc 1, Lisc 2), Lisc 3)
  let tree4 = Lisc 2
  let tree5 = Wezel (Wezel (Lisc 1, Lisc 2), Lisc 3)
  
  (*
  
    isSubtree tree1 tree3;;
    isSubtree tree2 tree3;;
    isSubtree tree4 tree3;;
    isSubtree tree5 tree3;;
  
  *)     
