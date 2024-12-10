(* LISTA 5 *)

(*  

  #use "/Users/alexrogozinski/visualStudioCode/OCAML/Laboratorium/OCAML/List5.ml";;

*)

(* -- ZADANIE 2 --------------------------------------------------------------------------- *)

let (+) vector1 vector2 =   
  let rec aux rest1 rest2 result =                                                            (* Funkcja pomocnicza *)
    match (rest1, rest2) with                                                                 (* Dopasuj parę aktualnej części wektorow do wzorca *)
    | ([], []) -> List.rev result                                                             (* Jeśli oba wektory są puste, zwroc odwrocony wynik *)
    | (head :: tail, []) -> aux tail [] (head :: result)                                      (* Jeśli pierwszy wektor z pary nie jest pusty, wywołaj funkcję pomocniczą dla reszty wektora *)
    | ([], head :: tail) -> aux [] tail (head :: result)                                      (* Jeśli drugi wektor z pary nie jest pusty, wywołaj funkcję pomocniczą dla reszty wektora *)
    | (head1 :: tail1, head2 :: tail2) ->                                                     (* Jeśli zaden wektor nie jest pusty, *)
      aux tail1 tail2 ((head1 +. head2) :: result)                                            (* Wywołaj funkcję dla reszty wektorow *)
  in
  aux vector1 vector2 []                                                                      (* Wywolaj funkcję pomocniczą dla obu wektorow *)


(* 

  [1.; 2.] + [3.; 1.];;
  [1.; 2.; 5.] + [3.; 1.];;
  [1.; 2.] + [3.; 1.; 17.];;
  [] + [1.];;
  [0.; 0.] + [47.];;
  [0.; 0.; 0.] + [0.; 47.];;

*)
