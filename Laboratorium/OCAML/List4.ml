(* LISTA 4 *)

(*  

  #use "/Users/alexrogozinski/visualStudioCode/OCAML/Laboratorium/OCAML/List4.ml";;

*)

(* -- ZADANIE 2 --------------------------------------------------------------------------- *)

let multiplyElements list =
  let rec addPowers current_element result_elem power res =                                   (* Funkcja pomocnicza do tworzenia podlist z potęg elementu *)
    if (power > 0) then                                                                       (* Jeśli aktualna potęga > 0, *)
      let new_result_elem = result_elem *. current_element in                                 (* Niech new_result_elem będzie aktualną potęgą elementu *)
      addPowers current_element new_result_elem (power - 1) (result_elem :: res)              (* Wywołaj rekurencyjnie funkcję pomocniczą dla aktualnych danych, przejdź do kolejnej potęgi *)
    else res                                                                                  (* W przeciwnym wypadku, zwroc wynik *)
  in

  let rec iterateList current_list n result =                                                 (* Funkcja pomocznica do przechodzenia przez listę *)
    match current_list with                                                                   (* Dopasuj listę do wzorca *)
    | [] -> List.rev result                                                                   (* Jeśli lista jest pusta, zwroc wynik *)
    | head :: tail ->                                                                         (* W przeciwnym wypadku, *)
      iterateList tail (n + 1) ((addPowers head head n []) @ result)                          (* Przejdz do kolejnego elementu, a dla poprzedniego wykonaj potęgowanie *)
  in

  iterateList list 1 []                                                                       (* Wywołaj funkcję do przechodzenia przez listę *)


let list = [1.0; 2.0; 3.0; 4.0; 5.0; 5.0; 6.0; 7.0; 8.0; 9.0] 

(*  

  multiplyElements list;;

*)