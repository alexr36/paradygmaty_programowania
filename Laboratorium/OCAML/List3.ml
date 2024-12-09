(* LISTA 3 *)

(*  

  #use "/Users/alexrogozinski/visualStudioCode/OCAML/Laboratorium/OCAML/List3.ml";;

*)

(* -- ZADANIE 1 --------------------------------------------------------------------------- *)

let splitList (list, pos) =
  if (pos < 0) then raise (Failure "Nie istnieje element o indeksie mniejszym niz 0.")        (* Jeśli podany indeks graniczny jest mniejszy niz 0, rzuc wyjątek *)
  else if (pos >= List.length list) then list                                                 (* Jeśli podany indeks graniczny ma wartośc większą niz dlugośc listy, zwroc listę *)
  else                                                                                        (* W przeciwnym wypadku: *)
    let rec aux current_list current_pos result =                                             (* Funkcja pomocnicza *)
      match (current_list, current_pos) with                                                  (* Dopasuj pare (aktualna lista, aktualna pozycja) do wzorca: *)
      | ([], _) -> (List.rev result, [])                                                      (* Jeśli aktualna lista jest pusta, zwroc wynik *)
      | (head :: tail, 0) -> (List.rev (head :: result), tail)                                (* Jeśli lista nie jest pusta oraz pozycja = 0, dodaj ostatni przetwarzany element to wyniku i zwroc wynik *)
      | (head :: tail, _) -> aux tail (current_pos - 1) (head :: result)                      (* Jeśli lista nie jest pusta a pozycja jest > 0, wywolaj funkcje pomocnicza dla zmniejszonego indeksu pozycji i dodaj ostatni przetwarzany element do wyniku *)
      in
      let (first, second) = aux list pos [] in                                                (* Niech para (first, second) bedzie wynikiem dzialania funkcji pomocniczej *)
      second @ first                                                                          (* Połacz te dwie listy w odwrotnej kolejności *)
      
let list = [1; 2; 3; 4; 5; 5; 6; 7; 8; 9] 

(* 

  splitList (list, 3);;

*)    

(* -- ZADANIE 2 --------------------------------------------------------------------------- *)

let repeatElements list =
  let rec addRepeats element amount res =                                                     (* Funkcja pomocnicza do powielania elementow *)
    if (amount > 0) then addRepeats element (amount - 1) (element :: res)                     (* Jeśli ilosc powtorzen jest > 0, zmniejsz ilosc powtorzen i dodaj element do listy wynikowej, wywolaj funkcje ponownie *)
    else res                                                                                  (* W przeciwnym wypadku, zwroc wynik *)
  in

  let rec iterateList pos current_list result =                                               (* Funkcja pomocnicza do przechodzenia przez listę *)
    match current_list with                                                                   (* Dopasuj aktualną liste do wzorca *)
    | [] -> List.rev result                                                                   (* Jeśli lista jest pusta, odwroc wynik i zwroc go *)
    | head :: tail ->                                                                         (* Jeśli lista nie jest pusta, *)
      iterateList (pos + 1) tail ((addRepeats head pos []) @ result)                          (* przejdz do nastpenej pozycji, jako aktualna listę podaj ogon poprzedniej listy oraz do wyniku dodaj wynik funkcji powielającej elementy *)
  in
  
  iterateList 1 list []                                                                       (* Wywołaj funkcję do przechodzenia przez listę *)

(* 

  repeatElements list;;

*)  