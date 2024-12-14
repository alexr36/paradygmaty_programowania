(* LISTA 9 *)

(*  

  #use "/Users/alexrogozinski/visualStudioCode/OCAML/Laboratorium/OCAML/List9.ml";;

*)

(* -- ZADANIE 1 --------------------------------------------------------------------------- *)

type 'a llist =                                                                               (* Definicja typu listy leniwej *)
  | LNil
  | LCons of 'a * (unit -> 'a llist)

let rec lfrom k =                                                                             (* Funkcja do generowania listy leniwej od zadanego elementu *)
  LCons (k , fun () -> lfrom (k + 1))

let rec ltake (n, lxs) =                                                                      (* Funkcja do ekstrahowania n pierwszych elementow z zadanej listy leniwej *)
  match (n, lxs) with
  | (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons (x, xf)) -> x :: ltake (n - 1, xf ())


let rec (#@) llist1 llist2 =                                                                  (* Definicja operatora konkatenacji dla list leniwych *)
  match llist1 with                                                                           (* Dopasuj pierwszą listę do wzorca *)
  | LNil -> llist2 ()                                                                         (* Jeśli lista jest pusta, zwroc listę drugą *)
  | LCons (x, xf) -> LCons (x, fun () -> (xf ()) #@ llist2)                                   (* W przeciwnym wypadku, dodaj element do listy wynikowej i przejdź do kolejnego *)  


let transformLlist llist func =                                                               
  let rec repeatElements elem count =                                                         (* Funkcja pomocnicza do tworzenia podlisty leniwej z powtorzeniami zadanego elementu *)
    if (count <= 0) then LNil                                                                 (* Jeśli licznik powtorzen jest <= 0, zwroc listę pustą *)
    else LCons (elem, fun () -> repeatElements elem (count - 1))                              (* W przeciwnym wypadku, dodaj element do listy wynikowej i zmniejsz licznik o 1 *)
  in
  
  let rec aux current_llist i =                                                               (* Funkcja pomocnicza głowna *)
    match current_llist with                                                                  (* Dopasuj aktualną listę do wzorca *)
    | LNil -> LNil                                                                            (* Jeśli lista jest pusta, zwroc listę pustą *)
    | LCons (x, xf) ->                                                                        (* W przeciwnym wypadku, *)
      let repeats = repeatElements x (func i) in                                              (* Niech repeats będzię listą leniwą złozoną z powtorzen zadanego elementu x w liczbie func(i), gdzie i to pozycja w liście wejściowej *)
      repeats #@ (fun () -> aux (xf ()) (i + 1))                                              (* Dodaj do siebie listę wynikową repeats i wynikową z wywołania funkcji dla następnego elementu listy *)
  in

  aux llist 1                                                                                 (* Wywołanie funkcji pomoczniej *)


let example_func x = x - 1 
let example_llist = lfrom 1

(* 
  ltake (15, (transformLlist example_llist example_func));;
*)


(* -- ZADANIE 2 --------------------------------------------------------------------------- *)

type slowo = (int * int)

let rec addToDict elem (dict : slowo list) =
  match dict with                                                                             (* Dopasuj słownik do wzorca *)
  | [] -> [(elem, 1)]                                                                         (* Jeśli słownik jest pusty, zwroc słownik z dodanym zadanym elementem *)
  | (value, count) :: tail ->                                                                 (* W przeciwnym wypadku, *)
    if (elem = value) then (value, count + 1) :: tail                                         (* Jeśli dany element juz znajduje się w słowniku, zwiększ jego liczbę wystąpien *)
    else if (elem < value) then (elem, 1) :: dict                                             (* Jeśli dany element jest mniejszy niz pierwsza wartośc w słowniku, dodaj ten element na początek aktualnego słownika *)
    else (value, 1) :: (addToDict elem tail)                                                  (* Jeśli dany element jest większy niz pierwsza wartośc w słowniku, dodaj go do słownika wynikowego wywołania funkcji dla reszty słownika *)


let (dictionary : slowo list) = [
  (5, 3);
  (7, 1 );
  (10, 2);
]

(* 
  addToDict 8 dictionary;;
  addToDict 5 dictionary;;
  addToDict 3 dictionary;;
*)
