(* LISTA 6 *)

(*  

  #use "/Users/alexrogozinski/visualStudioCode/OCAML/Laboratorium/OCAML/List6.ml";;

*)

(* -- ZADANIE 1 --------------------------------------------------------------------------- *)

type litera = Zawiera | Nie_zawiera                                                           (* Definicja typu litera *)

type dane_slow =                                                                              (* Definicja typu dane_slow *)
  | Puste                                                                                     (* Długośc: 0 *)
  | Krotkie of litera                                                                         (* Długośc: 1 - 10 *)
  | Srednie of litera                                                                         (* Długośc: 11 - 20 *)
  | Dlugie of litera                                                                          (* Długośc: 20+ *)


let wordInfo list character =
  let rec aux current_list result =                                                           (* Funkcja pomocnicza *)
    match current_list with                                                                   (* Dopasuj akutalną listę do wzorca *)
    | [] -> List.rev result                                                                   (* Jeśli lista jest pusta, zwroc wynik *)
    | head :: tail ->                                                                         (* W przeciwnym wypadku, *)
      match head with                                                                         (* Dopasuj głowę listy do wzorca *)
      | "" -> aux tail (Puste :: result)                                                      (* Jeśli głowa listy to pusty wyraz, dodaj 'Puste' do wyniku *)
      | head ->                                                                               (* W przeciwnym wypadku, *)

        let contains_char =                                                                   (* Niech zmienna contains_char jest znacznikiem zawierania litery w wyrazie *)
          match (String.contains head character) with                                         (* Dopasuj wynik funkcji .contains do wzorca *)
          | true -> Zawiera                                                                   (* Jeśli wynik = true, zwroc 'Zawiera' *)
          | false -> Nie_zawiera                                                              (* Jeśli wynik = false, zwroc 'Nie_zawiera' *)
        in    

        let word_length = String.length head in                                               (* Niech zmienna words_length jest długością głowy listy (akutalnego wyrazu) *)
        let res =                                                                             (* Niech zmienna res jest długością aktualnego wyrazu *)
          if (word_length <= 10) then (Krotkie contains_char)                                 (* Jeśli długośc <= 10, zwroc 'Krotkie' *)
          else if (11 <= word_length && word_length <= 20) then (Srednie contains_char)       (* Jeśli 11 <= długośc <= 20, zwroc 'Srednie' *)
          else (Dlugie contains_char)                                                         (* Jeśli długośc > 20, zwroc 'Dlugie' *)
        in
        aux tail (res :: result)                                                              (* Dodaj res do wyniku *)
    in
    aux list []                                                                               (* Wywołaj funkcję pomocniczą *)
        
(* 

  wordInfo ["Ala"; "ma"; "kota"] 'a';;
  wordInfo ["Zaba"; "na"; "monocyklu"] 'w';;
  wordInfo ["Kot"; "zjadl"; "mysz"] 'o';;
  wordInfo [""; "fawfaw"; "fwawgwagwagaw"; "gagegsegsehehsssseehs"] 'w';;
  wordInfo [""; "deszcz"; "pada"] 'g';;

*)
