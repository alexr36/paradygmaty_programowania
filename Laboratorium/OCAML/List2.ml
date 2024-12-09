(* LISTA 2 *)

(* 

  #use "/Users/alexrogozinski/visualStudioCode/OCAML/Lab2/List2.ml";;

*)

(* -- ZADANIE 1 --------------------------------------------------------------------------- *)

(* a) *)
let someFuncA (x, y) =                                                                        (* Dla pary podanych liczb, funkcja zwraca ich dwukrotnośc *)
  (x * 2, y * 2)  

(* b) *)
let someFuncB (x, y) =                                                                        (* Dla pary podanych liczb, funkcja sprawdza, czy pierwsza liczba zwiększona o 2 *)
  x +. 2.0 > y -. 5.0                                                                         (* jest większa od drugiej zmniejszonej o 5 *)
 
(* c) *)
let someFuncC(list, x) =                                                                      
  if (List.length list > x) then list                                                         (* Jeśli długosc listy jest większa od podanej wartości x, zwroc listę *)
  else []                                                                                     (* W przeciwnym wypadku, zwroc listę pustą *)


(* -- ZADANIE 3 --------------------------------------------------------------------------- *)

let evaluatePi precision =
  if (precision <= 0.0) then                                                                  (* Jeśli zadana precyzja jest <= 0 *)
    raise (Failure "Nie mozna obliczyc pi z dokladnoscia mniejsza niz ani rowna zero.")       (* Rzuc wyjątek *)
  else                                                                                        (* W przeciwnym wypadku: *)
    let rec aux prev_term curr_product =                                                      (* Funkcja pomocnicza *)
      let prev_eval = 2.0 /. curr_product in                                                  (* Niech poprzednie przyblizenie pi = 2 / (aktualny iloczyn ciągu) *)
      let next_term = 0.5 +. 0.5 *. prev_term in                                              (* Niech następny wyraz ciągu = 0.5 + 0.5 * (poprzedni wyraz ciągu) *)
      let next_product = curr_product *. sqrt next_term in                                    (* Niech następny iloczyn ciągu = (aktualny iloczyn ciągu) * sqrt(następny wyraz ciągu *)
      let pi_eval = 2.0 /. next_product in

      if (abs_float (pi_eval -. prev_eval) < precision) then pi_eval                          (* Jeśli |(altualne przyblienie pi) - (poprzednie przyblizenie pi)| < dokladnośc, zwroc przyblizenie *)
      else aux (sqrt next_term) next_product                                                  (* W przeciwnym wypadku, wywolaj funkcje pomocnicza dla poprzedniego wyrazu = sqrt(nastepny wyraz) i aktualny iloczyn = nastepny iloczyn *)
    in
    aux (sqrt 0.5) (sqrt 0.5)                                                                 (* Wywolaj funkcje pomocnicza dla parametrow startowych poprzedni wyraz = sqrt(0.5) i aktualny iloczyn = sqrt(0.5) *)
  
(* 

  evaluatePi 1e-8;;

*)  
      