(* LISTA 1 *)

(*

  #use "/Users/alexrogozinski/visualStudioCode/OCAML/Lab/List1/List1.ml";;

*)

(*  --  ZADANIE 1 ----------------------------------------------------------------------- *)

let buildTraingle a b c =
  if (a <= 0.0 || b <= 0.0 || c <= 0.0) then 
    raise (Failure "Nie mozna skonstruowac trokjata o ujmenych dlugosciach bokow.")         (* Jeśli ktorykolwiek z bokow jest <= 0, rzuc wyjątek *)
  else if (abs_float (b -. c) < a && a < b +. c) then                                       (* Jeśli warunek |b - c| < a < b + c jest spełniony: *)
    let p = (a +. b +. c) /. 2.0 in                                                         (* Niech p = (a + b + c) / 2 *)
    sqrt (p *. (p -. a) *. (p -. b) *. (p -. c))                                            (* Oblicz i zwroc pole trojkąta ze wzoru Herona *)
  else raise (Failure "Nie mozna zbudowac trojkata.")                                       (* W przeciwnym wypadku, rzuc wyjątek *)
    

(* 
  buildTraingle 2. 3. 4.;;
  buildTraingle 8. 2. 1.;;
  buildTraingle (-1.) 5. 0.;;
*)  
 
    
(*  --  ZADANIE 2 ----------------------------------------------------------------------- *)

let calculateSum n x =
  if (n <= 0) then 
    raise (Failure "Nie mozna obliczyc wyrazu ciagu o indeksie mniejszym lub rownym 0.")
  else 
    let rec aux i current_sum current_term =                                                (* Funkcja pomocnicza *)
      if (i > n) then current_sum                                                           (* Jeśli i > n, zwroc aktualną sume *)
      else                                                                                  (* W przeciwnym wypadku *)
        let next_term = current_term *. (-1.0) *. x /. float_of_int i in                    (* Oblicz następny wyraz ciagu ze wzoru (-1)^i x^i / i! *)
        let next_sum = current_sum +. next_term in                                          (* Zaktualizuj aktualną sume *)
        aux (i + 1) next_sum next_term                                                      (* Wywolaj funkcje pomocnicza dla zaktualizowanych parametrow: inkrementowane i, aktualna suma, nastepny wyraz *)
    in    
    aux 1 0. 1.                                                                             (* Wywolaj funkcje pomocnicza dla parametrow startowych: i = 1, aktualna suma = 0, aktualny wyraz = 1 *)


(* 
  calculateSum 5 2.;;
*)  


(*  --  ZADANIE 3 ----------------------------------------------------------------------- *)

let findNthTerm r =
  let rec aux n current_sum =                                                               (* Funkcja pomocnicza *)
    if (current_sum > r) then n                                                             (* Jeśli aktualna suma jest większa od zadanej wartości r, zwroc n *)
    else                                                                                    (* W przeciwnym wypadku: *)
      let next_term = 1. /. float_of_int n                                                  (* Oblicz następny wyraz ciągu ze wzoru 1 / i *)
      in
      aux (n + 1) (current_sum +. next_term)                                                (* Wywołaj funkcje pomocniczą dla inkrementowanego n oraz aktualnej sumy zwiększonej o nastęony wyraz ciągu *)
    in
    aux 1 0.                                                                                (* Wywołaj funkcje pomocniczą dla n = 1 oraz aktualnej sumy = 0 *)


(* 
  findNthTerm 2.;;
*)    


(*  --  ZADANIE 4 ----------------------------------------------------------------------- *)

let findMin list =
  let rec aux current_list current_min =                                                    (* Funkcja pomocnicza *)
    if (current_list = []) then current_min                                                 (* Jeśli aktualna lista jest pusta, zwroc aktualną najmniejszą wartośc *)
    else                                                                                    (* W przeciwnym wypadku: *)
      let current_element = List.hd current_list in                                         (* Niech aktualnie rozpatrywany element to głowa aktualnej listy *)
      let current_tail = List.tl current_list in                                            (* Niech aktualny ogon listy to ogon aktualnej listy (oczywiste) *)
      if (current_element < current_min) then                                               (* Jeśli głowa aktualnej listy jest mniejsza niz aktualna najmniejsza wartośc: *)
        aux current_tail current_element                                                    (* Wywołaj funkcje pomocniczą dla ogona aktualnej listy oraz głowy aktualnej listy jako najmniejszy element *)
      else aux current_tail current_min                                                     (* W przciwnym wypadku, wywołaj funkcje pomocniczą dla ogona aktualnej listy i aktualnej najmniejszej wartości *)
  in
  aux list (float_of_int max_int)                                                           (* Wywołanie funkcji pomocniczej dla zadanej listy i aktualnej najmniejszej wartości = max_int *)


(*
  findMin [5.; 1.; (-24.); 623.];;
*)


(*  --  ZADANIE 5 ----------------------------------------------------------------------- *)

let divide list =
  let rec aux (less, more) current_list =                                                   (* Funkcja pomocnicza *)
    if (current_list = []) then (less, more)                                                (* Jeśli aktualna lista jest pusta, zwroc pare list wynikowych *)
    else                                                                                    (* W przeciwnym wypadku: *)
      let current_elem = List.hd current_list in                                            (* Niech aktualnie rozpatrywany element to głowa aktualnej listy *)
      let current_tail = List.tl current_list in                                            (* Niech aktualny ogon listy to ogon aktualnej listy (oczywiste) *)
      if (current_elem < 0) then aux (current_elem :: less, more) current_tail              (* Jeśli aktualnie rozpatrywany element jest < 0, dodaj go do listy wynikowej liczb mniejszych od 0 *)
      else aux (less, current_elem :: more) current_tail                                    (* W przeciwnym wypadku, dodaj go do listy wynikowej liczb wiekszych lub rownych 0 *)
  in
  aux ([], []) list                                                                         (* Wywołanie funkcji pomocniczej dla pary pustych list i zadanej listy jako listy aktualnej *)


(*
  divide [5; 1; (-24); 623];;
*)