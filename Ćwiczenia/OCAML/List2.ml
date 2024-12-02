(* LISTA 2 *)

(* #use "/Users/alexrogozinski/visualStudioCode/OCAML/Cw2/List2.ml";; *)

(* -- ZADANIE 2   ---------------------------------------------------------------------------------------------------------- *)

(* Wersja "z definicji" *)
let rec fib n =
  match n with                                                                                                                    (* Dopasowanie wartości n *)
  | n when (n < 0) -> raise (Failure "Nie mozna obliczyc wyrazu ciagu o pozycji mniejszej niz zero.")                             (* Jeśli mniejsze niż 0, rzuć wyjątek *)
  | 0 -> 0                                                                                                                        (* Jeśli równe 0, zwróć 0 *)
  | 1 -> 1                                                                                                                        (* Jeśli równe 1, zwróć 1 *)
  | n -> fib (n - 1) + fib (n - 2)                                                                                                (* Jeśli większe od 1, oblicz kolejny (poprzedni) wyraz ciągu *)


(* Wersja z rekurencją ogonową *)
let fibTail n =
  let rec helperFunc n previousTerm currentTerm =                                                                                 (* Funkcja pomocnicza *)
    match n with                                                                                                                  (* Dopasowanie wartości n *)
    | n when (n < 0) -> raise (Failure "Nie mozna obliczyc wyrazu ciagu o pozycji mniejszej niz zero.")                           (* Jeśli mniejsze niż 0, rzuć wyjątek *)
    | 0 -> previousTerm                                                                                                           (* Jeśli równe 0, zwróć poprzedni wyraz ciągu *)
    | n -> helperFunc (n - 1) currentTerm (currentTerm + previousTerm)                                                            (* Jeśli większe od 0, wywołaj funkcję pomocniczą i oblicz następny wyraz ciągu; zastąp poprzedni wyraz aktualnym wyrazem *)
  in
  helperFunc n 0 1                                                                                                                (* Wywołaj funkcję wewnętrzną dla wartości n, poprzedniego wyrazu = 0 i aktualnego wyrazu = 1 *)

(* TESTY 

fib 42;;          - zlozonosc obliczeniowa: O(2^n)
fibTail 42;;      - zlozonosc obliczeniowa: O(n)

*)

(* -- ZADANIE 3   ---------------------------------------------------------------------------------------------------------- *)

let rec root3 a =
  let epsilon = 1e-15 in                                                                                                         (* Deklaracja i definicja stałej epsilon (dokładności) *)
  let rec helperFunc x =                                                                                                         (* Funkcja pomocnicza *)
    if (abs_float (x ** 3. -. a) <= epsilon *. abs_float a) then x                                                               (* Jeśli wartość |x^3 - a| jest mniejsza lub równa wartości epsilon * |a|, to zwróć x *)
    else                                                                                                                         (* W przeciwnym wypadku: *)
      let x_next = x +. (a /. (x ** 2.) -. x) /. 3. in                                                                           (* Niech x_next będzie następną wartością x, obliczoną na podstawie wzoru: x + (a / x^2 - x) / 3 *)
      helperFunc x_next                                                                                                          (* Wywołanie funkcji pomocniczej dla x_next *)
  in
  let x0 =                                                                                                                       (* Deklaracja wartości początkowej dla x *)
    if (a > 1.) then a /. 3.                                                                                                     (* Jeśli a > 1, to x = a / 3 *)
    else a                                                                                                                       (* W przeciwynym wypadku, x = a *)
  in
  helperFunc x0                                                                                                                  (* Wywołanie funkcji pomocniczej dla wartości początkowej x *)


(* TESTY 
  
root3 27.;;       - zlozonosc obliczeniowa: O(log(1/epsilon))
root3 8.;;
  
*)

(* -- ZADANIE 4   ---------------------------------------------------------------------------------------------------------- *)

let matchCaseOne list =
  match list with
  | [_; _; x; _; _] when (x = 0) -> "Dopasowano wzorzec z x = 0."
  | _ -> "Nie dopasowano wzorca."


let matchCaseTwo list =
  match list with
  | [(_, _); (x, _)] when (x = 0) -> "Dopasowano wzorzec z x = 0."
  | _ -> "Nie dopasowano wzorca."


(* TESTY

matchCaseOne [-2; -1; 0; 1; 2];;        - zlozonosc obliczeniowa: O(1)
matchCaseOne [1; 2; 3; 4];;
matchCaseTwo [(1,2); (0,1)];;           - zlozonosc obliczeniowa: O(1)
matchCaseTwo [(1, 3); (4, 0)];;

*)  

(* -- ZADANIE 5   ---------------------------------------------------------------------------------------------------------- *)

let rec initSegment (xs, ys) =
  match (xs, ys) with                                                                                                          (* Dopasowanie wartości list xs i ys *)
  | ([], _) -> true                                                                                                            (* Jeśli xs jest puste, zwróć true *)
  | (_, []) -> false                                                                                                           (* Jeśli ys jest puste, zwróć false *)
  | (xs_head :: xs_tail, ys_head :: ys_tail) -> xs_head = ys_head && initSegment (xs_tail, ys_tail)                            (* Jeśli obie listy mają elementy, weź głowy list i porównaj je, wywołaj funkcję dla ogonów tych list *)


(* TESTY

initSegment ([1; 2], [1; 2; 3; 4]);;        - zlozonosc obliczeniowa: O(min(m, n)); m, n - dlugosci list
initSegment ([1; 3], [1; 2; 3; 4]);;

*)  

(* -- ZADANIE 6   ---------------------------------------------------------------------------------------------------------- *)

let rec replaceNth (list, n, a) =
  match list with                                                                                                              (* Dopasowanie wartości listy *)
  | [] -> []                                                                                                                   (* Jeśli jest pusta, zwróć listę pustą *)
  | _ :: listTail when (n = 0) -> a :: listTail                                                                                (* Jeśli ma więcej niż 1 element a licznik n = 0, dodaj element a do ogona listy, pomijając jej głowę (zastępując) *)
  | listHead :: listTail -> listHead :: replaceNth (listTail, n - 1, a)                                                        (* Jeśli licznik n jest > 0, weź głowę aktualnej listy i dodaj ją do listy wynikowej, otrzymanej z wywołania funkcji dla zmniejszonego licznika n *)

(* TESTY

replaceNth (['o'; 'l'; 'a'; 'm'; 'a'; 'k'; 'o'; 't'; 'a'], 1, 's');;          - zlozonosc obliczeniowa: O(n)
replaceNth (['o'; 'l'; 'a'; 'm'; 'a'; 'k'; 'o'; 't'; 'a'], -4, 's');;

*)  
