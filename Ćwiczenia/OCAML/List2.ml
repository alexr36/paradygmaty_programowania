(* #use "/Users/alexrogozinski/visualStudioCode/OCAML/Cw2/List2.ml";; *)

(* -- ZADANIE 2   ---------------------------------------------------------------------------------------------------------- *)

(* Wersja "z definicji" *)
let rec fib n =
  match n with
  | n when (n < 0) -> raise (Failure "Nie mozna obliczyc wyrazu ciagu o pozycji mniejszej niz zero.")
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)


(* Wersja z rekurencją ogonową *)
let fibTail n =
  let rec helperFunc n previousTerm currentTerm =
    match n with
    | n when (n < 0) -> raise (Failure "Nie mozna obliczyc wyrazu ciagu o pozycji mniejszej niz zero.")
    | 0 -> previousTerm
    | n -> helperFunc (n - 1) currentTerm (currentTerm + previousTerm)
  in
  helperFunc n 0 1

(* TESTY 

fib 42;;          - zlozonosc obliczeniowa: O(2^n)
fibTail 42;;      - zlozonosc obliczeniowa: O(n)

*)

(* -- ZADANIE 3   ---------------------------------------------------------------------------------------------------------- *)

let rec root3 a =
  let epsilon = 1e-15 in
  let rec helperFunc x =
    if (abs_float (x ** 3. -. a) <= epsilon *. abs_float a) then x
    else 
      let x_next = x +. (a /. (x ** 2.) -. x) /. 3. in
      helperFunc x_next
  in
  let x0 = 
    if (a > 1.) then a /. 3. 
    else a
  in
  helperFunc x0  


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
  match (xs, ys) with
  | ([], _) -> true
  | (_, []) -> false
  | (xs_head :: xs_tail, ys_head :: ys_tail) -> xs_head = ys_head && initSegment (xs_tail, ys_tail)


(* TESTY

initSegment ([1; 2], [1; 2; 3; 4]);;        - zlozonosc obliczeniowa: O(min(m, n)); m, n - dlugosci list
initSegment ([1; 3], [1; 2; 3; 4]);;

*)  

(* -- ZADANIE 6   ---------------------------------------------------------------------------------------------------------- *)

let rec replaceNth (list, n, a) =
  match list with
  | [] -> []
  | _ :: listTail when (n = 0) -> a :: listTail
  | listHead :: listTail -> listHead :: replaceNth (listTail, n - 1, a)

(* TESTY

replaceNth (['o'; 'l'; 'a'; 'm'; 'a'; 'k'; 'o'; 't'; 'a'], 1, 's');;          - zlozonosc obliczeniowa: O(n)
replaceNth (['o'; 'l'; 'a'; 'm'; 'a'; 'k'; 'o'; 't'; 'a'], -4, 's');;

*)  
