(* #use "/Users/alexrogozinski/visualStudioCode/OCAML/Cw1/List1.ml";; *)

(*  --  ZADANIE 1 ------------------------------------------------------------------------------------------------------------ *)

let rec flatten1 xss =                                                                                                         
  if (xss = []) then raise (Failure "Podana lista jest pusta.")                                                                  (* Jeśli lista list jest pusta, rzuć wyjątek *)
  else if (List.tl xss = []) then List.hd xss                                                                                    (* Jeśli ogon listy list jest pusty, zwróć głowę tej listy *) 
  else (List.hd xss) @ flatten1 (List.tl xss)                                                                                    (* Jeśli ogon listy nie jest pusty, dodaj listę będącą głową listy list do listy wynikowej wywołania funkcji dla ogona listy list *)

(* TESTY

flatten1 [[5; 6]; [1; 2; 3]];;
flatten1 [[5; 6]];;
flatten1 [];;

*)  

(*  --  ZADANIE 2 ------------------------------------------------------------------------------------------------------------ *)

let rec count (x, xs) =
  if (xs = []) then 0                                                                                                            (* Jeśli lista jest pusta, to zwróć 0 *)
  else if (List.hd xs = x) then 1 + count (x, (List.tl xs))                                                                      (* Jeśli głowa listy jest równa wartości, której liczbę powtórzeń liczymy, dodaj 1 do wywołania funkcji dla ogona listy *)
  else count (x, (List.tl xs))                                                                                                   (* Jeśli głowa listy nie jest równa tej wartości, wowołaj funkcję dla ogona listy *)


(* TESTY

count ('a', ['a'; 'l'; 'a']);; 
count (5, [5; 5; 1; 1; 2; 5; 6]);;
count (1, []);;

*)  

(*  --  ZADANIE 1 ------------------------------------------------------------------------------------------------------------ *)

let rec replicate (x, n) =
  if (n < 0) then raise (Failure "Element nie moze byc powtorzony ujemna ilosc razy.")                                           (* Jeśli n jest mniejsze od 0, rzuć wyjątek *)
  else if (n = 0) then []                                                                                                        (* Jeśli n jest równe 0, to zwróć listę pustą *)
  else x :: replicate (x, n - 1)                                                                                                 (* Jeśli n jest większe od 0, dodaj powtarzany element do wyniku wywołania funkcji dla (n - 1) *)
  
(*  TESTY

replicate ("la", 3);;
replicate(5, 15);;
replicate('s', 0);;
replicate(1000, -4);;

*)

(*  --  ZADANIE 4 ------------------------------------------------------------------------------------------------------------ *)

(* Jako metoda *)
let rec sqrListMet xs =  
  if (xs = []) then []                                                                                                           (* Jeśli lista jest pusta, zwróć listę pustą *)
  else (List.hd xs * List.hd xs) :: sqrListMet (List.tl xs)                                                                      (* W przeciwnym wypadku, dodaj kwadrat głowy listy do listy wynikowej wywołania funkcji dla ogona listy *)

(* Jako funkcja *)  
let sqrListFun = fun xs ->                                                                                                       (* Analogicznie jak wyżej, ale z funkcją pomocniczą wewnątrz *)
  let rec helper xs =
    if (xs = []) then []
    else (List.hd xs * List.hd xs) :: helper (List.tl xs) in
  helper xs  

(* TESTY

sqrListMet [1; 2; 3; -4];;
sqrListMet [6; 4; 9; 15];;
sqrListMet [];;

sqrListFun [1; 2; 3; -4];;
sqrListFun [6; 4; 9; 15];;
sqrListFun [];;

*)  

(*  --  ZADANIE 5 ------------------------------------------------------------------------------------------------------------ *)

let palindromeExperimental xs =
  xs = List.rev xs                                                                                                               (* Sprawdzenie, czy odwrócona lista jest równa samej sobie *)

(*  TESTY

palindromeExperimental ['a'; 'l'; 'a'];;
palindromeExperimental [1; 2; 3; 4; 5];;
palindromeExperimental ["Jestem"; "Palindromem"; "Jestem"];;
palindromeExperimental [585];;
palindromeExperimental [];;

*)  

(*  --  ZADANIE 6 ------------------------------------------------------------------------------------------------------------ *)

let rec listLength xs =
  if (xs = []) then 0                                                                                                            (* Jeśli lista jest pusta, zwróć 0 *)
  else 1 + listLength (List.tl xs)                                                                                               (* W przeciwnym wypadku, dodaj 1 do wyniku wywołania funkcji dla ogona listy *)

(*  TESTY --  #use "/Users/alexrogozinski/visualStudioCode/OCAML/Cw1/Cw1_6.ml";;

listLength [1; 2; 3; 4; 5];;
listLength ["Xx"; "Yy"; "Zz"];;
listLength [];;

*)  
