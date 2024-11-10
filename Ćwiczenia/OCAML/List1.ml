(* #use "/Users/alexrogozinski/visualStudioCode/OCAML/Cw1/List1.ml";; *)

(*  --  ZADANIE 1 ------------------------------------------------------------------------------------------------------------ *)

let rec flatten1 xss = 
  if (xss = []) then raise (Failure "Podana lista jest pusta.")
  else if (List.tl xss = []) then List.hd xss
  else (List.hd xss) @ flatten1 (List.tl xss)

(* TESTY

flatten1 [[5; 6]; [1; 2; 3]];;
flatten1 [[5; 6]];;
flatten1 [];;

*)  

(*  --  ZADANIE 2 ------------------------------------------------------------------------------------------------------------ *)

let rec count (x, xs) =
  if (xs = []) then 0
  else if (List.hd xs = x) then 1 + count (x, (List.tl xs))
  else count (x, (List.tl xs))


(* TESTY

count ('a', ['a'; 'l'; 'a']);; 
count (5, [5; 5; 1; 1; 2; 5; 6]);;
count (1, []);;

*)  

(*  --  ZADANIE 1 ------------------------------------------------------------------------------------------------------------ *)

let rec replicate (x, n) =
  if (n < 0) then raise (Failure "Element nie moze byc powtorzony ujemna ilosc razy.")
  else if (n = 0) then []
  else x :: replicate (x, n - 1)
  
(*  TESTY

replicate ("la", 3);;
replicate(5, 15);;
replicate('s', 0);;
replicate(1000, -4);;

*)

(*  --  ZADANIE 4 ------------------------------------------------------------------------------------------------------------ *)

(* Jako metoda *)
let rec sqrListMet xs =
  if (xs = []) then []
  else (List.hd xs * List.hd xs) :: sqrListMet (List.tl xs) 

(* Jako funkcja *)  
let sqrListFun = fun xs ->
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

(* Osobiście zdefiniowana funkcja reverseList *)
let rec reverseList xs =
  if (xs = []) then []
  else reverseList (List.tl xs) @ [List.hd xs] 
  
(* Z osobiście zdefiniowaną funkcją reverseList *)  
let rec palindromeCustomRev xs =
  if (xs = [] || List.tl xs = []) then true
  else if (List.hd xs = List.hd (reverseList xs)) then palindromeCustomRev (List.tl (reverseList (List.tl xs)))
  else false  

(* Z wbudowaną funkcją List.rev *)
let palindromeBuiltInRev xs =
  if (xs = [] || List.tl xs = []) then true
  else xs = List.rev xs  

(* Oryginalny pomysł *)  
let rec palindrome xs =
  if (xs = [] || List.tl xs = []) then true
  else if (List.hd xs = List.hd (List.rev xs)) then palindrome (List.tl (List.rev (List.tl xs)))
  else false  

let palindromeExperimental xs =
  xs = List.rev xs  

(*  TESTY

reverseList [1; 2; 3; 4; 5];;

palindrome ['a'; 'l'; 'a'];;
palindrome [1; 2; 3; 4; 5];;
palindrome ["Jestem"; "Palindromem"; "Jestem"];;
palindrome [585];;
palindrome [];;

palindromeCustomRev ['a'; 'l'; 'a'];;
palindromeCustomRev [1; 2; 3; 4; 5];;
palindromeCustomRev ["Jestem"; "Palindromem"; "Jestem"];;
palindromeCustomRev [585];;
palindromeCustomRev [];;

palindromeBuiltInRev ['a'; 'l'; 'a'];;
palindromeBuiltInRev [1; 2; 3; 4; 5];;
palindromeBuiltInRev ["Jestem"; "Palindromem"; "Jestem"];;
palindromeBuiltInRev [585];;
palindromeBuiltInRev [];;

palindromeExperimental ['a'; 'l'; 'a'];;
palindromeExperimental [1; 2; 3; 4; 5];;
palindromeExperimental ["Jestem"; "Palindromem"; "Jestem"];;
palindromeExperimental [585];;
palindromeExperimental [];;

*)  

(*  --  ZADANIE 6 ------------------------------------------------------------------------------------------------------------ *)

let rec listLength xs =
  if (xs = []) then 0
  else 1 + listLength (List.tl xs)

(*  TESTY --  #use "/Users/alexrogozinski/visualStudioCode/OCAML/Cw1/Cw1_6.ml";;

listLength [1; 2; 3; 4; 5];;
listLength ["Xx"; "Yy"; "Zz"];;
listLength [];;

*)  