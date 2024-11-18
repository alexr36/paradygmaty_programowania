(* LISTA 3 *)

(* 

#use "/Users/alexrogozinski/visualStudioCode/OCAML/Cw3/List3.ml";;

*)

(*  --  ZADANIE 1   --------------------------------------------------------------------------------------------  *)

(* 

a) let f1 x - x 2 2

   x - musi byc funkcją, ktora przyjmuje w argumenty typu int
   a zatem typy funkcji x:  int -> int -> 'a (nie wiadomo jaki typ jest zwracany)
   finalnie, typy funkcji f1: (int -> int -> 'a) -> 'a

b) let f2 x y z = x (y ^ z)

    y i z muszą byc typu string, poniewaz wystepuje miedzy nimi dzialanie operatora ^
    x musi byc funkcja przyjmujaca argument typu string, czyli
    x:  string -> 'a (niewiadomo jaki typ zwraca)
    finalnie, typy funkcji f2: (string -> 'a) -> string -> string -> 'a

*)

(*  --  ZADANIE 2   --------------------------------------------------------------------------------------------  *)

(* a) *)

(* Z lukrem syntaktycznym *)
let curry3Luk f x y z = f (x, y, z)

(* Bez lukru syntaktycznego *)
let curry3BezLuk = fun f -> fun x -> fun y -> fun z -> f (x, y, z)

(* 

  Nie jest określone jaki typ muszą przyjmowac ani zwracac funkcje, a zatem:
  f: ('a * 'b * 'c) -> 'd
  curry3Luk: ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd

*)

(* b) *)

(* Z lukrem syntaktycznym *)
let uncurry3Luk f (x, y, z) = f x y z

(* Bez lukru syntaktycznego *)
let uncurry3BezLuk = fun f -> fun (x, y, z) -> f x y z  

(* 

  Nie jest określone jaki typ muszą przyjmowac ani zwracac funkcje, a zatem:
  f: ('a -> 'b -> 'c ->) -> 'd 
  uncurry3Luk: ('a -> 'b -> 'c -> 'd) -> ('a * 'b * 'c) -> 'd

*)

(*  --  ZADANIE 3   --------------------------------------------------------------------------------------------  *)

let sumProd xs =
  List.fold_left (fun (sum, product) x -> (sum + x, product * x)) (0, 1) xs

(*  --  ZADANIE 4   --------------------------------------------------------------------------------------------  *)

(*

Pierwsza wersja nie działa poprawnie, ponieważ funkcja List.filter w obu przypadkach używa List.hd xs 
jako pivota, co oznacza, że pivot jest ponownie używany po filtrowaniu, 
co prowadzi do problemów z wydajnością i poprawnością (mogą pojawić się elementy zduplikowane lub pominięte).
Druga wersja działa lepiej, ponieważ używa x::xs do przypisania pivotowi zmiennej x, 
ale nadal istnieje problem, ponieważ używa fun y -> y > x, 
co powoduje brak porównania y == x, czyli niespełnienie warunku stabilności.

*)

(*  --  ZADANIE 5   --------------------------------------------------------------------------------------------  *)

(* Funkcja do porownywania *)
let compare x y = x <= y

(* Funkcja pomocnicza do wstawiania elementu na odpowiednie miejsce *)
let rec insert compFunc x list =
  match list with
  | [] -> [x]
  | listHead :: listTail ->
      if (compFunc x listHead) then x :: list
      else listHead :: insert compFunc x listTail 

let rec insertionsort compFunc list =
  match list with
  | [] -> []
  | listHead :: listTail -> insert compFunc listHead (insertionsort compFunc listTail)

(* 

insertionsort compare [6; 7; 21; 1; 8; 24; 51; 2; 2];;

*)

(* Funkcja pomocnicza do scalania list *)
let rec merge compFunc listA listB =
  match listA, listB with
  | ([], _) -> listB
  | (_, []) -> listA
  | (listHeadA :: listTailA, listHeadB :: listTailB) ->
      if (compFunc listHeadA listHeadB) then listHeadA :: merge compFunc listTailA listB
      else listHeadB :: merge compFunc listTailB listA  

(* Funkcja pomocnicza do dzielenia list *)  
let rec divide list =
  match list with
  | [] -> ([], [])
  | [x] -> ([x], [])
  | listHead1 :: listHead2 :: listTail ->
      let (leftList, rightList) = divide listTail in
      (listHead1 :: leftList, listHead2 :: rightList)

let rec mergesort compFunc list =
  match list with
  | [] -> []
  | [_] -> list
  | _ ->
      let (leftList, rightList) = divide list in
      merge compFunc (mergesort compFunc leftList) (mergesort compFunc rightList)

(* 

mergesort compare [6; 7; 21; 1; 8; 24; 51; 2; 2];;

*)      
