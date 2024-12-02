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
let uncurry3Luk f x y z = f (x, y, z)

(* Bez lukru syntaktycznego *)
let uncurry3BezLuk = fun f -> fun x -> fun y -> fun z -> f (x, y, z)

(* 

  Nie jest określone jaki typ muszą przyjmowac ani zwracac funkcje, a zatem:
  f: ('a * 'b * 'c) -> 'd
  uncurry3Luk: ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd

*)

(* b) *)

(* Z lukrem syntaktycznym *)
let curry3Luk f (x, y, z) = f x y z

(* Bez lukru syntaktycznego *)
let curry3BezLuk = fun f -> fun (x, y, z) -> f x y z  

(* 

  Nie jest określone jaki typ muszą przyjmowac ani zwracac funkcje, a zatem:
  f: ('a -> 'b -> 'c ->) -> 'd 
  curry3Luk: ('a -> 'b -> 'c -> 'd) -> ('a * 'b * 'c) -> 'd

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
  match list with                                                                                                   (* Dopasowanie listy do wzorca *)
  | [] -> [x]                                                                                                       (* Jeśli lista jest pusta, zwróć jednoelementową listę zawierającą wstawiany element *)
  | listHead :: listTail ->                                                                                         (* Jeśli lista nie jest pusta, weź jej głowę *)
      if (compFunc x listHead) then x :: list                                                                       (* Jeśli wstawiany element jest mniejszy lub równy głowie listy, dodaj element do listy *)
      else listHead :: insert compFunc x listTail                                                                   (* W przeciwnym wypadku dodaj głowę listy do listy wynikowej wywołania funkcji dla wstawianego elementu i ogona pierwotnej listy *)


let rec insertionsort compFunc list =
  match list with                                                                                                   (* Dopasowanie listy do wzorca *)
  | [] -> []                                                                                                        (* Jeśli lista jest pusta, zwróć listę pustą *)
  | listHead :: listTail -> insert compFunc listHead (insertionsort compFunc listTail)                              (* Jeśli lista nie jest pusta, weź jej głowę i umieść na odpowiednim miejscu w liście otrzymanej z wywołania funkcji dla ogona pierwotnej listy *)

(* 

insertionsort compare [6; 7; 21; 1; 8; 24; 51; 2; 2];;

*)

(* Funkcja pomocnicza do scalania list *)
let rec merge compFunc listA listB =
  match (listA, listB) with                                                                                         (* Dopasowanie pary list do wzorca *)
  | ([], _) -> listB                                                                                                (* Jeśli lewa lista jest pusta, zwróć prawą *)
  | (_, []) -> listA                                                                                                (* Jeśli prawa lista jest pusta, zwróć lewą *)
  | (listHeadA :: listTailA, listHeadB :: listTailB) ->                                                             (* Jeśli listy nie są puste, weź ich głowy *)
      if (compFunc listHeadA listHeadB) then listHeadA :: merge compFunc listTailA listB                            (* Jeśli głowa listy pierwszej jest <= głowie listy drugiej, dodaj głowę listy pierwszej do listy wynikowej wywołania funkcji dla ogona listy pierwszej i pierwotnej listy drugiej *)
      else listHeadB :: merge compFunc listTailB listA                                                              (* W przeciwnym wypadku, odwrotnie *)


(* Funkcja pomocnicza do dzielenia list *)  
let rec divide list =
  match list with                                                                                                   (* Dopasowanie listy do wzorca *)
  | [] -> ([], [])                                                                                                  (* Jeśli lista jest pusta, zwróć parę list pustych *)
  | [x] -> ([x], [])                                                                                                (* Jeśli lista jest jednoelementowa, niech będzie pierwszą listą w parze *)
  | listHead1 :: listHead2 :: listTail ->                                                                           (* Jeśli lista nie jest pusta, weź dwa jej pierwsze elementy *)
      let (leftList, rightList) = divide listTail in                                                                (* Niech para list (leftList, rightList) będzie wynikiem podziału listy z wywołania funkcji dla ogona pierwotnej listy *)
      (listHead1 :: leftList, listHead2 :: rightList)                                                               (* Dodaj pierwszy element do leftList i drugi do rightList *)


let rec mergesort compFunc list 
  match list with                                                                                                   (* Dopasowanie listy do wzorca *)
  | [] -> []                                                                                                        (* Jeśli lista jest pusta, zwróć listę pustą *)
  | [_] -> list                                                                                                     (* Jeśli lista jest jednoelementowa, zwróć tę listę *)
  | _ ->                                                                                                            (* Jeśli lista ma więcej niż 1 element: *)
      let (leftList, rightList) = divide list in                                                                    (* - Niech para list (leftList, rightList) będzie wynikiem podziału listy z wywołania funkcji dzielenia list *)
      merge compFunc (mergesort compFunc leftList) (mergesort compFunc rightList)                                   (* - Wywołanie funkcji łączenia list dla list wynikowych wywołań funkcji sortowania dla leftList i rightList *)

(* 

mergesort compare [6; 7; 21; 1; 8; 24; 51; 2; 2];;

*)      
