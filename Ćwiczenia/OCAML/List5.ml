(* LISTA 5 *)

(* 

#use "/Users/alexrogozinski/visualStudioCode/OCAML/Ćwiczenia/OCAML/List5.ml";;

*)

(* Definicja typu listy leniwej: *)
type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t

(* Metody pomocnicze do testowania: *)
let rec lfrom n = LCons (n, lazy (lfrom (n + 1)))

let rec ltake n lxs =
  match (n, lxs) with
  | (n, _) when n < 0 -> failwith("Nie mozna wziac ujemnej liczby elementow z listy.")
  | (0, _) -> []
  | (_, LNil) -> []
  | (_, LCons (x, lazy xs)) -> x :: ltake (n - 1) xs


(*  --  ZADANIE 1 -------------------------------------------------------------------------------- *)

let rec lrepeat k lxs =
  if (k <= 0) then failwith("Nie mozna powtorzyc elementu mniej niz jeden raz.")                      (* Jesli liczba powtorzen jest mniejsza lub rowna zero to rzuc wyjatek *)
  else                                                                                                (* W przeciwnym wypadku przejdz do wykonania reszty funkcji *)
  let rec helperFunc counter curr_elem result =                                                       (* Deklaracja funkcji pomocniczej *)
    if (counter = 0) then lrepeat k result                                                            (* Jesli licznik powtorzen jest rowny zero, wywolaj funkcje dla aktualnej listy wynikowej *)
    else LCons (curr_elem, lazy (helperFunc (counter - 1) curr_elem result))                          (* W przeciwnym wypadku utworz element listy leniwej i wywolaj funkcje pomocnicza dla licznika zmniejszonego o jeden i aktualnej listy wynikowej *)
  in  
  match lxs with                                                                                      (* Dopasuj wzorzec zadanej listy leniwej *)
  | LNil -> LNil                                                                                      (* Jesli jest to lista pusta to zwroc liste pusta *)
  | LCons (x, lazy ltail) -> helperFunc k x ltail;;                                                   (* Jesli nie jest to lista pusta to wywolaj funkcje pomocnicza dla ogona tej listy *)


ltake 15 (lrepeat 3 (lfrom 1))


(*  --  ZADANIE 2 -------------------------------------------------------------------------------- *)

let rec lfib =
  let rec fib a b = LCons (a, lazy (fib b (a + b)))
  in fib 1 1;;

ltake 15 lfib  


(*  --  ZADANIE 3 -------------------------------------------------------------------------------- *)

(* Definicja polimorficznego leniwego drzewa binarnego *)
type 'a lBT = LEmpty | LNode of 'a * (unit -> 'a lBT) * (unit -> 'a lBT)

(* Podpunkt a): *)
let rec lBreadth ltree =
  let rec helperFunc queue =
    match queue with
    | [] -> LNil                                                                                      (* Jesli kolejka jest pusta to zwrocenie pustej listy leniwej *)
    | LEmpty :: rest -> helperFunc rest                                                               (* Jesli w drzewie jest pusty wezel, to jest on pomijany *)
    | LNode (x, left, right) :: rest ->                                                               (* Jesli w drzewie jest wezel z wartoscia x, to *)
      LCons (x, lazy (helperFunc (rest @ [left (); right()])))                                        (* Tworzona jest nowa lista leniwa z glowa x i przetwarza kolejke do ktorej dodaje lewe i prawe poddrzewo tego wezla *)
    in helperFunc [ltree];;    

(* Podpunkt b): *)
let rec lTree n = LNode (n, (fun () -> lTree (2 * n)), (fun () -> lTree (2 * n + 1)));;


ltake 20 (lBreadth (lTree 1))