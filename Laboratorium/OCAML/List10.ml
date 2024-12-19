(* LISTA 10 *)

(*  

  #use "/Users/alexrogozinski/visualStudioCode/OCAML/Laboratorium/OCAML/List10.ml";;

*)

(* -- ZADANIE 1 --------------------------------------------------------------------------- *)

(* Implementacja w stylu funkcyjnym *)
let findMinFunc matrix =
  let rows = Array.length matrix in                                                           (* Liczba wierszy w macierzy *)                   
  let cols = Array.length matrix.(0) in                                                       (* Liczba kolumn w macierzy *)

  let findMinInCol col_index =                                          
    let rec aux row_index current_min =
      if (row_index >= rows) then current_min                                                 (* Jeśli indeks wiersza jest >= od liczby wierszy, zwroc aktualny wynik *)
      else aux (row_index + 1) (min current_min matrix.(row_index).(col_index))               (* W przeciwnym wypadku, wywołaj funkcję dla kolejnego wiersza *)
    in
   
    aux 0 max_int
  in
  
  Array.init cols findMinInCol                                                                (* Zwroc wynikową tablicę o długości rownej ilości kolumn zadanej macierzy *)



(* Implementacja w stylu imperatywnym *)
let findMinImper matrix =
  let rows = Array.length matrix in                                                           (* Liczba wierszy w macierzy *)                   
  let cols = Array.length matrix.(0) in                                                       (* Liczba kolumn w macierzy *)

  let result = Array.make cols 0 in                                                           (* Inicjalizacja wynikowej tablicy zerami *)

  for i = 0 to cols - 1 do                                                                    (* Iteracja po kolumnach *)
    let current_min = ref max_int in
    
    for j = 0 to rows - 1 do                                                                  (* Iteracja po wieszach *)
      current_min := (min !current_min matrix.(j).(i))
    done;

    result.(i) <- !current_min
  done;

  result



(* Przykładowa macierz prostokątna *)
let example_3d_array = [|
  [|23; 42; 12; -19; 20|];
  [|1; 5; 4; 7; 7|];
  [|51; 31; -2; -2; 68|];
  [|76; 21; 2; 2; 0|]
|]  


(* 

  findMinFunc example_3d_array;;
  findMinImper example_3d_array;;

*)
