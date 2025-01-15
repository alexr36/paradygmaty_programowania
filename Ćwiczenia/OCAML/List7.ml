(* LISTA 7 *)

(* 

#use "/Users/alexrogozinski/visualStudioCode/OCAML/Ćwiczenia/OCAML/List7.ml";;

*)

(*  --  ZADANIE 1 -------------------------------------------------------------------------------- *)

(* Sygnatura dla kolejek czysto funkcyjnych *)
module type QUEUE_FUN = 
sig
  type 'a t
  exception Empty of string
  val empty: unit -> 'a t
  val enqueue: 'a * 'a t -> 'a t
  val dequeue: 'a t -> 'a t
  val first: 'a t -> 'a
  val isEmpty: 'a t -> bool
end


(* Podpunkt a) *)
module QueueList : QUEUE_FUN =
struct
  type 'a t = 'a list

  exception Empty of string
    
  let empty() = []

  let enqueue(elem, queue) = queue @ [elem]

  let dequeue queue = 
    match queue with
    | [] -> raise (Empty "The queue is empty.")
    | _ :: tail -> tail

  let first queue = 
    match queue with
    | [] -> raise (Empty "The queue is empty.")
    | head :: _ -> head

  let isEmpty queue = (queue = [])
end


(* Podpunkt b) *)
module QueuePair : QUEUE_FUN =
struct
  type 'a t = 'a list * 'a list
  
  exception Empty of string

  let empty() = ([], [])


  (* Zwraca pierwszą z list w parze *)
  let getFront queue =
    match queue with
    | (front, _) -> front


  (* Zwraca drugą z list w parze *)
  let getRear queue =
    match queue with
    | (_, rear) -> rear  


  (* Funkcja normalizująca postac kolejki *) 
  let normalize(front, rear) =
    match front with
    | [] -> (List.rev rear, [])
    | _ -> (front, rear)


  let enqueue(elem, queue) = 
    normalize(getFront queue, elem :: getRear queue)

  let dequeue queue =
    match queue with
    | (_ :: tail, rear) -> normalize(tail, rear)
    | ([], _) -> raise(Empty "The queue is empty.")

  let first queue = 
    match queue with
    | (head :: _, _) -> head
    | ([], _) -> raise(Empty "The queue is empty.")
     
  let isEmpty queue =
    getFront queue = [] && getRear queue = []
end


(* --  TESTY ------------------------------------------------------------------------------------- *)

(* Dla implementacji z pojedyncza lista *)
let runList() =
  Printf.printf "===== Tests for QueueList =====\n";
  let module Q = QueueList in
  let q = Q.empty () in
  let q = Q.enqueue (1, q) in
  let q = Q.enqueue (2, q) in
  let q = Q.enqueue (3, q) in
  Printf.printf "First element: %d\n" (Q.first q); (* Oczekiwane: 1 *)
  let q = Q.dequeue q in
  Printf.printf "First element after dequeue: %d\n" (Q.first q); (* Oczekiwane: 2 *)
  let q = Q.dequeue q in
  let q = Q.dequeue q in
  Printf.printf "Is empty: %b\n" (Q.isEmpty q); (* Oczekiwane: true *)
  try
    let _ = Q.first q in ()
  with Q.Empty msg -> Printf.printf "Exception: %s\n" msg


(* Dla implementacji z para list *)
let runPair() =
  Printf.printf "===== Tests for QueuePair =====\n";
  let module Q = QueuePair in
  let q = Q.empty () in
  let q = Q.enqueue (1, q) in
  let q = Q.enqueue (2, q) in
  let q = Q.enqueue (3, q) in
  Printf.printf "First element: %d\n" (Q.first q); (* Oczekiwane: 1 *)
  let q = Q.dequeue q in
  Printf.printf "First element after dequeue: %d\n" (Q.first q); (* Oczekiwane: 2 *)
  let q = Q.dequeue q in
  let q = Q.dequeue q in
  Printf.printf "Is empty: %b\n" (Q.isEmpty q); (* Oczekiwane: true *)
  try
    let _ = Q.first q in ()
  with Q.Empty msg -> Printf.printf "Exception: %s\n" msg


let runTask1() =
  runList();
  Printf.printf "\n";
  runPair();    


(*  --  ZADANIE 2 -------------------------------------------------------------------------------- *)

(* Sygnatura dla kolejek modyfikowalnych *)
module type QUEUE_MUT =
sig
  type 'a t
            (* The type of queues containing elements of type ['a]. *)
  exception Empty of string
            (* Raised when [first q] is applied to an empty queue [q]. *)
  exception Full of string
            (* Raised when [enqueue(x,q)] is applied to a full queue [q]. *)
  val empty: int -> 'a t
            (* [empty n] returns a new queue of length [n], initially empty. *)
  val enqueue: 'a * 'a t -> unit
            (* [enqueue (x,q)] adds the element [x] at the end of a queue [q]. *)
  val dequeue: 'a t -> unit
            (* [dequeue q] removes the first element in queue [q] *)
  val first: 'a t -> 'a
            (* [first q] returns the first element in queue [q] without removing
                it from the queue, or raises [Empty] if the queue is empty. *)
  val isEmpty: 'a t -> bool
            (* [isEmpty q] returns [true] if queue [q] is empty,
                otherwise returns [false]. *)
  val isFull: 'a t -> bool
            (* [isFull q] returns [true] if queue [q] is full,
                otherwise returns [false]. *)
end


module QueueCyclic : QUEUE_MUT =
struct
  type 'a t = {
    mutable arr : 'a option array;  (* Tablica jako podstawa implementacji *)
    mutable front : int;            (* Indeks pierwszego elementu w kolejce *)
    mutable rear : int;             (* Indeks ostatniego elementu w kolejce *)
    mutable size : int;             (* Aktualna ilośc elementow w kolejce *)
    capacity : int                  (* Maksymalny rozmiar kolejki *)
  }

  exception Empty of string

  exception Full of string

  let empty n = 
    if (n <= 0) then raise(Invalid_argument "Queue capacity must be grater than 0.");
    {
      arr = Array.make n None;
      front = 0;
      rear = 0;
      size = 0;
      capacity = n
    }


  let isEmpty queue = 
    queue.size = 0    
    
    
  let isFull queue =
    queue.size = queue.capacity  


  let enqueue(elem, queue) =
    if (isFull queue) then raise(Full "The queue is full.");
    queue.arr.(queue.rear) <- Some elem;
    queue.rear <- (queue.rear + 1) mod queue.capacity;
    queue.size <- queue.size + 1


  let dequeue queue =
    if (isEmpty queue) then raise(Empty "The queue is empty.");  
    queue.arr.(queue.front) <- None;
    queue.front <- (queue.front + 1) mod queue.capacity;
    queue.size <- queue.size - 1


  let first queue =
    if (isEmpty queue) then raise(Empty "The Queue is empty.");
    match queue.arr.(queue.front) with
    | Some x -> x
    | None -> failwith "Unexpected state: front element is None"
end


(* --  TESTY ------------------------------------------------------------------------------------- *)

let runTask2() =
  let module Q = QueueCyclic in
  let q = Q.empty 3 in
  Q.enqueue (1, q);
  Q.enqueue (2, q);
  Q.enqueue (3, q);
  Printf.printf "First element: %d\n" (Q.first q);  (* Oczekiwane: 1 *)
  Q.dequeue q;
  Printf.printf "First element after dequeue: %d\n" (Q.first q);  (* Oczekiwane: 2 *)
  Q.dequeue q;
  Q.dequeue q;
  Printf.printf "Is empty: %b\n" (Q.isEmpty q);  (* Oczekiwane: true *)
  try
    let _ = Q.first q in ()
  with Q.Empty msg -> Printf.printf "Exception: %s\n" msg
