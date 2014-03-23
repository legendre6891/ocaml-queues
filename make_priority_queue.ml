(* Type for ordered comparisons. *)
type comparison = LessThan | Equal | GreaterThan


(* Signature for ordered objects. *)
module type OrderedSig =
  sig
    type t
    val compare: t -> t -> comparison
  end


(* Signature for priority queues. *)
module type PriorityQueueSig =
  sig
    exception Empty

    type t
    type queue

    val empty      : queue
    val is_empty   : queue  -> bool
    val insert     : queue  -> t -> queue
    val find_min   : queue  -> t
    val delete_min : queue  -> queue
    val from_list  : t list -> queue
  end


module MakePriorityQueue (Elt : OrderedSig) 
  : (PriorityQueueSig with type t = Elt.t) =
  struct
    exception Empty
    type t = Elt.t (* Why is this necessary, given the above? *)

    type leftist_heap =
      | Leaf
      | Node of int * t * leftist_heap * leftist_heap

    
    let rec merge_heaps h1 h2 = 
      let rank_heap h1 = 
        match h1 with
        | Leaf -> 0
        | Node (rank, _, _, _) -> rank
      in
      let merge_heap_helper min_elem h1 h2 =
        let r1 = rank_heap h1 in
        let r2 = rank_heap h2 in

        (*How to do this in one-shot?*)
        let left = 
          if r1 >= r2 then
            h1
          else
            h2
        in
        let right = 
          if r1 < r2 then
            h1 
          else
            h2
        in
        let min_rank = min r1 r2 in
        Node ((min_rank + 1), min_elem, left, right)
      in
      match h1, h2 with
        | Leaf, _ -> h2
        | _, Leaf -> h1
        | Node (r1, e1, h_left, h_right), Node (r2, e2, g_left, g_right) ->
            match Elt.compare e1 e2 with
              | LessThan ->
                merge_heap_helper e1 h_left (merge_heaps h_right h2)
              | _ ->
                merge_heap_helper e2 g_left (merge_heaps h1 g_right)


    type queue = leftist_heap

    let empty = Leaf
  
    let is_empty q =
      match q with
        | Leaf -> true
        | _ -> false

    let insert q t = merge_heaps q (Node (1, t, Leaf, Leaf))

    let find_min q = 
      match q with
        | Leaf -> raise Empty
        | Node (_, e, _, _) -> e

    let delete_min q = 
      match q with
        | Leaf -> raise Empty
        | Node (_, _, left, right) -> merge_heaps left right

    let from_list xs = 
      let rec from_list_helper xs q =
        match xs with
          | [] -> q
          | h :: t -> from_list_helper t (insert q h)
      in
      from_list_helper xs empty
    
    
  end

module OrderedString =
  struct
    type t = string
    let compare x y = 
      if x = y then Equal else if x < y then LessThan else GreaterThan
  end

module StringPQ = MakePriorityQueue(OrderedString)

let heap_sort xs =
  let rec heap_sort_helper q result =
    (* Apparently I can't match q with PriorityQueue.empty ... *)
    if StringPQ.is_empty q then
      result
    else
      heap_sort_helper (StringPQ.delete_min q) 
        ((StringPQ.find_min q) :: result)
  in
  let q = StringPQ.from_list xs in
  List.rev @@ heap_sort_helper q []

;;

    heap_sort [];;
    heap_sort ["a"];;
    heap_sort ["teddy bear"; "Caltech"; "economics"; "computer science"];;
    heap_sort ["a"; "c"; "d"];;
