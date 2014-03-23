module type PriorityQueueSig =
  sig
    exception Empty

    type t         (* Abstract type of elements of queue. *)
    type queue     (* Abstract type of queue. *)

    val empty      : queue                 (* Return empty queue.      *)
    val is_empty   : queue  -> bool        (* Check if queue is empty. *)
    val insert     : queue  -> t -> queue  (* Insert item into queue.  *)
    val find_min   : queue  -> t           (* Return minimum element.  *)
    val delete_min : queue  -> queue       (* Delete minimum element.  *)
    val from_list  : t list -> queue       (* Convert list to queue.   *)
  end


module PriorityQueue : (PriorityQueueSig with type t = int) = 
  struct
    exception Empty


    type leftist_heap =
      | Leaf
      | Node of int * int * leftist_heap * leftist_heap

    
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
            if e1 < e2 then
              merge_heap_helper e1 h_left (merge_heaps h_right h2)
            else
              merge_heap_helper e2 g_left (merge_heaps h1 g_right)
    
    (* Don't really know why
     * I still need to specify
     * that type t = int
     *
     * Isn't that done above, in
    * ( ... type t), and therefore
    * able to be inferred?
    *
    * *)
    type t = int

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





let heap_sort xs =
  let rec heap_sort_helper q result =
    (* Apparently I can't match q with PriorityQueue.empty ... *)
    if PriorityQueue.is_empty q then
      result
    else
      heap_sort_helper (PriorityQueue.delete_min q) 
        ((PriorityQueue.find_min q) :: result)
  in
  let q = PriorityQueue.from_list xs in
  List.rev @@ heap_sort_helper q []
;;


heap_sort [];;
    heap_sort [1;2;3;4;5;6;7;8;7;1;2;4;5;1;2;1;4;2;1];;
    heap_sort [11;12;91;99;72;62;39;36;96;62;38;100;60;36;67];;
