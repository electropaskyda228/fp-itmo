module type AVL_BAG = sig
  type 'a tree

  val empty : 'a tree
  val count: 'a -> 'a tree -> int
  val total_size: 'a tree -> int
  val unique_size: 'a tree -> int
  val contains: 'a -> 'a tree -> bool
  val insert: 'a -> 'a tree -> 'a tree
  val delete: 'a -> 'a tree -> 'a tree
  val map: ('a -> 'b) -> 'a tree -> 'b tree
  val filter: ('a -> bool) -> 'a tree -> 'a tree
  val union: 'a tree -> 'a tree -> 'a tree
  val intersection: 'a tree -> 'a tree -> 'a tree
  val difference: 'a tree -> 'a tree -> 'a tree
  val fold_left: ('acc -> 'a -> int -> 'acc) -> 'acc -> 'a tree -> 'acc
  val fold_right: ('a -> int -> 'acc -> 'acc) -> 'a tree -> 'acc -> 'acc
  val equal: 'a tree -> 'a tree -> bool
  val ( = ): 'a tree -> 'a tree -> bool
end

module AvlMultiset: AVL_BAG = struct
  (* declaration *)
  type 'a tree = 
    | Leaf 
    | Node of int * 'a * int * 'a tree * 'a tree

  let empty = Leaf
  let is_empty = function
    | Leaf -> true
    | _ -> false
  let height = function
    | Leaf -> 0
    | Node(h, _, _, _, _) -> h

  let create_node_with_count l v count r = 
    Node(1 + max (height l) (height r), v, count, l, r)

  let rec count x = function
    | Leaf -> 0
    | Node(_, v, cnt, l, r) ->
      if x < v then count x l
      else if x > v then count x r
      else cnt

  let rec total_size = function
    | Leaf -> 0
    | Node(_, _, count, l, r) -> count + (total_size l) + (total_size r)

  let rec unique_size = function
    | Leaf -> 0
    | Node(_, _, _, l, r) -> 1 + (unique_size l) + (unique_size r)

  let contains x tree = (count x tree) > 0

  let balance_factor = function
    | Leaf -> 0
    | Node(_, _, _, l, r) -> (height l) - (height r)

  (* rotation *)
  let rotate_right = function
    | Node(_, y, count_y, Node(_, x, count_x, a, b), c) -> 
        let new_c = create_node_with_count b y count_y c in
        create_node_with_count a x count_x new_c
    | t -> t

  let rotate_left = function
    | Node(_, y, count_y, a, Node(_, x, count_x, b, c)) ->
        let new_a = create_node_with_count a y count_y b in
        create_node_with_count new_a x count_x c
    | t -> t

  (* balance *)
  let balance = function
    | Leaf -> Leaf
    | Node(_, v, count, l, r) as node ->
        let bf = balance_factor node in
        if bf > 1 then
          if balance_factor l >= 0 then
            rotate_right node
          else 
            rotate_right (create_node_with_count (rotate_left l) v count r)
        else if bf < -1 then
          if balance_factor r <= 0 then
            rotate_left node
          else
            rotate_left (create_node_with_count l v count (rotate_right r)) 
        else
          node

  (* insert *)
  let rec insert v tree = 
    match tree with
    | Leaf -> Node(1, v, 1, Leaf, Leaf)
    | Node(h, v_curr, count, l, r) -> 
        if v < v_curr then 
          balance (create_node_with_count (insert v l) v_curr count r)
        else if v > v_curr then
          balance (create_node_with_count l v_curr count (insert v r))
        else 
          Node(h, v_curr, count + 1, l, r)

  (* delete *)
  let rec min_value = function
    | Leaf -> None
    | Node(_, v, count, Leaf, _) -> Some (v, count)
    | Node(_, _, _, l, _) -> min_value l

  let rec delete_min = function
    | Leaf -> Leaf
    | Node(h, v, count, Leaf, r) -> 
        if count > 1 then
          Node(h, v, count - 1, Leaf, r)
        else
          r
    | Node(_, v, count, l, r) -> 
        balance (create_node_with_count (delete_min l) v count r)

  let rec delete x tree = 
    match tree with
    | Leaf -> Leaf
    | Node(h, v, count, l, r) -> 
        if x < v then
          balance (create_node_with_count (delete x l) v count r)
        else if x > v then
          balance (create_node_with_count l v count (delete x r))
        else 
          if count > 1 then
            Node(h, v, count - 1, l, r)
          else
            match l, r with
            | Leaf, Leaf -> Leaf
            | Leaf, r -> r
            | l, Leaf -> l
            | l, r -> 
                match min_value r with
                | Some (min_val, min_count) -> 
                    let new_right = delete_min r in
                    balance (create_node_with_count l min_val min_count new_right)
                | None -> tree

  (* map *)
  let map f tree =
    let rec collect = function
      | Leaf -> []
      | Node(_, v, count, l, r) -> 
          let left_items = collect l in
          let current_items = List.init count (fun _ -> f v) in
          let right_items = collect r in
          left_items @ current_items @ right_items
    in
    
    let elements = collect tree in
    
    let rec group sorted_list =
      match sorted_list with
      | [] -> []
      | x :: xs ->
          let rec count_same cnt = function
            | y :: ys when y = x -> count_same (cnt + 1) ys
            | rest -> (x, cnt), rest
          in
          let (elem, cnt), rest = count_same 1 xs in
          (elem, cnt) :: group rest
    in
    
    let grouped = group (List.sort compare elements) in
    
    List.fold_left (fun acc (x, cnt) ->
      let rec insert_many t x cnt =
        if cnt = 0 then t
        else insert_many (insert x t) x (cnt - 1)
      in
      insert_many acc x cnt
    ) Leaf grouped

  (* filter *)
  let rec merge_avl t1 t2 = 
    match t1, t2 with
    | Leaf, t | t, Leaf -> t
    | Node(_, v, count, l, r), _ ->
        let dest_with_v = 
          let rec insert_many t x cnt =
            if cnt = 0 then t
            else insert_many (insert x t) x (cnt - 1)
          in
          insert_many t2 v count
        in
        let with_left = merge_avl l dest_with_v in
        merge_avl r with_left

  let rec filter p = function
    | Leaf -> Leaf
    | Node(_, v, count, l, r) -> 
        let left_filtered = filter p l in
        let right_filtered = filter p r in
        if p v then
          let rec insert_many t x cnt =
            if cnt = 0 then t
            else insert_many (insert x t) x (cnt - 1)
          in
          insert_many (merge_avl left_filtered right_filtered) v count
        else
          merge_avl left_filtered right_filtered

  (* multiset methods *)
  let union t1 t2 = merge_avl t1 t2

  let intersection t1 t2 =
    let rec aux t1 t2 =
      match t1, t2 with
      | Leaf, _ | _, Leaf -> Leaf
      | Node(_, v1, cnt1, l1, r1), _ ->
          let cnt_in_t2 = count v1 t2 in
          let new_cnt = min cnt1 cnt_in_t2 in
          if new_cnt > 0 then
            let left_inter = aux l1 t2 in
            let right_inter = aux r1 t2 in
            let rec insert_many t x cnt =
              if cnt = 0 then t
              else insert_many (insert x t) x (cnt - 1)
            in
            insert_many (merge_avl left_inter right_inter) v1 new_cnt
          else
            merge_avl (aux l1 t2) (aux r1 t2)
    in
    aux t1 t2

  let difference t1 t2 =
    let rec aux t1 t2 =
      match t1, t2 with
      | Leaf, _ -> Leaf
      | _, Leaf -> t1
      | Node(_, v1, cnt1, l1, r1), _ ->
          let cnt_in_t2 = count v1 t2 in
          let new_cnt = max 0 (cnt1 - cnt_in_t2) in
          let left_diff = aux l1 t2 in
          let right_diff = aux r1 t2 in
          if new_cnt > 0 then
            let rec insert_many t x cnt =
              if cnt = 0 then t
              else insert_many (insert x t) x (cnt - 1)
            in
            insert_many (merge_avl left_diff right_diff) v1 new_cnt
          else
            merge_avl left_diff right_diff
    in
    aux t1 t2

  (* fold *)
  let fold_left f init tree =
    let rec aux acc = function
      | Leaf -> acc
      | Node(_, v, cnt, l, r) ->
          let acc1 = aux acc l in       
          let acc2 = f acc1 v cnt in     
          aux acc2 r                    
    in
    aux init tree

  let fold_right f tree init =
    let rec aux acc = function
      | Leaf -> acc
      | Node(_, v, cnt, l, r) ->
          let acc1 = aux acc r in      
          let acc2 = f v cnt acc1 in 
          aux acc2 l                   
    in
    aux init tree

    (* equal *)
    let equal t1 t2 =
      is_empty (difference t1 t2) && is_empty (difference t2 t1)
    let ( = ) t1 t2 = equal t1 t2
end

