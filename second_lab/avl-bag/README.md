# Лабораторная работа №2. AVL-BAG

## Юдин Георгий Дмитриевич

## Описание проблемы
Цель: освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing), а также разделением интерфейса и особенностей реализации.
В рамках лабораторной работы вам предлагается реализовать одну из предложенных классических структур данных (список, дерево, бинарное дерево, hashmap, граф...).


## Мой вариант - avl-bag
Мне необходимо было реализовать мульти множество на avl дереве. Для балансировки avl-дерева в каждом узле хранится balance_factor (разница высот левого и правого поддерева), который может принимать значения -1, 0 и 1. 
При нарушении условия сбалансированности, происходит балансировка снизу вверх левыми и правыми поворотами. Структура данных поддерживает баззовые методы для коллекции: insert, delete, map, filter, fold_right, fold_left, equal;
и специальные для множества: union, intersaction и difference.

## Тестирование

API структуры данных покрыто unit тестами.

Проведено property-based тестирование. Проверялись следующие утверждения:

1) Avl-bag - моноид на операции union.

- Ассоциативность
```ocaml
(AvlMultiset.union tree1 tree2) = (AvlMultiset.union tree2 tree1) 
```

- Существование нейтрального элемента
```ocaml
(AvlMultiset.union tree AvlMultiset.empty) = (AvlMultiset.union AvlMultiset.empty tree) = tree
```

2) Коммутативность операции union
```ocaml
( AM.union (AM.union a b) c ) = ( AM.union a (AM.union b c) )
```

3) Работаспособность закона де-моргана
```ocaml
( AM.difference tree (AM.union tree1 tree2) ) = ( AM.intersection (AM.difference tree tree1) (AM.difference tree tree2) )
```

## Ключевые элементы реализации

### Интерфейс avl-bag
```ocaml
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
```


### Реализация

1) Метод поворота поддерева

```ocaml
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
```

2) Балансировка дерева

```ocaml
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
```

3) метод вставки элемента

```ocaml
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
```

4) Функция удаления

```ocaml
(* delete *)
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
```

5) Функция map

```ocaml
(*map)
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
```

6) Метод filter

```ocaml
(* filter *)
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
```

7) Fold-ы
```ocaml
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

```

8) Объединение множеств

```ocaml
let union t1 t2 = merge_avl t1 t2
```

9) Пересечение

```ocaml
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
```

10) Разница множеств

```ocaml
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
            else insert_many x (insert x t) x (cnt - 1)
          in
          insert_many (merge_avl left_diff right_diff) v1 new_cnt
        else
          merge_avl left_diff right_diff
  in
  aux t1 t2   
```

11) Равенство множеств, реализовано на уровне API через проверку разницы множеств на пустоту.

```ocaml
(* equal *)
let equal t1 t2 =
  is_empty (difference t1 t2) && is_empty (difference t2 t1)
let ( = ) t1 t2 = equal t1 t2
```

# Выводы

- В ocaml довольно простая и удобная работа с полиморфизмом для структур данных

- avl дерево больше подходит для частого чтения информации нежели для частой записи

- В ocaml удобные инструменты для создания API для структур

Итог: Я посмотрел на базовые приёмы и абстракции языка OCaml. Реализовал пструтуру данных multiset на avl-дереве и провел unit и property-based тестирование.
