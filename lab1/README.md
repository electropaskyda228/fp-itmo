# Лабораторная работа №1 Project Euler #4 и #27

## Юдин Георгий Дмитриевич

## Описание проблемы
[Project Euler - Problem 4](https://projecteuler.net/problem=4): Найти наибольший палиндром, образованный произведением двух трехзначных чисел.

[Project Euler - Problem 27](https://projecteuler.net/problem=27): Дана квадратичная форма: $n^2 + an + b$, где $|a| < 1000$ и $|b| <= 10000$.
Необходимо найти произведение коэффициентов $a$ и $b$, при которых эта квадратичная форма производит наибольшое количество простых чисел для каждого подряд идущего значения n, начиная с 0.

Обе задачи реализованы несколькими стилями: обычная/хвостовая рекурсия, модульный стиль `map/filter/fold`, императивные циклы и ленивые последовательности.
<hr>


## Ключевые элементы реализации

### Euler #4 - наибольший палиндром

Проверка числа (в виде строки) на палиндром

```ocaml
let first_sign s = s.[0]
let last_sign s = s.[String.length s - 1] 
let get_middle s = String.sub s 1 (String.length s - 2) 

let rec is_palindrom s = if String.length s == 1 then true else
    match s with
    | "" -> true
    | _ -> (first_sign s == last_sign s) && is_palindrom (get_middle s)
```

Варианты решения:

1) обычная рекурсия

```ocaml
let rec f2 x y = match x with
    | 99 -> 0
    | _ -> let z = x * y in if is_palindrom(string_of_int z) then max z (f2 (x-1) y) else f2 (x-1) y

let rec f1 x = match x with
    | 99 -> 0
    | _ -> let y = f2 999 x in if is_palindrom(string_of_int y) then max y (f1 (x-1) ) else f1 (x-1)

let _ = print_endline (string_of_int (f1 999))
```

2) хвостовая рекурсия

```ocaml
let rec f2_tr x y acc = match x with
    | 99 -> acc
    | _ -> let z = x * y in if is_palindrom(string_of_int z) then f2_tr (x - 1) y (max z acc) else f2_tr (x-1) y acc

let rec f1_tr x acc = match x with
    | 99 -> acc
    | _ -> let y = f2_tr 999 x 0 in if is_palindrom(string_of_int y) then f1_tr (x-1) (max y acc) else f1_tr (x-1) acc

let _ = print_endline (string_of_int (f1_tr 999 0))
```

3) в модульном стиле

```ocaml
module PalindromeSolver = struct
  module Generation = struct
    let range start stop =
      List.init (stop - start + 1) (fun x -> x + start)
    
    let cartesian_product list1 list2 =
      List.concat_map (fun x -> 
        List.map (fun y -> (x, y)) list2
      ) list1
    
    let generate_products min_val max_val =
      let numbers = range min_val max_val in
      cartesian_product numbers numbers
      |> List.map (fun (a, b) -> a * b)
  end
  
  module Filter = struct
    let is_palindrome_string s =
      let len = String.length s in
      let rec check i j =
        if i >= j then true
        else if s.[i] <> s.[j] then false
        else check (i + 1) (j - 1)
      in
      check 0 (len - 1)
    
    let is_palindrome_number n =
      let s = string_of_int n in
      is_palindrome_string s
    
    let filter_palindromes numbers =
      List.filter is_palindrome_number numbers
  end
  
  module Reduction = struct
    let find_maximum numbers =
      List.fold_left max min_int numbers
    
    let find_maximum_opt numbers =
      match numbers with
      | [] -> None
      | hd::tl -> Some (List.fold_left max hd tl)
  end
  
  let solve min_digits max_digits =
    let min_val1 = int_of_float (10.0 ** float (min_digits - 1)) in
    let max_val1 = int_of_float (10.0 ** float min_digits) - 1 in
    let min_val2 = int_of_float (10.0 ** float (max_digits - 1)) in
    let max_val2 = int_of_float (10.0 ** float max_digits) - 1 in
    
    let numbers1 = Generation.range min_val1 max_val1 in
    let numbers2 = Generation.range min_val2 max_val2 in
    
    Generation.cartesian_product numbers1 numbers2
    |> List.map (fun (a, b) -> a * b)
    |> Filter.filter_palindromes
    |> Reduction.find_maximum
end

let _ =
  let result1 = PalindromeSolver.solve 3 3 in
  Printf.printf "Для 3-значных чисел: %d\n" result1;
```

 4) с генерацией последовательности через map

 ```ocaml
(* maximum *)
let max x y = if x > y then x else y

(* work with palindrom *)
let first_sign s = s.[0]
let last_sign s = s.[String.length s - 1] 
let get_middle s = String.sub s 1 (String.length s - 2) 

let rec is_palindrom s = if String.length s == 1 then true else
    match s with
    | "" -> true
    | _ -> (first_sign s == last_sign s) && is_palindrom (get_middle s)

(* with map generate *)
let rec ( -- ) (i1, j1) (i2, j2) = 
  if i1 > i2 || j1 > j2 then []
  else if j1 == j2 then (i1, j1) :: (i1 + 1, 100) -- (i2, j2)
  else (i1, j1) :: (i1, j1 + 1) -- (i2, j2)
let get_multiple (x, y) = string_of_int (x * y)
let max_element compare lst = 
  match lst with 
  | [] -> 0
  | h :: t -> List.fold_left compare h t

let result =
  (100, 100) -- (999, 999)
  |> List.map get_multiple
  |> List.filter is_palindrom
  |> List.map int_of_string
  |> max_element max
let _ = print_endline (string_of_int result)
```

5) с императивными циклами

```ocaml
let find_answer_with_circles =
  let acc = ref 0 in
  for a = 100 to 999 do 
    for b = 100 to 999 do 
      if is_palindrom (string_of_int (a * b)) then acc := max !acc (a * b)
    done
  done;
  !acc
let _ = print_endline (string_of_int find_answer_with_circles)
```

6) с ленивыми последовательностями
```ocaml
type 'a lazysequence = Cons of 'a * 'a lazysequence Lazy.t
let rec map_lazy f (Cons (h, t)) = Cons(f h, lazy(map_lazy f (Lazy.force t)))

let rec palindrom = Cons((100, 100), lazy (map_lazy (fun (x, y) -> if y == 999 then (x+1, 100) else (x, y+1)) palindrom))

let rec find_answer_lazy (Cons((x, y), t)) = if x == 999 then 0 else (if is_palindrom (string_of_int (x * y)) then max (x * y) (find_answer_lazy (Lazy.force t)) else (find_answer_lazy (Lazy.force t)))

let _ = print_endline (string_of_int (find_answer_lazy palindrom))
```

7) реализация на python

```python
def is_palindrome(n):
    s = str(n)
    return s == s[::-1]
    
maximum = 0
for a in range (100, 1000):
    for b in range(100, 1000):
        if is_palindrome(a * b):
            maximum = max(a * b, maximum)
print(maximum)
```

### Euler #27 - квадратичная форма

Проерка числа на простоту

```ocaml
let is_prime n = 
  if n < 2 then false
  else 
    let rec check p = 
      if p * p > n then true
      else if n mod p == 0 then false
      else check (p+1)
    in check 2
```

Вычисление квадратичной формы и нахождение максимального n, которое может дать квадратичная форма с данными коэффициентами

```ocaml
let f n a b = n * n + a * n + b

let rec sort_through_n n a b= 
  if is_prime (f n a b) then sort_through_n (n + 1) a b else (n - 1)
```

Варианты решения:

1) обычная рекурсия

```ocaml
let find_answer_with_recursion a_max b_max = 
  let rec sort_through_a a = 
    match a with
    | _ when a = -a_max - 1 -> (0, 0)
    | _ -> let rec sort_through_b b = 
      match b with 
      | _ when b = -b_max - 1 -> (0, 0)
      | _ -> max (sort_through_n 0 a b, a * b) (sort_through_b (b - 1))
      in max (sort_through_b b_max) (sort_through_a (a - 1)) in
  sort_through_a a_max
```

2) хвостовая рекурсия

```ocaml
let find_answer_with_tail_recursion a_max b_max = 
  let rec sort_through_a a acc = 
    match a with
    | _ when a = -a_max - 1 -> acc
    | _ -> let rec sort_through_b b acc = 
      match b with 
      | _ when b = -b_max - 1 -> acc
      | _ -> sort_through_b (b - 1) (max (sort_through_n 0 a b, a * b) acc)
      in sort_through_a (a - 1) (max (sort_through_b b_max (0, 0)) acc) in
  sort_through_a a_max (0, 0)
```

3) модульная реализация

```ocaml
module PrimeChecker = struct
  let is_prime n =
    if n < 2 then false
    else
      let rec check i =
        if i * i > n then true
        else if n mod i = 0 then false
        else check (i + 1)
      in
      check 2
end

module QuadraticSequence = struct
  let count_primes a b =
    let rec count n =
      let value = n * n + a * n + b in
      if value < 2 || not (PrimeChecker.is_prime value) then n
      else count (n + 1)
    in
    count 0
end

module CoefficientFinder = struct
  let find_best_coefficients limit =
    let best_count = ref 0 in
    let best_a = ref 0 in
    let best_b = ref 0 in
    
    for a = -limit to limit do
      for b = -limit to limit do
        let count = QuadraticSequence.count_primes a b in
        if count > !best_count then begin
          best_count := count;
          best_a := a;
          best_b := b
        end
      done
    done;
    
    (!best_a, !best_b, !best_count)
end

module Main = struct
  let solve () =
    let limit = 1000 in
    let a, b, _ = CoefficientFinder.find_best_coefficients limit in
    a * b
end

let _ = print_endline (string_of_int (Main.solve()))
```

4) с генерацией последовательности через map

```ocaml
let rec ( -- ) (i1, j1) (i2, j2) = 
  if i1 > i2 || j1 > j2 then [] 
  else if j1 == j2 then (i1, j1) :: (i1 + 1, -1000) -- (i2, j2)
  else (i1, j1) :: (i1, j1 + 1) -- (i2, j2)
let get_info (a, b) = ((sort_through_n 0 a b), a * b)
let max_element compare lst = 
  match lst with 
  | [] -> (0, 0)
  | h :: t -> List.fold_left compare h t 

let result = 
  (-1000, -1000) -- (1000, 1000)
  |> List.map get_info
  |> max_element max
```

4) с императивными циклами

```ocaml
let find_answer_with_circles =
  let r = ref (0, 0) in
  for a = -1000 to 1000 do
    for b = -1000 to 1000 do
      let n = ref 0 in
      while is_prime (f !n a b) do
        n := !n + 1 
      done;
      if fst !r < !n then r := (!n - 1, a * b)
    done
  done;
  fst !r
```

5) с ленивыми последовательностями

```ocaml
type 'a lazysequence = Cons of 'a * 'a lazysequence Lazy.t
let rec map f (Cons(h, t)) = Cons(f h, lazy(map f (Lazy.force t)))
let rec sort_through = Cons((-1000, -1000), 
  lazy(map (fun (x, y) -> if y == 1000 then (x + 1, -1000) else (x, y + 1)) sort_through))

let rec find_answer_with_sequence (Cons((a, b), t)) = 
  if a == 1001 then (0, 0) else max ((sort_through_n 0 a b), a * b) (find_answer_with_sequence (Lazy.force t))
```

6) реализация на python

```python
def f(n, a, b):
    return n * n + a * n + b
    
def is_prime(n):
    if n <= 1:
        return False
    i = 2
    while i * i <= n:
        if n % i == 0:
            return False
        i += 1
    return True
    
result = 0
n_max = 0
for a in range(-1000, 1001):
    for b in range(-1000, 1001):
        n = 0
        while is_prime(f(n, a, b)):
            n += 1
        if n_max < (n - 1):
            n_max = n - 1
            result = a * b
print(result)
print(n_max)   
```

# Выводы

- Хвостовая рекурсия ведёт себя как обычный цикл: работает быстро и не ест лишнюю память. Обычная рекурсия короче на вид, но на длинных входах может упереться в стек.

- Там, где нужно много раз подряд пройти по данным, обычные массивы и for-циклы получаются проще и часто быстрее.

- sequences удобны, когда данные можно «делать по ходу» и не хранить лишние промежуточные списки — код получается аккуратнее.

Итог: Я посмотрел на базовые приёмы и абстракции языка OCaml. Реализовал простые алгоритмы и струтуры данных и применил их на задачах проекта Эйлера.
