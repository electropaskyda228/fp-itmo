# Лабораторная работа №3.

## Юдин Георгий Дмитриевич

## Описание проблемы
Цель: получить навыки работы с вводом/выводом, потоковой обработкой данных, командной строкой.

## Задание
В рамках лабораторной работы вам предлагается повторно реализовать лабораторную работу по предмету "Вычислительная математика" посвящённую интерполяции (в разные годы это лабораторная работа 3 или 4) со следующими дополнениями:

обязательно должна быть реализована линейная интерполяция (отрезками, link);
настройки алгоритма интерполяции и выводимых данных должны задаваться через аргументы командной строки:

какие алгоритмы использовать (в том числе два сразу);
частота дискретизации результирующих данных;
и т.п.;


входные данные должны задаваться в текстовом формате на подобии ".csv" (к примеру x;y\n или x\ty\n) и подаваться на стандартный ввод, входные данные должны быть отсортированы по возрастанию x;
выходные данные должны подаваться на стандартный вывод;
программа должна работать в потоковом режиме (пример -- cat | grep 11), это значит, что при запуске программы она должна ожидать получения данных на стандартный ввод, и, по мере получения достаточного количества данных, должна выводить рассчитанные точки в стандартный вывод;

    +---------------------------+
    | обработка входного потока |
    +---------------------------+
            |
            | поток / список / последовательность точек
            v
    +-----------------------+      +------------------------------+
    | алгоритм интерполяции |<-----| генератор точек, для которых |
    +-----------------------+      | необходимо вычислить         |
            |                      | промежуточные значения       |
            |                      +------------------------------+
            |
            | поток / список / последовательность рассчитанных точек
            v
    +------------------------+
    | печать выходных данных |
    +------------------------+


## Ключевые элементы реализации

### Module input
```ocaml
let pairs () : Common.pair Seq.t =
  let rec next () =
    try 
      let line = input_line stdin in
      let parts = 
        if String.contains line ';' then
          String.split_on_char ';' line
        else
          String.split_on_char ' ' line
      in
      match parts with
      | [x; y] -> 
          let x_float = float_of_string (String.trim x) in
          let y_float = float_of_string (String.trim y) in
          Seq.Cons(Common.Pair(x_float, y_float), next)
      | _ -> next ()
    with
    | End_of_file -> Seq.Nil
  in next
```

### Module processor
```ocaml
let process_stream n delta linear newton (input_stream : pair Seq.t) =
  let buffer = RingBuffer.create n in
  
  let emit_interpolated (): interpolated Seq.node =
    if RingBuffer.is_full buffer then
      let points = RingBuffer.to_list buffer in
      let linear_results = 
        if linear then 
          let interpolated = Interpolation.linear_interpolate points delta in
          List.map (fun p -> Interpolated("linear", p)) interpolated
        else []
      in
      
      let newton_results = 
        if newton then 
          let interpolated = Interpolation.newton_interpolate points delta in
          List.map (fun p -> Interpolated("newton", p)) interpolated
        else []
      in
      
      let all_results = linear_results @ newton_results in
      
      match all_results with
      | [] -> Seq.Nil
      | _ ->
          let rec emit results_list =
            match results_list with
            | [] -> Seq.Nil
            | p :: remaining -> 
                Seq.Cons(p, fun () -> emit remaining)
          in
          emit all_results
    else
      Seq.Nil in
  
  let rec process in_stream () =
    match in_stream () with
    | Seq.Nil -> 
        emit_interpolated ()
    | Seq.Cons(point,next_stream) ->
        RingBuffer.push buffer point;
        match emit_interpolated () with
        | Seq.Nil -> 
            process next_stream ()
        | interpolation_seq ->
            let rec combine_interpolation interpolation ()  =
              match interpolation () with
              | Seq.Nil -> process next_stream ()
              | Seq.Cons(p, next_interp) -> 
                  Seq.Cons(p, combine_interpolation next_interp)
            in
            combine_interpolation (fun () -> interpolation_seq) ()
          in
  process input_stream
```

### Module interpolation
```ocaml
let sort_by_x points =
  List.sort (fun (Pair(x1, _)) (Pair(x2, _)) -> 
    compare x1 x2
  ) points

(* Линейная интерполяция между двумя точками *)
let linear_interpolate_one (Pair(x1, y1)) (Pair(x2, y2)) delta =
  if x2 <= x1 then []
  else
    let step = delta in
    let rec interpolate current_x acc =
      if current_x > x2 then List.rev acc
      else
        let t = (current_x -. x1) /. (x2 -. x1) in
        let y = y1 +. t *. (y2 -. y1) in
        interpolate (current_x +. step) (Pair(current_x, y) :: acc)
    in
    interpolate x1 []

(* Интерполяция для всего набора отсортированных точек *)
let linear_interpolate points delta =
  let sorted = sort_by_x points in
  match sorted with
  | [] -> []
  | [_] -> []
  | first :: second :: rest ->
      let rec process prev remaining acc =
        match remaining with
        | [] -> acc
        | current :: next ->
            let interpolated = linear_interpolate_one prev current delta in
            let filtered_interpolated = 
              match interpolated with
              | [] -> []
              | _ :: tail -> tail
            in
            process current next (acc @ filtered_interpolated)
      in
      let initial_interpolated = linear_interpolate_one first second delta in
      process second rest initial_interpolated

(* Интерполяция Ньютона *)
let newton_interpolate points delta =
  let sorted_points = sort_by_x points in
  let n = List.length sorted_points in
  
  if n < 2 then []
  else
    let xs = List.map (fun (Pair(x, _)) -> x) sorted_points in
    let ys = List.map (fun (Pair(_, y)) -> y) sorted_points in
    
    let rec compute_divided_differences i j =
      if j = 0 then List.nth ys i
      else
        let diff1 = compute_divided_differences i (j - 1) in
        let diff2 = compute_divided_differences (i + 1) (j - 1) in
        let xi = List.nth xs i in
        let xi_j = List.nth xs (i + j) in
        (diff2 -. diff1) /. (xi_j -. xi)
    in
    
    let coefficients = 
      List.init n (fun i -> compute_divided_differences 0 i)
    in
    
    let evaluate x =
      let rec loop i acc product =
        if i >= n then acc
        else
          let coeff = List.nth coefficients i in
          let new_acc = acc +. coeff *. product in
          if i = n - 1 then new_acc
          else
            let xi = List.nth xs i in
            let new_product = product *. (x -. xi) in
            loop (i + 1) new_acc new_product
      in
      loop 0 0.0 1.0
    in
    
    let Pair(min_x, _) = List.hd sorted_points in
    let Pair(max_x, _) = List.rev sorted_points |> List.hd in
    let step = delta in
    
    let rec generate current_x acc =
      if current_x > max_x +. step /. 2.0 then List.rev acc
      else
        let y = evaluate current_x in
        generate (current_x +. step) (Pair(current_x, y) :: acc)
    in
    
    generate min_x []
```

### Module output
```ocaml
let to_stdout stream =
    Seq.iter (fun interpol -> 
      let Interpolated(logic, dot) = interpol in
      let Pair(a, b) = dot in 
      printf "%s: %.2f %.2f\n" logic a b;
      flush stdout;
    ) stream 
```

# Выводы

- Потоковую работу в ocaml можно реализовать через Seq.node

- Работа с командной строкой в ocaml весьма проста

Итог: Я познакомился с потоковой работой в ocaml, с вводом-выводом и с работой с командной строкой.
