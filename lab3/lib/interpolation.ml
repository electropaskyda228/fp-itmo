open Common

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