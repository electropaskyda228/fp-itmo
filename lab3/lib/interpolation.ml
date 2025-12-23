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
  
  let rec divided_difference = function
    | [] -> failwith "Empty indices"
    | [i] -> 
        let Pair(_, y) = List.nth sorted_points i in
        y
    | i :: j :: rest ->
        let Pair(xi, _) = List.nth sorted_points i in
        let Pair(xj, _) = List.nth sorted_points j in
        let diff1 = divided_difference (i :: rest) in
        let diff2 = divided_difference (j :: rest) in
        (diff1 -. diff2) /. (xj -. xi)
  in
  let rec compute_differences order idx acc =
    if idx + order >= List.length sorted_points then List.rev acc
    else
      let diff = divided_difference (List.init (order + 1) ((+) idx)) in
      compute_differences order (idx + 1) (diff :: acc)
  in
  
  let rec all_differences order acc =
    if order >= List.length sorted_points then List.rev acc
    else
      let diffs = compute_differences order 0 [] in
      all_differences (order + 1) (diffs :: acc)
  in
  
  let differences_list = all_differences 1 [] in
  
  let Pair(min_x, _) = List.hd sorted_points in
  let Pair(max_x, _) = List.rev sorted_points |> List.hd in
  
  let rec newton_poly_value x idx acc product =
    if idx >= List.length sorted_points then acc
    else
      let diff = 
        if idx = 0 then
          let Pair(_, y0) = List.hd sorted_points in
          y0
        else
          List.nth (List.nth differences_list (idx - 1)) 0
      in
      
      let new_product = 
        if idx = 0 then 1.0
        else 
          let Pair(x_prev, _) = List.nth sorted_points (idx - 1) in
          product *. (x -. x_prev)
      in
      
      newton_poly_value x (idx + 1) (acc +. diff *. new_product) new_product
  in
  
  let step = delta in
  
  let rec generate_points current_x acc =
    if current_x > max_x +. step /. 2.0 then List.rev acc
    else
      let y = newton_poly_value current_x 0 0.0 1.0 in
      generate_points (current_x +. step) (Pair(current_x, y) :: acc)
  in
  
  generate_points min_x []