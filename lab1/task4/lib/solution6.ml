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