module Common = struct
  (* maximum *)
  let max x y = if x > y then x else y

  (* work with palindrome *)
  let first_sign s = s.[0]
  let last_sign s = s.[String.length s - 1] 
  
  let get_middle s = 
    if String.length s <= 2 then ""
    else String.sub s 1 (String.length s - 2)

  let rec is_palindrome s = 
    if String.length s <= 1 then true 
    else 
      (first_sign s = last_sign s) && 
      is_palindrome (get_middle s)
end