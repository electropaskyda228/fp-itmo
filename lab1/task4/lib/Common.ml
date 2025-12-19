module Common = struct
  let max x y = if x > y then x else y
  let rec pow10 e =
    if e = 0 then 1
    else 10 * pow10 (e - 1)
  
  let rec num_digits n =
    if n = 0 then 1
    else if n < 10 then 1
    else 1 + num_digits (n / 10)
  
  let is_palindrome n =
    let len = num_digits n in
    let rec check i =
      if i >= len / 2 then true
      else
        let left = (n / pow10 (len - 1 - i)) mod 10 in
        let right = (n / pow10 i) mod 10 in
        if left <> right then false
        else check (i + 1)
    in
    check 0
end
