module Common = struct
  (* common *)
  let f n a b = n * n + a * n + b 

  let max (n1, p1) (n2, p2) = 
    if n1 >= n2 then (n1, p1) else (n2, p2)

  (* check whether number is prime *)
  let is_prime n = 
    if n < 2 then false
    else 
      let rec check p = 
        if p * p > n then true
        else if n mod p == 0 then false
        else check (p+1)
      in check 2

  let rec sort_through_n n a b= 
    if is_prime (f n a b) then sort_through_n (n + 1) a b else (n - 1)
end