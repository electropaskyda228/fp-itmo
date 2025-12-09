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