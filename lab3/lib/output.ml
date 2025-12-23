open Common
open Stdio

let to_stdout stream =
    Seq.iter (fun interpol -> 
      let Interpolated(logic, dot) = interpol in
      let Pair(a, b) = dot in 
      printf "%s: %.2f %.2f\n" logic a b;
      flush stdout;
    ) stream 