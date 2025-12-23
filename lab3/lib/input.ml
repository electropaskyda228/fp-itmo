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