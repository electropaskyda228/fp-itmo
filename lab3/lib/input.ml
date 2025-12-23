let pairs () : Common.pair Seq.t =
  let rec next () =
    try 
      let line = input_line stdin in
      match String.split_on_char ' ' line with
      | [x; y] -> Seq.Cons(Common.Pair(float_of_string x, float_of_string y), next)
      | _ -> next ()
    with
    | End_of_file -> Seq.Nil
  in next