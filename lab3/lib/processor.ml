open Common

let process_stream n delta linear newton (input_stream : pair Seq.t) =
  let buffer = RingBuffer.create n in
  
  let emit_interpolated (): interpolated Seq.node =
    if RingBuffer.is_full buffer then
      let points = RingBuffer.to_list buffer in
      let linear_results = 
        if linear then 
          let interpolated = Interpolation.linear_interpolate points delta in
          List.map (fun p -> Interpolated("linear", p)) interpolated
        else []
      in
      
      let newton_results = 
        if newton then 
          let interpolated = Interpolation.newton_interpolate points delta in
          List.map (fun p -> Interpolated("newton", p)) interpolated
        else []
      in
      
      let all_results = linear_results @ newton_results in
      
      match all_results with
      | [] -> Seq.Nil
      | _ ->
          let rec emit results_list =
            match results_list with
            | [] -> Seq.Nil
            | p :: remaining -> 
                Seq.Cons(p, fun () -> emit remaining)
          in
          emit all_results
    else
      Seq.Nil in
  
  Seq.flat_map (fun point ->
    RingBuffer.push buffer point;
    fun () -> emit_interpolated ()
  ) input_stream