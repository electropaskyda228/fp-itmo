type pair = Pair of float * float
type interpolated = Interpolated of string * pair

module RingBuffer = struct
  type t = {
    data : pair array;
    mutable start : int;
    mutable size : int;
    capacity : int;
  }

  let create capacity = {
    data = Array.make capacity (Pair(0., 0.));
    start = 0;
    size = 0;
    capacity;
  }

  let is_full t = t.size = t.capacity

  let push t value =
    let idx = (t.start + t.size) mod t.capacity in
    t.data.(idx) <- value;
    if is_full t then
      t.start <- (t.start + 1) mod t.capacity
    else
      t.size <- t.size + 1

  let to_list t =
    let rec loop i acc =
      if i >= t.size then acc
      else
        let idx = (t.start + i) mod t.capacity in
        loop (i + 1) (t.data.(idx) :: acc)
    in
    List.rev (loop 0 [])
end
