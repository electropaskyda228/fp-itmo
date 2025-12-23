open Stdio
open Arg
open Mylib

let buffer_size = ref 4
let delta = ref 0.5
let linear = ref false
let newton = ref false

let usage_msg =
  "Usage: train.exe -n <число> -step <число> --linear --newton [опции]\n\
   Примеры:\n
     ./train.exe -n 42 -step 0.5\n
     ./train.exe --n=100 --step=0.5"

let speclist = [
  ("--linear", Arg.Unit (fun () -> linear := true), "Включить линейный режим");
  ("-l",      Arg.Unit (fun () -> linear := true),  "То же, что --linear");
  ("--newton", Arg.Unit (fun () -> newton := true), "Включить newton режим");
  ("-n",      Arg.Unit (fun () -> newton := true),  "То же, что --newton");
  ("-n",       Set_int buffer_size,    "Задать число n");
  ("--n",      Set_int buffer_size,    "Задать число n");
  ("-step", Set_float delta, "Задать step");
  ("--step", Set_float delta, "Задать step");
  ("-help",    Unit (fun () -> print_endline usage_msg; exit 0), "Показать помощь");
  ("--help",   Unit (fun () -> print_endline usage_msg; exit 0), "Показать помощь")
]

let () =
  Arg.parse
    speclist
    (fun _ -> ())
    usage_msg;
  Input.pairs () 
  |> Processor.process_stream !buffer_size !delta !linear !newton 
  |> Output.to_stdout