open Lang
open Interp
open Printer
open In_channel

let rec repl ctx =
  print_string ">>> ";
  let txt = read_line () in
  match txt with
  | "exit" ->
      print_endline "Goodbye!";
      ctx
  | "pctx" ->
      print_endline (fmap ctx);
      ctx
  | _ -> read txt ctx false |> repl

let () =
  if Array.length Sys.argv <> 1 then
    let str = Array.get Sys.argv 1 |> open_bin |> input_all in
    let _ = read str Empty false in
    ()
  else
    let _ = repl Empty in
    ()
