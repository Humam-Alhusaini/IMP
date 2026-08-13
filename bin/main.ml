open Lang
open Interp
open Printer
open In_channel

let rec repl ctx =
  print_string ">>> ";
  let txt = read_line () in
  match txt with
  | "exit" -> print_endline "Goodbye!"
  | "pctx" -> print_endline (fmap ctx); repl ctx
  | _ -> read txt ctx |> repl

let () = 
  (*If there is a file as an argument, parse the file*)
  if Array.length Sys.argv <> 1 then
    let str = Array.get Sys.argv 1 |> open_bin |> input_all in 
    let _ = read str Empty in () 
  else
    (*Else give a repl*)
    let _ = print_int (Array.length Sys.argv) in
  repl Empty
;;
