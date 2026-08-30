open Lang
open Interp
open Printer
open In_channel

let rec repl ctx debug_tokens debug_ast =
  print_string ">>> ";
  let txt = read_line () in
  match txt with
  | "exit" ->
      print_endline "Goodbye!";
      ctx
  | "pctx" ->
      print_endline (fmap ctx);
      ctx
  | _ ->
      let newctx = read txt ctx ~debug_tokens ~debug_ast () in
      repl newctx debug_tokens debug_ast

let () =
  let debug_tokens = ref false in
  let debug_ast = ref false in
  let args = ref [] in
  let spec =
    [
      ("-tokens", Arg.Set debug_tokens, "Print lexed tokens");
      ("-ast", Arg.Set debug_ast, "Print the parsed AST");
    ]
  in
  let usage = "Usage: Lang [-tokens] [-ast] [file]" in
  Arg.parse spec (fun file -> args := file :: !args) usage;
  match List.rev !args with
  | [] ->
      let _ = repl Empty !debug_tokens !debug_ast in
      ()
  | file :: _ ->
      let str = open_bin file |> input_all in
      let _ = read str Empty ~debug_tokens:!debug_tokens ~debug_ast:!debug_ast () in
      ()
