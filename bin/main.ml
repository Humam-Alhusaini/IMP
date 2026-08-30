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
      if debug_tokens || debug_ast then begin
        let tokens = lex txt debug_tokens in
        if debug_ast then parse tokens true |> ignore;
        ctx
      end
      else
        let newctx = read txt ctx () in
        repl newctx debug_tokens debug_ast

let run_file file debug_tokens debug_ast =
  let str = open_bin file |> input_all in
  if debug_tokens || debug_ast then begin
    let tokens = lex str debug_tokens in
    if debug_ast then parse tokens true |> ignore
  end
  else read str Empty () |> ignore

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
  | file :: _ -> run_file file !debug_tokens !debug_ast
