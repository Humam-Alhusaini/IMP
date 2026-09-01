open Lang
open Interp
open In_channel

let () =
  let str = open_bin "example.mdc" |> input_all in
  let _ = read str Empty ~debug_tokens:true ~debug_ast:true () in
  ()
