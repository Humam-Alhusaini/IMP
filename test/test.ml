open Lang
open Interp
open In_channel

let () =
  let str = open_bin "test.mdc" |> input_all in
  let _ = read str Empty true in
  ()
