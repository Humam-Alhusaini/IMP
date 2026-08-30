open Parser
open Lexer
open Ctx

val faexp : aexp -> string
val fmap : aexp_map -> string
val fast : ast -> int -> string
val print_tokens : token list -> unit
val format_ptoken : parseable_token -> string
