open Parser
open Ctx

val lex : string -> bool -> Lexer.parseable_token list
val parse : Lexer.parseable_token list -> bool -> ast

val read :
  string ->
  aexp_map ->
  ?debug_tokens:bool ->
  ?debug_ast:bool ->
  unit ->
  aexp_map
