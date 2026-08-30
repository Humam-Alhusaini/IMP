open Parser
open Ctx

val simplify_aexp : aexp_map -> aexp -> int
val simplify_bexp : aexp_map -> bexp -> bool
val simplify_term : aexp_map -> term -> ast
val simplify_ast : aexp_map -> ast -> ast
val interp_term : aexp_map -> term -> aexp_map
val interp_ast : aexp_map -> ast -> aexp_map
val lex : string -> bool -> Lexer.parseable_token list
val parse : Lexer.parseable_token list -> bool -> ast
val read :
  string -> aexp_map -> ?debug_tokens:bool -> ?debug_ast:bool -> unit -> aexp_map
