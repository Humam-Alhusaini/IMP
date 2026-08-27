open Lexer

type aexp =
  | Num of int
  | Aplus of aexp * aexp
  | Asub of aexp * aexp
  | Amult of aexp * aexp
  | Var of string

type bexp =
  | True
  | False
  | And of bexp * bexp
  | Or of bexp * bexp
  | Not of bexp
  | BEq of aexp * aexp
  | BNeq of aexp * aexp
  | BLe of aexp * aexp
  | BGt of aexp * aexp

type def = string * aexp

type term =
  | Def of def
  | Elif of bexp * ast * ast
  | If of bexp * ast
  | Print of aexp
  | Nop

and ast = term list

type toks = parseable_token list

exception Parsing_error of string * parseable_token
exception Fatal of string

val format_tok : token -> string
val toks_to_tokens : toks -> token list
val print_tokens : token list -> unit
val format_pos : position -> string
val format_token : parseable_token -> string
val error_of_token : string -> parseable_token -> string
val create : toks -> toks
val check_and_skip : toks -> token -> toks
val parse_aexp : toks -> toks * aexp
val parse_bool : parseable_token -> bexp
val parse_bexp : toks -> toks * bexp
val parse_nested : toks -> toks * ast
val parse_def : toks -> toks * term
val parse_if : toks -> toks * term
val parse_elif : toks -> toks * term
val parse_ret : toks -> toks * term
val parse_term : toks -> toks * term
val parse : toks -> ast -> token -> toks * ast
