open Lexer

val format_tok : Lexer.token -> string

type aexp =
  | ALit of literal
  | Aplus of aexp * literal
  | Asub of aexp * literal
  | Amult of aexp * literal

type bexp =
  | Bool of bool
  | And of bexp * bexp
  | Or of bexp * bexp
  | Not of bexp
  | BEq of aexp * aexp
  | BNeq of aexp * aexp
  | BLe of aexp * aexp
  | BGt of aexp * aexp

type term =
  | Def of string * aexp
  | Elif of bexp * ast * ast
  | If of bexp * ast
  | While of bexp * ast
  | Print of aexp
  | Nop

and ast = term list

type toks = parseable_token list

exception Parsing_error of string * parseable_token
exception Fatal of string

val create : toks -> toks
val parse : toks -> ast -> token -> toks * ast
