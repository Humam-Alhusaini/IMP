type token =
  | NUM of int
  | VAR of string
  | MULT
  | PLUS
  | SUB
  | EQ
  | NEQ
  | NOT
  | GT
  | LE
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | SEMICOLON
  | COLON
  | AND
  | OR
  | COMMA
  | PERIOD
  | IF
  | ELSE
  | TRUE
  | FALSE
  | NAT
  | EOF
  | THEN
  | ELIF
  | DEF
  | PRINT

type cursor = {
  mutable line_num : int;
  mutable bol_off : int;
  mutable offset : int;
}

type position = {
  line_num : int;
  bol_off : int;
  offset : int;
}

type parseable_token = token * position

type t

exception Lexing_error of string * token list * cursor

val curs_to_pos : cursor -> position
val pos_to_curs : position -> cursor
val string_of_chars : char list -> string
val string_to_tok : string -> token
val create : string -> t
val shiftr : t -> unit
val shiftl : t -> unit
val reset_bol_off : t -> unit
val new_line : t -> unit
val at_eof : t -> bool
val tokenize : t -> parseable_token list -> parseable_token list
