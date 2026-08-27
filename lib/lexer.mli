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

type position = {
  line_num : int;
  bol_off : int;
  offset : int;
}

val start_pos : position

type parseable_token = token * position

exception Lexing_error of string * position

val parse_char : char -> position -> parseable_token

val tokenize : string -> position -> parseable_token list
