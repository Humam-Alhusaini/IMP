
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
};;

let start_pos = {
  line_num = 1;
  bol_off = 0;
  offset = 0
}

type parseable_token = (token * position);;

exception Lexing_error of string * position;;

open Printf

let string_of_chars chars =
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf;;

let string_to_tok str =
  match str with
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "true" -> TRUE
  | "false" -> FALSE
  | "nat" -> NAT
  | "def" -> DEF
  | "elif" -> ELIF
  | "print" -> PRINT
  | "not" -> NOT
  | _ -> VAR str;;

let shiftr pos : position =
  { line_num = pos.line_num;
    offset = pos.offset + 1;
    bol_off = pos.bol_off + 1
  };;

let shiftrn pos n : position =
  { line_num = pos.line_num;
    offset = pos.offset + n;
    bol_off = pos.bol_off + n
  };;

let shiftl pos =
  { line_num = pos.line_num;
    offset = pos.offset - 1;
    bol_off = pos.bol_off - 1
  };;

let reset_bol_off pos =
  { line_num = pos.line_num;
    offset = pos.offset;
    bol_off = 0
  };;

let new_line pos =
  { line_num = pos.line_num + 1;
    offset = pos.offset + 1;
    bol_off = 1
  };;

let at_eof str pos = pos.offset >= String.length str;;

let parse_char char pos : parseable_token = 
  let t = match char with
     (*| 'a' .. 'z' | 'A' .. 'Z' ->  tokenize_word [char]
     | '0' .. '9' -> tokenize_num [char]
     | '<' -> tokenize_symbol () (*Add an interp symbol function*)*)
     | '+' -> PLUS
     | '*' -> MULT
     | '-' -> SUB
     | '=' -> EQ
     | '>' -> GT
     | '(' -> LPAREN
     | ')' -> RPAREN
     | '{' -> LBRACE
     | '}' -> RBRACE
     | '[' -> LBRACK
     | ']' -> RBRACK
     | ';' -> SEMICOLON
     | ':' -> COLON
     | '&' -> AND
     | '|' -> OR
     | ',' -> COMMA
     | '.' -> PERIOD
     | errt -> Lexing_error (sprintf "%c does not match any known char" errt, pos) |> raise in
        (t, pos);;
        (*t :: (shiftr pos |> tokenize str ) in*)

(*
          let final_num = chars |> string_of_chars |> int_of_string in
          let token = NUM final_num in token end
  else
    let final_num = chars |> string_of_chars |> int_of_string in
      let token = NUM final_num in token;;
*)

let rec charify_word lx pos =
  if at_eof lx pos |> not then begin
    let char = lx.[pos.offset] in
    match char with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> char :: (shiftr pos |> charify_word lx)
    | _ -> [] end
  else [];;

let rec charify_num lx pos : char list =
  if at_eof lx pos |> not then begin
    let char = lx.[pos.offset] in
    match char with
    | 'a' .. 'z' | 'A' .. 'Z' -> 
      Lexing_error ("Can't end number with letter, add a space or something", pos) |> raise;
    | '0' .. '9' -> char :: (shiftr pos |> charify_num lx)
    | _ -> [] end
    else [];;

let rec tokenize str pos : parseable_token list =
  (*
  let rec tokenize_word chars =
    shiftr lx;
    if at_eof lx |> not then begin
      let char = lx.txt.[lx.curs.offset] in
      match char with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> chars @ [char] |> tokenize_word
      | _ -> shiftl lx; let token = chars |> string_of_chars |> string_to_tok in token end
    else
      let token = chars |> string_of_chars |> string_to_tok in token
          in

  let tokenize_symbol () =
    shiftr lx;
    if at_eof lx |> not then begin
      let char = lx.txt.[lx.curs.offset] in
      match char with
      | '=' -> LE
      | '>' -> NEQ
      | _ ->
          let (toks, _) = List.split tokens in
        Lexing_error ("< should either end with = or >", toks, lx.curs) |> raise end
  else 
    let (toks, _) = List.split tokens in
    Lexing_error ("< should either end with = or >, but it ended with EOF", toks, lx.curs) |> raise
in
*)

let tokenize_word lx pos =
  let chars = charify_word lx pos in
  let str = chars |> string_of_chars in
    (VAR str, pos) :: (List.length chars |> shiftrn pos |> tokenize lx) in

let tokenize_num lx pos =
  let chars = charify_num lx pos in
  let int = chars |> string_of_chars  |> int_of_string in
    (NUM int, pos) :: (List.length chars |> shiftrn pos |> tokenize lx) in

  let rec skip_comment lx pos =
    if at_eof lx pos |> not then begin
      let char = str.[pos.offset] in
      match char with
      | '\\' -> shiftr pos |> tokenize lx
      | _ -> shiftr pos |> skip_comment lx
    end
  else Lexing_error ("Forgot to close comment", pos) |> raise in
  
  if at_eof str pos |> not then begin
    let char = str.[pos.offset] in
    match char with
    | '0' .. '9' -> tokenize_num str pos
    | 'a' .. 'z' | 'A' .. 'Z' -> tokenize_word str pos
    | ' ' | '\t' | '\r' -> shiftr pos |> tokenize str 
    | '\n' -> new_line pos |> tokenize str
    | '\\' -> shiftr pos |> skip_comment str
    | char -> parse_char char pos :: (shiftr pos |> tokenize str)
    end
  else
    [(EOF, pos)];;