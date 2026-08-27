
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

let rec tokenize str pos : parseable_token list=
  (*
  (*This tokenizes numbers, it is initiated when the lexer finds a num*)
  let rec tokenize_num chars =
    shiftr lx;
    if at_eof lx |> not then begin
      let char = lx.txt.[lx.curs.offset] in
      match char with
      | 'a' .. 'z' | 'A' .. 'Z' ->
          let (toks, _) = List.split tokens in
          Lexing_error ("Can't end number with letter, add a space or something", toks, lx.curs) |> raise;
      | '0' .. '9' -> chars @ [char] |> tokenize_num
      | _ -> shiftl lx;
            let final_num = chars |> string_of_chars |> int_of_string in
            let token = NUM final_num in token end
    else
      let final_num = chars |> string_of_chars |> int_of_string in
        let token = NUM final_num in token
          in

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

  let rec skip_comment () =
    shiftr lx;
    if at_eof lx |> not then begin
      let char = lx.txt.[lx.curs.offset] in
      match char with
      | '\\' -> ()
      | _ -> skip_comment ()
    end
    else
    let (toks, _) = List.split tokens in
      Lexing_error ("Forgot to close comment", toks, lx.curs) |> raise in
  
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
  if at_eof str pos |> not then begin
    let char = str.[pos.offset] in
    match char with
    | ' ' | '\t' | '\r' -> shiftr pos |> tokenize str 
    | '\n' -> new_line pos |> tokenize str
(*  | '\\' -> skip_comment (); tokenize_next tokens*)
    | char -> parse_char char pos :: (shiftr pos |> tokenize str)
    end
  else
    [(EOF, pos)];;