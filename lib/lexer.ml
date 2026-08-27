
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

type parseable_token = (token * position);;

type str_pos = {
  txt : string;
  curs : position;
};;

exception Lexing_error of string * token list * position;;

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

let create str = { txt = str; curs = { line_num = 1; bol_off = 0; offset = 0 } };;

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

let at_eof lx = lx.curs.offset >= String.length lx.txt;;

let rec tokenize lx : parseable_token list=
  (*
  let tokenize_next toks = shiftr lx; tokenize lx toks in

  let tokenize_nline toks = new_line lx; tokenize lx toks in

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
  if at_eof lx |> not then begin
    let char = lx.txt.[lx.curs.offset] in
    match char with
    | ' ' | '\t' | '\r' -> {txt = lx.txt; curs = shiftr lx.curs} |> tokenize 
    | '\n' -> {txt = lx.txt; curs = new_line lx.curs} |> tokenize
(*  | '\\' -> skip_comment (); tokenize_next tokens*)
    | _ -> let _ = match char with
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
                       | t -> Lexing_error (sprintf "%c does not match any known char" t, [], lx.curs) |> raise in
                             {txt = lx.txt; curs = shiftr lx.curs} |> tokenize 
                          end
  else
    [(EOF, lx.curs)]
