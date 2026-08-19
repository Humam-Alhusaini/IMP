
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
	mutable line_num : int; (*The line number*)
	mutable bol_off : int; (*The offset between the cursor and the start of the line*)
	mutable offset : int; (*The offset between the cursor and the start of the file*)
};;

type position = {
  line_num : int;
  bol_off : int;
  offset : int;
};;

let curs_to_pos (curs : cursor) : position =
  {line_num = curs.line_num; bol_off = curs.bol_off; offset = curs.offset};;

let pos_to_curs (pos : position) : cursor =
  {line_num = pos.line_num; bol_off = pos.bol_off; offset = pos.offset};;

type parseable_token = (token * position);;

exception Lexing_error of string * token list * cursor;;

open Printf

let string_of_chars chars =
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf;;

let string_to_tok str : token =
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

type t = {
  txt : string;
  curs : cursor;
};;

let create str : t = { txt = str; curs = { line_num = 1; bol_off = 0; offset = 0 } };;

let shiftr (lx : t) =
  lx.curs.offset <- lx.curs.offset + 1;
  lx.curs.bol_off <- lx.curs.bol_off + 1;;

let shiftl (lx : t) =
  lx.curs.offset <- lx.curs.offset - 1;
  lx.curs.bol_off <- lx.curs.bol_off - 1;;

let reset_bol_off (lx : t) =
  lx.curs.bol_off <- 0;;

let new_line (lx : t) =
  lx.curs.line_num <- lx.curs.line_num + 1;
  reset_bol_off lx;
  shiftr lx;;

let at_eof (lx : t) : bool = lx.curs.offset >= String.length lx.txt;;

let rec tokenize (lx : t) (tokens : parseable_token list) : parseable_token list =

  let tokenize_next toks = shiftr lx; tokenize lx toks in

  let tokenize_nline toks = new_line lx; tokenize lx toks in

  (*This tokenizes numbers, it is initiated when the lexer finds a num*)
  let rec tokenize_num (chars : char list) =
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

  let rec tokenize_word (chars : char list) : token =
    shiftr lx;
    if at_eof lx |> not then begin
      let char = lx.txt.[lx.curs.offset] in
      match char with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> chars @ [char] |> tokenize_word
      | _ -> shiftl lx; let token = chars |> string_of_chars |> string_to_tok in token end
    else
      let token = chars |> string_of_chars |> string_to_tok in token
          in

  let rec skip_comment () : unit =
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

  if at_eof lx |> not then begin
    let char = lx.txt.[lx.curs.offset] in
    match char with
    | ' ' | '\t' | '\r' -> tokenize_next tokens
    | '\n' -> tokenize_nline tokens
    | '\\' -> skip_comment (); tokenize_next tokens
    | _ -> let pos = lx.curs |> curs_to_pos in
           let t = match char with
                       | 'a' .. 'z' | 'A' .. 'Z' ->  tokenize_word [char]
                       | '0' .. '9' -> tokenize_num [char]
                       | '+' -> PLUS
                       | '*' -> MULT
                       | '-' -> SUB
                       | '=' -> EQ
                       | '~' -> NEQ
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
                       | t -> let (toks, _) = List.split tokens in
                            Lexing_error (sprintf "%c does not match any known char" t, toks, lx.curs) |> raise in
                            let token = (t, pos) in
                            tokenize_next (tokens @ [token]) end
  else
    tokens @ [(EOF, lx.curs |> curs_to_pos)]
