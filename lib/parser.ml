
open Lexer
open Printf

let format_tok (tok : token) =
match tok with
| NUM i -> sprintf "NUM(%i)" i
| VAR s -> sprintf "VAR(%s)" s
| MULT -> "MULT"
| PLUS -> "PLUS"
| SUB -> "SUB"
| EQ -> "EQ"
| NEQ -> "NEQ"
| NOT -> "NOT"
| GT -> "GT"
| LE -> "LE"
| LPAREN -> "LPAREN"
| RPAREN -> "RPAREN"
| LBRACE -> "LBRACE"
| RBRACE -> "RBRACE"
| LBRACK -> "LBRACK"
| RBRACK -> "RBRACK"
| SEMICOLON -> "SEMICOLON"
| COLON -> "COLON"
| AND -> "AND"
| OR -> "OR"
| IF -> "IF"
| ELSE -> "ELSE"
| TRUE -> "TRUE"
| FALSE -> "FALSE"
| COMMA -> "COMMA"
| PERIOD -> "PERIOD"
| NAT -> "NAT"
| THEN -> "THEN"
| EOF -> "EOF"
| DEF -> "DEF"
| PRINT -> "PRINT"
| ELIF -> "ELIF";;

let rec toks_to_tokens toks : token list =
match toks with
| [] -> []
| (token, _) :: tl -> token :: toks_to_tokens tl;;

let rec print_tokens toks : unit =
match toks with
| [] -> ()
| hd :: tl ->
    format_tok hd |> printf "%s\n";
  print_tokens tl;;

let format_pos (pos : position) : string =
sprintf "line %d, offset %d" pos.line_num pos.bol_off;;

let format_token ((tok, pos) : parseable_token) : string =
  sprintf "%s at %s" (format_tok tok) (format_pos pos);;

let error_of_token (err: string) (tok : parseable_token) : string =
sprintf "%s, got %s" err (format_token tok);;

exception Parsing_error of string * parseable_token
exception Fatal of string

type aexp =
| Num of int
| Aplus of aexp * aexp
| Asub of aexp * aexp
| Amult of aexp * aexp
| Var of string

and bexp = 
  | True
  | False
  | And of bexp * bexp
  | Or of bexp * bexp
  | Not of bexp
  | BEq of aexp * aexp
  | BNeq of aexp * aexp
  | BLe of aexp * aexp
  | BGt of aexp * aexp

and def = string * aexp

and term =
| Def of def
| Elif of bexp * ast * ast
| If of bexp * ast
| Print of aexp
| Nop

and ast = term list;;

type toks = parseable_token list;;

let create tokens : toks = tokens;;

let check_and_skip (ps : toks) (endtok : token) : toks =
  match ps with
  | [] -> Fatal "No tokens to parse" |> raise
  | (ftok, p) :: ls -> if endtok = ftok then
                       ls
                      else
                        Parsing_error ("Expected token to end statement", (ftok, p)) |> raise;;

let parse_aexp (ps : toks) : toks * aexp =

  let rec parse_binop (ps : toks) (curr : aexp) : toks * aexp =
    match ps with
    | (MULT, p) :: (NUM y, _) :: ls -> parse_binop ls (Amult (curr, Num y))
    | (PLUS, p) :: (NUM y, _) :: ls -> parse_binop ls (Aplus (curr, Num y))
    | (SUB, p) :: (NUM y, _) :: ls -> parse_binop ls (Asub (curr, Num y))
    | [] -> Fatal "Token ended before finding end token" |> raise
    | (RPAREN, _) :: ls -> (ls, curr)
    | hd :: _ -> Parsing_error ("Expected aexpession to either end or continue", hd) |> raise in
  let match_num n =
    match n with
    | (NUM y, _) -> Num y
    | (VAR y, _) -> Var y
    | _ -> Parsing_error ("Expected num", n) |> raise in
  match ps with
  | (LPAREN, _) :: num :: ls -> parse_binop ls (match_num num)
  | [] -> Fatal "No tokens to parse" |> raise
  | hd :: _ -> Parsing_error ("aexpession did not start with LPAREN", hd) |> raise;;

let parse_bool ((tok, pos) : parseable_token) : bexp =
  match tok with
  | TRUE -> True
  | FALSE -> False
  | _ -> Parsing_error ("Expected bool", (tok, pos)) |> raise

let parse_bexp (ps : toks) : toks * bexp =
  (*
  let rec parse_binop (ps : toks) (curr : bexp) : toks * bexp =
    match ps with
    | ((AND | OR | EQ | NEQ) as op, p) :: num :: ls -> ()
    | [] -> Fatal "Token ended before finding end token" |> raise
    | (RPAREN, _) :: ls -> (ls, curr)
    | hd :: _ -> Parsing_error ("Expected aexpession to either end or continue", hd) |> raise in
    *)
  match ps with
  | (LPAREN, _) :: bool :: (RPAREN, _) :: ls  -> (ls, parse_bool bool)
  | [] -> Fatal "No tokens to parse" |> raise
  | hd :: _ -> Parsing_error ("aexpession did not start with LPAREN", hd) |> raise;;

let rec parse_nested (ps : toks) : toks * ast =
  match ps with
  | (LBRACE, _) :: ls -> parse ls [] RBRACE
  | hd :: _ -> Parsing_error("Expected { for nested", hd) |> raise
  | [] -> Fatal "Major Error in Nested CF Parsing" |> raise

and parse_def (ps : toks) : toks * term =
  match ps with
  | (VAR str, _) :: (EQ, _) :: ls  -> let (ps, aexp) = parse_aexp ls in
                                                let term = Def (str, aexp) in (ps, term)
  | (VAR str, _) :: tok :: _ -> Parsing_error ("Missing Equal Sign in Definition", tok) |> raise
  | tok :: _ -> Parsing_error ("Missing var", tok) |> raise
  | _ -> Fatal "Major issue in parsing definitions" |> raise

and parse_if (ps : toks) : toks * term =
  let (ps, cond) = parse_bexp ps in
  let ps = check_and_skip ps THEN in
    let (ps, nested_term) = parse_nested ps in
      let term = If (cond, nested_term) in (ps, term)

and parse_elif (ps : toks) : toks * term =
  let (ps, cond) = parse_bexp ps in 
  let ps = check_and_skip ps THEN in
  let (ps, term1) = parse_nested ps in
    let ps = check_and_skip ps ELSE in
  let (ps, term2) = parse_nested ps in
        let term = Elif (cond, term1, term2) in (ps, term)

and parse_ret (ps : toks) : toks * term =
  let (ps, aexp) = parse_aexp ps in
  let term = Print aexp in (ps, term)

and parse_term (ps : toks) : toks * term =
  let (ps, term) = 
    match ps with
  | (DEF, _) :: ls -> parse_def ls
  | (IF, _) :: ls -> parse_if ls
  | (ELIF, _) :: ls -> parse_elif ls
  | (PRINT, _) :: ls -> parse_ret ls
  | hd :: _ -> Parsing_error ("Expected def, num, or control flow", hd) |> raise
  | [] -> Fatal "Nothing here! Contact maintainers!" |> raise in
    match ps with
    | (SEMICOLON, _) :: ls -> (ls, term)
    | hd :: ls -> Parsing_error ("Expected Semicolon", hd) |> raise
    | [] -> Fatal "No tokens to parse" |> raise

and parse (ps : toks) (ast : ast) (endtok : token) : toks * ast =
    let (toks, term) = parse_term ps in
    match toks with
    | [] -> Fatal "Didn't encounter EOF token, probably a lexer issue" |> raise
    | (ftok, _) :: ls -> 
      let newast = ast @ [term] in
        if endtok = ftok then (ls, newast) else parse toks newast endtok;;


