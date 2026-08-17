
open Lexer
open Tokens
open Printf

let format_tok (tok : Tokens.t) =
match tok with
| NUM i -> sprintf "NUM(%i)" i
| VAR s -> sprintf "VAR(%s)" s
| MULT -> "MULT"
| PLUS -> "PLUS"
| SUB -> "SUB"
| EQ -> "EQ"
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
| RETURN -> "RETURN"
| ELIF -> "ELIF";;

let rec toks_to_tokens toks : Tokens.t list =
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

let format_token (token : token) : string =
let (tok, pos) = token in
  sprintf "%s at %s" (format_tok tok) (format_pos pos);;

let error_of_token (err: string) (token : token) : string =
sprintf "%s, got %s" err (format_token token);;

exception Parsing_error of string * token
exception Fatal of string

type expr =
| Num of int
| Binop of op * expr * expr
| Var of string

and op =
| Add
| Sub
| Mult
| Eq

and def = string * expr

and term =
| Def of def
| Elif of expr * ast * ast
| If of expr * ast
| Ret of expr
| Nop

and ast = term list;;

let match_num n =
  match n with
  | (NUM y, _) -> Num y
  | (TRUE, _) -> Num 1
  | (FALSE, _) -> Num 0
  | (VAR y, _) -> Var y
  | _ -> Parsing_error ("Expected num", n) |> raise;;

let match_op op =
  match op with
  | (MULT, _) -> Mult
  | (PLUS, _) -> Add
  | (SUB, _) -> Sub
  | (EQ, _) -> Eq
  | _ -> Parsing_error ("Expected operator", op) |> raise;;

type toks = token list;;

let create tokens : toks = tokens;;

let check_and_skip (ps : toks) (endtok : Tokens.t) : toks =
  match ps with
  | [] -> Fatal "No tokens to parse" |> raise
  | (ftok, p) :: ls -> if endtok = ftok then
                       ls
                      else
                        Parsing_error ("Expected token to end statement", (ftok, p)) |> raise;;

let parse_expr (ps : toks) : toks * expr =

  let rec parse_binop (ps : toks) (curr : expr) : toks * expr =
    match ps with
    | ((MULT | SUB | PLUS | EQ) as op, p) :: num :: ls -> parse_binop ls (Binop(match_op (op, p), curr, match_num num))
    | [] -> Fatal "Token ended before finding end token" |> raise
    | (RPAREN, _) :: ls -> (ls, curr)
    | hd :: _ -> Parsing_error ("Expected expression to either end or continue", hd) |> raise in

  match ps with
  | (LPAREN, _) :: num :: ls -> parse_binop ls (match_num num)
  | [] -> Fatal "No tokens to parse" |> raise
  | hd :: _ -> Parsing_error ("Expression did not start with LPAREN", hd) |> raise;;

let rec parse_nested (ps : toks) : toks * ast =
  match ps with
  | (LBRACE, _) :: ls -> parse ls [] RBRACE
  | hd :: _ -> Parsing_error("Expected { for nested", hd) |> raise
  | [] -> Fatal "Major Error in Nested CF Parsing" |> raise

and parse_def (ps : toks) : toks * term =
  match ps with
  | (VAR str, _) :: (EQ, _) :: ls  -> let (ps, expr) = parse_expr ls in
                                                let term = Def (str, expr) in (ps, term)
  | (VAR str, _) :: tok :: _ -> Parsing_error ("Missing Equal Sign in Definition", tok) |> raise
  | tok :: _ -> Parsing_error ("Missing var", tok) |> raise
  | _ -> Fatal "Major issue in parsing definitions" |> raise

and parse_if (ps : toks) : toks * term =
  let (ps, cond) = parse_expr ps in
  let ps = check_and_skip ps THEN in
    let (ps, nested_term) = parse_nested ps in
      let term = If (cond, nested_term) in (ps, term)

and parse_elif (ps : toks) : toks * term =
  let (ps, cond) = parse_expr ps in 
  let ps = check_and_skip ps THEN in
  let (ps, term1) = parse_nested ps in
    let ps = check_and_skip ps ELSE in
  let (ps, term2) = parse_nested ps in
        let term = Elif (cond, term1, term2) in (ps, term)

and parse_ret (ps : toks) : toks * term =
  let (ps, expr) = parse_expr ps in
  let term = Ret expr in (ps, term)

and parse_term (ps : toks) : toks * term =
  let (ps, term) = 
    match ps with
  | (DEF, _) :: ls -> parse_def ls
  | (IF, _) :: ls -> parse_if ls
  | (ELIF, _) :: ls -> parse_elif ls
  | (RETURN, _) :: ls -> parse_ret ls
  | hd :: _ -> Parsing_error ("Expected def, num, or control flow", hd) |> raise
  | [] -> Fatal "Nothing here! Contact maintainers!" |> raise in
    match ps with
    | (SEMICOLON, _) :: ls -> (ls, term)
    | hd :: ls -> Parsing_error ("Expected Semicolon", hd) |> raise
    | [] -> Fatal "No tokens to parse" |> raise

and parse (ps : toks) (ast : ast) (endtok : Tokens.t) : toks * ast =
    let (toks, term) = parse_term ps in
    match toks with
    | [] -> Fatal "Didn't encounter EOF token, probably a lexer issue" |> raise
    | (ftok, _) :: ls -> 
      let newast = ast @ [term] in
        if endtok = ftok then (ls, newast) else parse toks newast endtok;;


