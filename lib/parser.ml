
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
| Elif of expr * term * term
| If of expr * term
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

type t = {
  mutable toks : token list;
};;

let create tokens : t = { toks = tokens };;

let shift (ps : t) =
  match ps.toks with
  | [] -> Fatal "No more tokens" |> raise
  | _ :: ls -> ps.toks <- ls;;

let rec shift_n (ps : t) num =
  let rec loop num =
    if num > 0 then begin
      shift ps; loop (num-1) end
    else () in loop num;;

let check_and_skip (ps : t) (endtok : Tokens.t) : unit =
  match ps.toks with
  | [] -> Fatal "No tokens to parse" |> raise
  | (ftok, p) :: _ -> if endtok = ftok then
                      shift ps 
                      else
                        Parsing_error ("Expected token to end statement", (ftok, p)) |> raise;;

let parse_expr (ps : t) : expr =

  let rec parse_binop (curr : expr) : expr =
    match ps.toks with
    | ((MULT | SUB | PLUS | EQ) as op, p) :: num :: _ -> shift_n ps 2; Binop(match_op (op, p), curr, match_num num) |> parse_binop
    | [] -> Fatal "Token ended before finding end token" |> raise
    | (RPAREN, _) :: _ -> let _ = shift ps in curr
    | hd :: _ -> Parsing_error ("Expected expression to either end or continue", hd) |> raise in

  match ps.toks with
  | (LPAREN, _) :: num :: _ -> shift_n ps 2; parse_binop (match_num num)
  | [] -> Fatal "No tokens to parse" |> raise
  | hd :: _ -> Parsing_error ("Expression did not start with LPAREN", hd) |> raise;;

let rec parse_nested (ps : t) : term =
  match ps.toks with
  | (LBRACE, _) :: _ -> shift ps; let cf = parse_term ps RBRACE in cf
  | hd :: _ -> Parsing_error("Expected { for nested", hd) |> raise
  | [] -> Fatal "Major Error in Nested CF Parsing" |> raise

and parse_def (ps : t) : term =
  match ps.toks with
  | (DEF, _) :: (VAR str, _) :: (EQ, _) :: _ -> shift_n ps 3;
    let expr = parse_expr ps in
                                                let term = Def (str, expr) in term
  | (DEF, _) :: (VAR str, _) :: tok :: _ -> Parsing_error ("Missing Equal Sign in Definition", tok) |> raise
  | (DEF, _) :: tok :: _ -> Parsing_error ("Missing var", tok) |> raise
  | _ -> Fatal "Major issue in parsing definitions" |> raise

and parse_if (ps : t) : term =
  shift ps; 
  let cond = parse_expr ps in
  let _ = check_and_skip ps THEN in
    let nested_term = parse_nested ps in
      let term = If (cond, nested_term) in term

and parse_elif (ps : t) : term =
  shift ps; 
  let cond = parse_expr ps in 
  let _ = check_and_skip ps THEN in
    let term1 = parse_nested ps in
    let _ = check_and_skip ps ELSE in
      let term2 = parse_nested ps in
        let term = Elif (cond, term1, term2) in term

and parse_ret (ps : t) : term =
  shift ps; 
  let term = Ret (parse_expr ps) in term

and parse_term (ps : t) (endtok : Tokens.t) : term =
  let term = match ps.toks with
  | (DEF, _) :: _ -> parse_def ps
  | (IF, _) :: _ -> parse_if ps
  | (ELIF, _) :: _ -> parse_elif ps
  | (RETURN, _) :: _ -> parse_ret ps
  | hd :: _ -> Parsing_error ("Expected def, num, or control flow", hd) |> raise
  | [] -> Fatal "Nothing here! Contact maintainers!" |> raise in
    match ps.toks with
    | [] -> Fatal "No tokens to parse" |> raise
    | (ftok, p) :: _ -> if endtok = ftok then
                      let _ = shift ps in term 
                      else
                        Parsing_error ("You used Wrong token to end the statement", (ftok, p)) |> raise;;

let rec parse (ps : t) (ast : ast) : ast =
    match ps.toks with
    | [(EOF, _)] -> ast
    | hd :: _ -> parse ps (ast @ [parse_term ps SEMICOLON]) 
    | [] -> Fatal "Didn't encounter EOF token, probably a lexer issue" |> raise
