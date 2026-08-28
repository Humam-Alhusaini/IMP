
open Lexer
open Printf

exception Parsing_error of string * parseable_token
exception Fatal of string

type aexp =
| ALit of literal
| Aplus of aexp * literal
| Asub of  aexp * literal
| Amult of aexp * literal

and bexp = 
  | Bool of bool
  | And of bexp * bool 
  | Or of bexp * bool
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

let create tokens = tokens;;

let check_and_skip ps endtok =
  match ps with
  | [] -> Fatal "No tokens to parse" |> raise
  | (ftok, p) :: ls -> if endtok = ftok then
                       ls
                      else
                        Parsing_error ("Expected token to end statement", (ftok, p)) |> raise;;

let parse_aexp ps =
  let rec parse_binop ps curr =
    match ps with
    | (MULT, p) :: (LIT lit, _) :: ls -> parse_binop ls (Amult (curr, lit))
    | (PLUS, p) :: (LIT lit, _) :: ls -> parse_binop ls (Aplus (curr, lit))
    | (SUB, p) :: (LIT lit, _) :: ls -> parse_binop ls (Asub (curr, lit))
    | [] -> Fatal "No EOF token?" |> raise
    | (RPAREN, _) :: ls -> (ls, curr)
    | hd :: _ -> Parsing_error ("Expected aexpession to either end or continue", hd) |> raise in
  match ps with
  | (LPAREN, _) :: (LIT lit, _) :: ls -> parse_binop ls (ALit lit)
  | (LIT lit, _) :: ls -> (ls, ALit lit)
  | [] -> Fatal "No tokens to parse" |> raise
  | hd :: _ -> Parsing_error ("aexpession did not start with LPAREN", hd) |> raise;;
(*
let parse_bool (tok, pos) =
  match tok with
  | TRUE -> True
  | FALSE -> False
  | _ -> Parsing_error ("Expected bool", (tok, pos)) |> raise
*)

let tok_to_bool = function
  | `TRUE -> true 
  | `FALSE ->  false;;

let rec parse_bexp ps =
  let rec parse_bexp_in (ps : toks) (curr : bexp) : toks * bexp =
    match ps with
    | (RPAREN, _) :: ls -> (ls, curr)
    | [] -> Fatal "Token ended before finding end token" |> raise
    | hd :: _ -> Parsing_error ("Expected aexpession to either end or continue", hd) |> raise in
  match ps with
  | (LPAREN, _) :: (BOOL bye, _) :: ls -> Bool bye |> parse_bexp_in ls 
  | (LPAREN, _) :: (NOT, _) :: ls  -> let (toks, b) = parse_bexp ls in (toks, Not b)
  | (BOOL b, _) :: ls  -> (ls, Bool b)
  | [] -> Fatal "No tokens to parse" |> raise
  | hd :: _ -> Parsing_error ("bexpession did not start with LPAREN", hd) |> raise;;

let rec parse_nested ps =
  match ps with
  | (LBRACE, _) :: ls -> parse ls [] RBRACE
  | hd :: _ -> Parsing_error("Expected { for nested", hd) |> raise
  | [] -> Fatal "Major Error in Nested CF Parsing" |> raise

and parse_def ps =
  match ps with
  | (LIT (`VAR str), _) :: (EQ, _) :: ls  -> let (ps, aexp) = parse_aexp ls in
                                                let term = Def (str, aexp) in (ps, term)
  | (LIT (`VAR str), _) :: tok :: _ -> Parsing_error ("Missing Equal Sign in Definition", tok) |> raise
  | tok :: _ -> Parsing_error ("Missing var", tok) |> raise
  | _ -> Fatal "Major issue in parsing definitions" |> raise

and parse_if ps =
  let (ps, cond) = parse_bexp ps in
  let ps = check_and_skip ps THEN in
    let (ps, nested_term) = parse_nested ps in
      let term = If (cond, nested_term) in (ps, term)

and parse_elif ps =
  let (ps, cond) = parse_bexp ps in 
  let ps = check_and_skip ps THEN in
  let (ps, term1) = parse_nested ps in
    let ps = check_and_skip ps ELSE in
  let (ps, term2) = parse_nested ps in
        let term = Elif (cond, term1, term2) in (ps, term)

and parse_ret ps =
  let (ps, aexp) = parse_aexp ps in
  let term = Print aexp in (ps, term)

and parse_term ps =
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

and parse ps ast endtok =
    let (toks, term) = parse_term ps in
    match toks with
    | [] -> Fatal "Didn't encounter EOF token, probably a lexer issue" |> raise
    | (ftok, _) :: ls -> 
      let newast = ast @ [term] in
        if endtok = ftok then (ls, newast) else parse toks newast endtok;;


