
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

and ast =
| Def of def
| Elif of expr * ast * ast
| If of expr * ast
| Ret of expr
| Nop

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

let check_end (ps : t) (endtok : Tokens.t) (ast : ast) =
  match ps.toks with
  | [] -> Fatal "No tokens to parse" |> raise
  | (ftok, p) :: _ -> if endtok = ftok then
                      let _ = shift ps in ast 
                      else
                        Parsing_error ("Expected token to end statement", (ftok, p)) |> raise;;

let parse_expr (ps : t) (endtok : Tokens.t) : expr =

  let rec parse_binop (curr : expr) : expr =
    match ps.toks with
    | ((MULT | SUB | PLUS | EQ) as op, p) :: num :: _ -> shift_n ps 2; Binop(match_op (op, p), curr, match_num num) |> parse_binop
    | [] -> Fatal "Can't find end token" |> raise
    | (ftok, pos) :: _ -> (if ftok = endtok then let _ = shift ps in curr else
                    Parsing_error ("Expected expression to either end or continue", (ftok, pos)) |> raise) in

  match ps.toks with
  | [] -> Fatal "No tokens to parse" |> raise
  | num :: _ -> shift ps; match_num num |> parse_binop;;

let rec parse_nested (ps : t) : ast =
  match ps.toks with
  | (LBRACE, _) :: _ -> shift ps; let cf = parse ps RBRACE in cf
  | _ -> Fatal "Error in Nested Cf" |> raise

and parse_def (ps : t) : ast =
  match ps.toks with
  | (DEF, _) :: (VAR str, _) :: (EQ, _) :: _ -> shift_n ps 3;
                                                let expr = parse_expr ps SEMICOLON in
                                                let ast = Def (str, expr) in ast
  | _ -> Fatal "Issues" |> raise

and parse_if (ps : t) : ast =
  shift ps; let cond = parse_expr ps THEN in
    let nested_ast = parse_nested ps in
      let ast = If (cond, nested_ast) in ast

and parse_elif (ps : t) : ast =
  shift ps; let cond = parse_expr ps THEN in
    let ast1 = parse_nested ps in
      let ast2 = parse_nested ps in
        let ast = Elif (cond, ast1, ast2) in ast

and parse_ret (ps : t) : ast =
  shift ps; let ast = Ret (parse_expr ps SEMICOLON) in ast

and parse (ps : t) (endtok : Tokens.t) : ast =
  let ast = match ps.toks with
  | (DEF, _) :: _ -> parse_def ps
  | (IF, _) :: _ -> parse_if ps
  | (ELIF, _) :: _ -> parse_elif ps
  | (RETURN, _) :: _ -> parse_ret ps
  | hd :: _ -> Parsing_error ("Expected def, num, or control flow", hd) |> raise
  | [] -> Fatal "Nothing here! Contact maintainers!" |> raise in
  check_end ps endtok ast
