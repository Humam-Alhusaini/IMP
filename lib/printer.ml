
open Printf
open Parser
open Lexer
open Ctx

let fliteral lit = 
  match lit with
  | `NUM n -> sprintf "%d" n
  | `VAR str -> str;;

let rec faexp aexp = 
  match aexp with 
  | ALit (`NUM n) -> sprintf "%d" n
  | ALit (`VAR str) -> str
  | Aplus (aexp1, aexp2) -> 
      sprintf "(%s + %s)" (faexp aexp1) (fliteral aexp2)
  | Asub (aexp1, aexp2) -> 
      sprintf "(%s - %s)" (faexp aexp1) (fliteral aexp2)
  | Amult (aexp1, aexp2) -> 
      sprintf "(%s * %s)" (faexp aexp1) (fliteral aexp2)

let rec fbexp bexp = 
  match bexp with 
  | Bool b -> string_of_bool b
  | And (bexp1, bexp2) -> sprintf "%s && %s" (fbexp bexp1) (string_of_bool bexp2)
  | Or (bexp1, bexp2) -> sprintf "%s || %s" (fbexp bexp1) (string_of_bool bexp2)
  | Not bexp -> sprintf "not (%s)" (fbexp bexp)
  | BEq (aexp1, aexp2) -> sprintf "%s = %s" (faexp aexp1) (faexp aexp2)
  | BNeq (aexp1, aexp2) -> sprintf "%s <> %s" (faexp aexp1) (faexp aexp2)
  | BLe (aexp1, aexp2) -> sprintf "%s <= %s" (faexp aexp1) (faexp aexp2)
  | BGt (aexp1, aexp2) -> sprintf "%s < %s" (faexp aexp1) (faexp aexp2)
;;

let rec fmap map =
  match map with 
  | Empty -> ""
  | Elem (key, aexp, map') -> sprintf "%s -> %s\n%s" key (faexp aexp) (fmap map');;

let rec fdef (name, aexp) = 
  sprintf "Def %s = %s;\n" name (faexp aexp)

let rec fterm term = 
  match term with 
  | Def d -> fdef d
  | Elif (cond, ast1, ast2) -> sprintf "If (%s) then\n { %s }\n  else\n { %s };\n" (fbexp cond) (fast ast1) (fast ast2)
  | If (cond, ast) -> sprintf "If (%s) then\n { %s };\n" (fbexp cond) (fast ast)
  | Print aexp -> faexp aexp |> sprintf "Print (%s)"
  | Nop -> "Nop"

and fast ast = 
  match ast with
  | hd :: ls -> sprintf "%s %s" (fterm hd) (fast ls)
  | [] -> ""

let format_tok tok =
match tok with
| LIT lit -> "LIT"
| BOOL b -> "BOOL"
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
| COMMA -> "COMMA"
| PERIOD -> "PERIOD"
| THEN -> "THEN"
| EOF -> "EOF"
| DEF -> "DEF"
| PRINT -> "PRINT"
| ELIF -> "ELIF";;

let rec toks_to_tokens toks =
match toks with
| [] -> []
| (token, _) :: tl -> token :: toks_to_tokens tl;;

let rec print_tokens toks =
match toks with
| [] -> ()
| hd :: tl ->
    format_tok hd |> printf "%s\n";
  print_tokens tl;;

let format_pos pos =
  sprintf "line %d, offset %d" pos.line_num pos.bol_off;;

let format_ptoken (tok, pos) =
  sprintf "%s at %s" (format_tok tok) (format_pos pos);;

let rec print_ptokens ptokens = 
  match ptokens with
  | [] -> ()
  | hd :: tl ->
    format_ptoken hd |> printf "%s\n";
    print_ptokens tl;;

let error_of_token err tok =
sprintf "%s, got %s" err (format_ptoken tok);;
