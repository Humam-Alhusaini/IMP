open Printf
open Parser
open Lexer
open Ctx

let fliteral lit = match lit with `NUM n -> sprintf "%d" n | `VAR str -> str

let rec faexp aexp =
  match aexp with
  | ALit (`NUM n) -> sprintf "%d" n
  | ALit (`VAR str) -> str
  | Aplus (aexp1, aexp2) -> sprintf "(%s + %s)" (faexp aexp1) (fliteral aexp2)
  | Asub (aexp1, aexp2) -> sprintf "(%s - %s)" (faexp aexp1) (fliteral aexp2)
  | Amult (aexp1, aexp2) -> sprintf "(%s * %s)" (faexp aexp1) (fliteral aexp2)

let rec fbexp bexp =
  match bexp with
  | Bool b -> string_of_bool b
  | And (bexp1, bexp2) -> sprintf "%s && %s" (fbexp bexp1) (fbexp bexp2)
  | Or (bexp1, bexp2) -> sprintf "%s || %s" (fbexp bexp1) (fbexp bexp2)
  | Not bexp -> sprintf "not (%s)" (fbexp bexp)
  | BEq (aexp1, aexp2) -> sprintf "%s = %s" (faexp aexp1) (faexp aexp2)
  | BNeq (aexp1, aexp2) -> sprintf "%s <> %s" (faexp aexp1) (faexp aexp2)
  | BLe (aexp1, aexp2) -> sprintf "%s <= %s" (faexp aexp1) (faexp aexp2)
  | BGt (aexp1, aexp2) -> sprintf "%s < %s" (faexp aexp1) (faexp aexp2)

let rec fmap map =
  match map with
  | Empty -> ""
  | Elem (key, aexp, map') ->
      sprintf "%s -> %s\n%s" key (faexp aexp) (fmap map')

let repeat s n = String.concat "" (List.init n (fun _ -> s))

let rec fterm term scope =
  match term with
  | Def (name, aexp) -> sprintf "def %s = %s" name (faexp aexp)
  | Elif (cond, ast1, ast2) ->
      sprintf "if (%s) then\n%s%selse\n%send" (fbexp cond)
        (fast ast1 (scope + 1)) 
        (repeat "\t" scope)
        (fast ast2 (scope + 1))
  | If (cond, ast) ->
      sprintf "if (%s) then\n %send" (fbexp cond) (fast ast (scope + 1))
  | While (cond, ast) ->
      sprintf "while (%s) do\n %send" (fbexp cond) (fast ast (scope + 1))
  | Print aexp -> faexp aexp |> sprintf "print (%s)"
  | Nop -> "Nop"

and fast ast scope =
  match ast with
  | [] -> ""
  | hd :: ls ->
      let tabs = repeat "\t" scope in
      sprintf "%s%s\n%s" tabs (fterm hd scope) (fast ls scope)

let rec toks_to_tokens toks =
  match toks with [] -> [] | (token, _) :: tl -> token :: toks_to_tokens tl

let rec print_tokens toks =
  match toks with
  | [] -> ()
  | hd :: tl ->
      format_tok hd |> printf "%s\n";
      print_tokens tl

let format_pos pos = sprintf "line %d, offset %d" pos.line_num pos.bol_off

let format_ptoken (tok, pos) =
  sprintf "%s at %s" (format_tok tok) (format_pos pos)

let rec print_ptokens ptokens =
  match ptokens with
  | [] -> ()
  | hd :: tl ->
      format_ptoken hd |> printf "%s\n";
      print_ptokens tl

let error_of_token err tok = sprintf "%s, got %s" err (format_ptoken tok)
