open Lexer
open Printer
open Parser
open Ctx

open Printf

let rec simplify_aexp (ctx : aexp_map) (aexp : aexp) : int =
  match aexp with
  | Aplus (aexp1, aexp2) -> simplify_aexp ctx aexp1 + simplify_aexp ctx aexp2
  | Asub (aexp1, aexp2) -> simplify_aexp ctx aexp1 - simplify_aexp ctx aexp2
  | Amult (aexp1, aexp2) -> simplify_aexp ctx aexp1 * simplify_aexp ctx aexp2
  | Num y -> y
  | Var str -> simplify_aexp ctx (find str ctx);;

let rec simplify_bexp (ctx : aexp_map) (bexp : bexp) : bool =
  match bexp with
  | True -> true
  | False -> false
  | And (bexp1, bexp2) -> simplify_bexp ctx bexp1 && simplify_bexp ctx bexp2
  | Or (bexp1, bexp2) -> simplify_bexp ctx bexp1 || simplify_bexp ctx bexp2
  | Not bexp -> not (simplify_bexp ctx bexp)
  | BEq (aexp1, aexp2) -> simplify_aexp ctx aexp1 = simplify_aexp ctx aexp2
  | BNeq (aexp1, aexp2) -> simplify_aexp ctx aexp1 <> simplify_aexp ctx aexp2
  | BLe (aexp1, aexp2) -> simplify_aexp ctx aexp1 <= simplify_aexp ctx aexp2
  | BGt (aexp1, aexp2) -> simplify_aexp ctx aexp1 > simplify_aexp ctx aexp2

let rec simplify_term (ctx : aexp_map) (term : term) : ast  =
  match term with
  | If (bexp, ast1) -> if (simplify_bexp ctx bexp) then (simplify_ast ctx ast1) else [Nop]
  | Elif (bexp, ast1, ast2) -> if (simplify_bexp ctx bexp) then (simplify_ast ctx ast1) else (simplify_ast ctx ast2)
  | Def (str, aexp) -> [Def (str, Num (simplify_aexp ctx aexp))]
  | Print aexp -> [Print (Num (simplify_aexp ctx aexp))] 
  | Nop -> [Nop]

and simplify_ast (ctx : aexp_map) (ast : ast) : ast =
  match ast with
  | [] -> []
  | hd :: ls -> let ast = simplify_term ctx hd in ast @ simplify_ast ctx ls;;

let rec interp_term (ctx : aexp_map) (term : term) : aexp_map  =
  match term with
  | If (bexp, ast1) -> if (simplify_bexp ctx bexp) then (interp_ast ctx ast1) else ctx
  | Elif (bexp, ast1, ast2) -> if (simplify_bexp ctx bexp) then (interp_ast ctx ast1) else (interp_ast ctx ast2)
  | Print aexp -> Num (simplify_aexp ctx aexp) |> faexp |> printf "%s\n"; ctx 
  | Nop -> ctx
  | Def (str, aexp) -> let newaexp = Num (simplify_aexp ctx aexp) in
                        add str newaexp ctx

and interp_ast (ctx : aexp_map) (ast : ast) : aexp_map =
  match ast with
  | [] -> ctx
  | hd :: ls -> let newctx = interp_term ctx hd in
                interp_ast newctx ls;;

let read str (ctx : aexp_map) (debug : bool) : aexp_map =
  try
    let lex = Lexer.create str in
      let tokens = Lexer.tokenize lex [] in
        let ps = Parser.create tokens in
        let (_,ast) = Parser.parse ps [] EOF in
          if debug then
            fast ast |> printf "%s\n";
          let newctx = interp_ast ctx ast in newctx
  with 
  | Lexing_error (err, toks, pos) -> 
      printf "LEXING ERROR at line %d, offset %d: %s\n\n\n" pos.line_num pos.bol_off err;
      print_string "Printing retrieved tokens...\n\n";
      print_tokens toks; ctx
  | Parsing_error (err, tok) -> printf "\n"; printf "PARSING ERROR: %s, got %s\n" err (format_token tok); ctx
  | Map_error err -> printf "\n"; printf "Value %s does not exist in context\n" err; ctx
  | Fatal err -> printf "\n"; printf "CONTACT MAINTAINERS: %s\n" err; ctx;;

