open Lexer
open Printer
open Parser
open Ctx

open Printf
  
let rec simplify_expr (ctx : expr_map) (expr : expr) : int =
  let match_op op expr1 expr2 = 
    match op with
    | Add -> simplify_expr ctx expr1 + simplify_expr ctx expr2
    | Sub -> simplify_expr ctx expr1 - simplify_expr ctx expr2
    | Mult -> simplify_expr ctx expr1 * simplify_expr ctx expr2
    | Eq -> if simplify_expr ctx expr1 = simplify_expr ctx expr2 then 1 else 0 in

  match expr with
  | Binop (op, expr1, expr2) -> match_op op expr1 expr2
  | Num y -> y
  | Var str -> simplify_expr ctx (find str ctx);;

let rec simplify_term (ctx : expr_map) (term : term) : ast  =
  match term with
  | If (expr, ast1) -> if (simplify_expr ctx expr) > 0 then (simplify_ast ctx ast1) else [Nop]
  | Elif (expr, ast1, ast2) -> if (simplify_expr ctx expr) > 0 then (simplify_ast ctx ast1) else (simplify_ast ctx ast2)
  | Def (str, expr) -> [Def (str, Num (simplify_expr ctx expr))]
  | Ret expr -> [Ret (Num (simplify_expr ctx expr))] 
  | Nop -> [Nop]

and simplify_ast (ctx : expr_map) (ast : ast) : ast =
  match ast with
  | [] -> []
  | hd :: ls -> let ast = simplify_term ctx hd in ast @ simplify_ast ctx ls;;

let rec interp_term (ctx : expr_map) (term : term) : expr_map  =
  match term with
  | If (expr, ast1) -> if (simplify_expr ctx expr) > 0 then (interp_ast ctx ast1) else ctx
  | Elif (expr, ast1, ast2) -> if (simplify_expr ctx expr) > 0 then (interp_ast ctx ast1) else (interp_ast ctx ast2)
  | Ret expr -> Num (simplify_expr ctx expr) |> print fexpr; ctx 
  | Nop -> ctx
  | Def (str, expr) -> let newexpr = Num (simplify_expr ctx expr) in
                        add str newexpr ctx

and interp_ast (ctx : expr_map) (ast : ast) : expr_map =
  match ast with
  | [] -> let _ = print_string "ended" in ctx
  | hd :: ls -> let newctx = interp_term ctx hd in
                interp_ast newctx ls;;

let read str (ctx : expr_map) : expr_map =
  try
    let lex = Lexer.create str in
      let tokens = Lexer.tokenize lex [] in
        let ps = Parser.create tokens in
        let (_,ast) = Parser.parse ps [] EOF in
          let _ =  print fast ast in
            let newctx = interp_ast ctx ast in newctx
  with 
  | Lexing_error (err, toks, pos) -> 
      printf "LEXING ERROR at line %d, offset %d: %s\n\n\n" pos.line_num pos.bol_off err;
      print_string "Printing retrieved tokens...\n\n";
      print_tokens toks; ctx
  | Parsing_error (err, tok) -> printf "\n"; printf "PARSING ERROR: %s, got %s\n" err (format_token tok); ctx
  | Map_error err -> printf "\n"; printf "Value %s does not exist in context\n" err; ctx
  | Fatal err -> printf "\n"; printf "CONTACT MAINTAINERS: %s\n" err; ctx;;

