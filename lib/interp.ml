open Lexer
open Printer
open Parser
open Ctx

open Printf
  
let rec simplify_aexp (ctx : aexp_map) (aexp : aexp) : int =
  let match_op op aexp1 aexp2 = 
    match op with
    | Add -> simplify_aexp ctx aexp1 + simplify_aexp ctx aexp2
    | Sub -> simplify_aexp ctx aexp1 - simplify_aexp ctx aexp2
    | Mult -> simplify_aexp ctx aexp1 * simplify_aexp ctx aexp2
    | Eq -> if simplify_aexp ctx aexp1 = simplify_aexp ctx aexp2 then 1 else 0 in

  match aexp with
  | Binop (op, aexp1, aexp2) -> match_op op aexp1 aexp2
  | Num y -> y
  | Var str -> simplify_aexp ctx (find str ctx);;

let rec simplify_term (ctx : aexp_map) (term : term) : ast  =
  match term with
  | If (aexp, ast1) -> if (simplify_aexp ctx aexp) > 0 then (simplify_ast ctx ast1) else [Nop]
  | Elif (aexp, ast1, ast2) -> if (simplify_aexp ctx aexp) > 0 then (simplify_ast ctx ast1) else (simplify_ast ctx ast2)
  | Def (str, aexp) -> [Def (str, Num (simplify_aexp ctx aexp))]
  | Print aexp -> [Print (Num (simplify_aexp ctx aexp))] 
  | Nop -> [Nop]

and simplify_ast (ctx : aexp_map) (ast : ast) : ast =
  match ast with
  | [] -> []
  | hd :: ls -> let ast = simplify_term ctx hd in ast @ simplify_ast ctx ls;;

let rec interp_term (ctx : aexp_map) (term : term) : aexp_map  =
  match term with
  | If (aexp, ast1) -> if (simplify_aexp ctx aexp) > 0 then (interp_ast ctx ast1) else ctx
  | Elif (aexp, ast1, ast2) -> if (simplify_aexp ctx aexp) > 0 then (interp_ast ctx ast1) else (interp_ast ctx ast2)
  | Print aexp -> Num (simplify_aexp ctx aexp) |> print faexp; ctx 
  | Nop -> ctx
  | Def (str, aexp) -> let newaexp = Num (simplify_aexp ctx aexp) in
                        add str newaexp ctx

and interp_ast (ctx : aexp_map) (ast : ast) : aexp_map =
  match ast with
  | [] -> let _ = print_string "ended" in ctx
  | hd :: ls -> let newctx = interp_term ctx hd in
                interp_ast newctx ls;;

let read str (ctx : aexp_map) : aexp_map =
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

