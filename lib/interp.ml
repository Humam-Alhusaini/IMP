open Lexer
open Printer
open Parser
open Ctx
open Printf

let rec int_of_lit ctx lit =
  match lit with `NUM n -> n | `VAR str -> simplify_aexp ctx (find str ctx)

and simplify_aexp ctx aexp =
  match aexp with
  | Aplus (aexp1, aexp2) -> simplify_aexp ctx aexp1 + int_of_lit ctx aexp2
  | Asub (aexp1, aexp2) -> simplify_aexp ctx aexp1 - int_of_lit ctx aexp2
  | Amult (aexp1, aexp2) -> simplify_aexp ctx aexp1 * int_of_lit ctx aexp2
  | ALit lit -> int_of_lit ctx lit

let rec simplify_bexp ctx bexp =
  match bexp with
  | Bool b -> b
  | And (bexp1, bexp2) -> simplify_bexp ctx bexp1 && simplify_bexp ctx bexp2
  | Or (bexp1, bexp2) -> simplify_bexp ctx bexp1 || simplify_bexp ctx bexp2
  | Not bexp -> not (simplify_bexp ctx bexp)
  | BEq (aexp1, aexp2) -> simplify_aexp ctx aexp1 = simplify_aexp ctx aexp2
  | BNeq (aexp1, aexp2) -> simplify_aexp ctx aexp1 <> simplify_aexp ctx aexp2
  | BLe (aexp1, aexp2) -> simplify_aexp ctx aexp1 <= simplify_aexp ctx aexp2
  | BGt (aexp1, aexp2) -> simplify_aexp ctx aexp1 > simplify_aexp ctx aexp2

let rec simplify_term ctx term =
  match term with
  | If (bexp, ast1) ->
      if simplify_bexp ctx bexp then simplify_ast ctx ast1 else [ Nop ]
  | While (cond, ast) -> if simplify_bexp ctx cond then [ term ] else [ Nop ]
  | Elif (bexp, ast1, ast2) ->
      if simplify_bexp ctx bexp then simplify_ast ctx ast1
      else simplify_ast ctx ast2
  | Def (str, aexp) -> [ Def (str, ALit (`NUM (simplify_aexp ctx aexp))) ]
  | Print aexp -> [ Print (ALit (`NUM (simplify_aexp ctx aexp))) ]
  | Nop -> [ Nop ]

and simplify_ast ctx ast =
  match ast with
  | [] -> []
  | hd :: ls ->
      let ast = simplify_term ctx hd in
      ast @ simplify_ast ctx ls

let rec interp_term ctx term =
  match term with
  | If (bexp, ast1) ->
      if simplify_bexp ctx bexp then interp_ast ctx ast1 else ctx
  | While (cond, ast) ->
      if simplify_bexp ctx cond then interp_term (interp_ast ctx ast) term
      else ctx
  | Elif (bexp, ast1, ast2) ->
      if simplify_bexp ctx bexp then interp_ast ctx ast1
      else interp_ast ctx ast2
  | Print aexp ->
      ALit (`NUM (simplify_aexp ctx aexp)) |> faexp |> printf "%s\n";
      ctx
  | Nop -> ctx
  | Def (str, aexp) ->
      let newaexp = ALit (`NUM (simplify_aexp ctx aexp)) in
      add str newaexp ctx

and interp_ast ctx ast =
  match ast with
  | [] -> ctx
  | hd :: ls ->
      let newctx = interp_term ctx hd in
      interp_ast newctx ls

let lex str debug =
  try
    let tokens = Lexer.start_pos |> Lexer.tokenize str in
    if debug then print_ptokens tokens;
    tokens
  with
  | Lexing_error (err, pos) ->
      printf "LEXING ERROR at line %d, offset %d: %s\n\n\n" pos.line_num
        pos.bol_off err;
      print_string "Printing retrieved tokens...\n\n";
      (*print_tokens toks;*) []
  | err ->
      printf "\n";
      Printexc.to_string err |> printf "CONTACT MAINTAINERS: %s\n";
      []

let parse toks debug =
  try
    let _, ast = Parser.parse (Parser.create toks) [] EOF in
    if debug then fast ast 0 |> printf "%s\n";
    ast
  with
  | Fatal err ->
      printf "\n";
      printf "CONTACT MAINTAINERS: %s\n" err;
      []
  | Parsing_error (err, tok) ->
      printf "\n";
      printf "PARSING ERROR: %s, got %s\n" err (format_ptoken tok);
      []
  | err ->
      printf "\n";
      Printexc.to_string err |> printf "CONTACT MAINTAINERS: %s\n";
      []

let read str ctx ?(debug_tokens = false) ?(debug_ast = false) () =
  try
    let tokens = lex str debug_tokens in
    let ast = parse tokens debug_ast in
    interp_ast ctx ast
  with
  | Map_error err ->
      printf "\n";
      printf "Value %s does not exist in context\n" err;
      ctx
  | err ->
      printf "\n";
      Printexc.to_string err |> printf "CONTACT MAINTAINERS: %s\n";
      ctx
