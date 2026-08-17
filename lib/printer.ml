
open Printf
open Parser
open Ctx

let fop op =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Eq -> "=";;

let rec fexpr expr = 
  match expr with 
  | Num n -> sprintf "%d" n
  | Binop (op, expr1, expr2) -> 
      sprintf "(%s %s %s)" (fexpr expr1) (fop op) (fexpr expr2)
  | Var str -> str

let rec fmap (map : expr_map) : string =
  match map with 
  | Empty -> ""
  | Elem (key, expr, map') -> sprintf "%s -> %s\n%s" key (fexpr expr) (fmap map');;

let rec fdef ((name, expr) : def) = 
  sprintf "Def %s = %s" name (fexpr expr)

let rec fterm term = 
  match term with 
  | Def d -> fdef d
  | Elif (cond, ast1, ast2) -> sprintf "If (%s) then\n { %s }\n  else\n { %s }" (fexpr cond) (fast ast1) (fast ast2)
  | If (cond, ast) -> sprintf "If (%s) then { %s }" (fexpr cond) (fast ast)
  | Print expr -> fexpr expr |> sprintf "Print (%s)"
  | Nop -> "Nop"

and fast ast = 
  match ast with
  | hd :: ls -> sprintf "%s; %s" (fterm hd) (fast ls)
  | [] -> ""

let print (func : 'a -> string) (obj : 'a) =
  let str = func obj in
  printf "%s\n" str;;
