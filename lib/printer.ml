
open Printf
open Parser
open Ctx

let fop op =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Eq -> "=";;

let rec faexp aexp = 
  match aexp with 
  | Num n -> sprintf "%d" n
  | Binop (op, aexp1, aexp2) -> 
      sprintf "(%s %s %s)" (faexp aexp1) (fop op) (faexp aexp2)
  | Var str -> str

let fbexp bexp = 
  match bexp with 
  | True -> "True"
  | False -> "False";;

let rec fmap (map : aexp_map) : string =
  match map with 
  | Empty -> ""
  | Elem (key, aexp, map') -> sprintf "%s -> %s\n%s" key (faexp aexp) (fmap map');;

let rec fdef ((name, aexp) : def) = 
  sprintf "Def %s = %s" name (faexp aexp)

let rec fterm term = 
  match term with 
  | Def d -> fdef d
  | Elif (cond, ast1, ast2) -> sprintf "If (%s) then\n { %s }\n  else\n { %s }" (fbexp cond) (fast ast1) (fast ast2)
  | If (cond, ast) -> sprintf "If (%s) then { %s }" (fbexp cond) (fast ast)
  | Print aexp -> faexp aexp |> sprintf "Print (%s)"
  | Nop -> "Nop"

and fast ast = 
  match ast with
  | hd :: ls -> sprintf "%s; %s" (fterm hd) (fast ls)
  | [] -> ""

let print (func : 'a -> string) (obj : 'a) =
  let str = func obj in
  printf "%s\n" str;;
