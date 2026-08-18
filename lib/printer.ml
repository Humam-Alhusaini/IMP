
open Printf
open Parser
open Ctx

let rec faexp aexp = 
  match aexp with 
  | Num n -> sprintf "%d" n
  | Aplus (aexp1, aexp2) -> 
      sprintf "(%s + %s)" (faexp aexp1) (faexp aexp2)
  | Asub (aexp1, aexp2) -> 
      sprintf "(%s - %s)" (faexp aexp1) (faexp aexp2)
  | Amult (aexp1, aexp2) -> 
      sprintf "(%s * %s)" (faexp aexp1) (faexp aexp2)
  | Var str -> str

let rec fbexp bexp = 
  match bexp with 
  | True -> "True"
  | False -> "False"
  | And (bexp1, bexp2) -> sprintf "%s && %s" (fbexp bexp1) (fbexp bexp2)
  | Or (bexp1, bexp2) -> sprintf "%s || %s" (fbexp bexp1) (fbexp bexp2)
  | Not bexp -> sprintf "~ %s" (fbexp bexp)
  | BEq (aexp1, aexp2) -> sprintf "%s = %s" (faexp aexp1) (faexp aexp2)
  | BNeq (aexp1, aexp2) -> sprintf "%s <> %s" (faexp aexp1) (faexp aexp2)
  | BLe (aexp1, aexp2) -> sprintf "%s <> %s" (faexp aexp1) (faexp aexp2)
  | BGt (aexp1, aexp2) -> sprintf "%s <> %s" (faexp aexp1) (faexp aexp2)
;;

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
