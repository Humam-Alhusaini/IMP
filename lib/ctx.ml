exception Map_error of string

open Parser
open Printf

type aexp_map = 
  | Empty
  | Elem of string * aexp * aexp_map

let rec find key map =
  match map with 
  | Empty -> Map_error key |> raise
  | Elem (str, aexp, map') -> if str = key then aexp else find key map';;

let rec remove key map =
  match map with 
  | Empty -> Map_error key |> raise
  | Elem (str, aexp, map') -> if str = key then map' else Elem (str, aexp, remove key map');;

let rec add name valu map = 
  match map with 
  | Empty -> Elem (name, valu, map)
  | Elem (str, aexp, map') -> if str = name then Elem (str, valu, map') else Elem (str, aexp, add name valu map');;
  
