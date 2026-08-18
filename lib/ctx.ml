exception Map_error of string

open Parser
open Printf

type aexp_map = 
  | Empty
  | Elem of string * aexp * aexp_map

let rec find (key : string) (map : aexp_map) : aexp =
  match map with 
  | Empty -> Map_error key |> raise
  | Elem (str, aexp, map') -> if str = key then aexp else find key map';;

let rec remove (key : string) (map: aexp_map) : aexp_map =
  match map with 
  | Empty -> Map_error key |> raise
  | Elem (str, aexp, map') -> if str = key then map' else Elem (str, aexp, remove key map');;

let rec add (name: string) (valu : aexp) (map : aexp_map) : aexp_map = 
  match map with 
  | Empty -> Elem (name, valu, map)
  | Elem (str, aexp, map') -> if str = name then Elem (str, valu, map') else Elem (str, aexp, add name valu map');;
  
