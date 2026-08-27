open Parser

type aexp_map =
  | Empty
  | Elem of string * aexp * aexp_map

exception Map_error of string

val find : string -> aexp_map -> aexp
val remove : string -> aexp_map -> aexp_map
val add : string -> aexp -> aexp_map -> aexp_map
