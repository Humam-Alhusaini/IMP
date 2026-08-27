open Parser
open Ctx

val faexp : aexp -> string
val fbexp : bexp -> string
val fmap : aexp_map -> string
val fdef : def -> string
val fterm : term -> string
val fast : ast -> string
