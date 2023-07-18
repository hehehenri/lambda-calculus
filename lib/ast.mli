module Context : module type of Map.Make(String)

type expr =
  | Var of string
  | Abs of string * expr
  | App of expr * expr

type value =
  | Closure of string * expr * value Context.t

val eval : value Context.t -> expr -> value

val to_string : expr -> string
