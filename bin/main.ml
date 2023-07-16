module Context = Map.Make(String)

type expr =
  | Var of string
  | Abs of (string * expr)
  | App of (expr * expr)

type value =
  | Closure of { context: value Context.t; param : string; body : expr; }

let rec eval context expr =
  match expr with
  | Var name -> Context.find name context
  | Abs (param, body) -> Closure {context; param; body}
  | App (l_expr, r_expr) ->
      let arg = eval context r_expr in
      match eval context l_expr with
      | Closure {context; param; body} ->
          let context = Context.add param arg context in
          eval context body

(** (\x.x x) (\x.x x) **)
let u_comb = Abs ("x", App((Var "x"), (Var "x")))
let ast = App(u_comb, u_comb)

let _result = eval Context.empty ast
