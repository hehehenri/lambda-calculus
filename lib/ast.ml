module Context = Map.Make(String)

type expr =
  | Var of string
  | Abs of string * expr
  | App of expr * expr

type value =
  | Closure of string * expr * value Context.t

let rec eval context expr =
  let apply context l_expr r_expr =
    let arg = eval context r_expr in
      match eval context l_expr with
      | Closure (param, body, context) ->
          let context = Context.add param arg context in
          eval context body in

  match expr with
  | Var name -> (match Context.mem name context with
    | true -> Context.find name context
    | false -> failwith ("unbound variable: " ^ name))
  | Abs (param, body) -> Closure (param, body, context)
  | App (l_expr, r_expr) -> apply context l_expr r_expr

let rec to_string expr =
  match expr with
  | Var name -> Printf.sprintf "Var %s" name
  | Abs (arg, body) -> Printf.sprintf "Abs(%s, %s)" arg (to_string body)
  | App (l_expr, r_expr) -> Printf.sprintf "App(%s, %s)" (to_string l_expr) (to_string r_expr)

