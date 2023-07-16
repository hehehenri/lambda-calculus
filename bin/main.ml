module Interpreter = struct 
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

  let ast = App(Abs("x", Var "x"), Abs("y", Var "y"))
  let _result = eval Context.empty ast
end

module Lexer = struct
  type token =   
  | LParen
  | RParen
  | Lambda
  | Dot
  | Identifier of string

  let rec lex input =
    let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false in

    let rec lex_ident acc seq =
      match seq with
      | char::seq when is_alpha char -> lex_ident (acc ^ String.make 1 char) seq
      | _ -> Identifier acc in

    match input with
    | [] -> []
    | char :: seq ->
        let rest = lex seq in
        match char with
        | '(' -> LParen::rest
        | ')' -> RParen::rest
        | '\\' -> Lambda::rest
        | '.' -> Dot::rest
        | char when is_alpha char ->
            let token = lex_ident (String.make 1 char) seq in
            token::rest
        | _ -> rest

  let lex input =
    let chars = List.of_seq (String.to_seq input) in
    lex chars

  let debug tokens =
    List.iter (fun token -> match token with
      | LParen -> print_endline "("
      | RParen -> print_endline ")"
      | Lambda -> print_endline "λ"
      | Dot -> print_endline "."
      | Identifier id -> print_endline id) tokens
end

module Parser = struct
  
  let rec parse tokens =
    match tokens with
    | LParen::seq ->
        let (expr, rem) = parse rest in
        assert false
        

end

let input = {|(\x.x) (\y.y)|}
let tokens = Lexer.lex input

let () = Lexer.debug tokens

