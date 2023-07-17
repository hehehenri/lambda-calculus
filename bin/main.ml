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

  let rec to_string expr =
    match expr with
    | Var name -> Printf.sprintf "Var %s" name
    | Abs (arg, body) -> Printf.sprintf "Abs(%s, %s)" arg (to_string body)
    | App (l_expr, r_expr) -> Printf.sprintf "App(%s, %s)" (to_string l_expr) (to_string r_expr)
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

  let to_string token =
    match token with
      | LParen -> "LParen"
      | RParen -> "RParen"
      | Lambda -> "Lambda"
      | Dot -> "Dot"
      | Identifier id -> Printf.sprintf "Identifier (%s)" id

  let to_string tokens =
    List.map(fun token -> to_string token) tokens
    |> String.concat " "

  (* let debug tokens = *)
  (*   List.iter (fun token -> print_string ((to_string token) ^ " ")) tokens; *)
  (*   print_endline "" *)
end

module Parser = struct
  open Lexer
  open Interpreter

  let rec parse_expr tokens = 
    let parse_abs tokens =
      (match tokens with
      | Lambda::Identifier id::Dot::rest ->
          let (body, rem_tokens) = parse_expr rest in
          Abs (id, body), rem_tokens
      | _ -> failwith "failed to parse: abstraction expected") in

    let rec parse_app l_expr tokens =
      match tokens with
      | [] as empty ->  (l_expr, empty)
      | RParen::rest -> (l_expr, rest)
      | tokens ->
          let r_expr, rem = parse_expr tokens in
          parse_app (App (l_expr, r_expr)) rem
    in

    match tokens with
    | LParen::rem ->
        let l_expr, rem = parse_expr rem in
        (match rem with
        | RParen :: rem -> parse_app l_expr rem
        | _ -> failwith "failed to parse: expecting closing parenthesis")
    | Lambda::_ -> parse_abs tokens
    | Identifier name::rem -> Var name, rem
    | tokens -> failwith (Printf.sprintf "failed to parse: unexpected token(%s)" (Lexer.to_string tokens))
    
  let parse tokens =
    match parse_expr tokens with
    | expr, [] -> expr
    | _expr, _tokens -> failwith "failed to parse: extra tokens"
end

let input = {|(\x. x)(\z. z)|}
let tokens = Lexer.lex input
let ast = Parser.parse tokens
let () = print_endline (Interpreter.to_string ast)
let _ = Interpreter.(eval Context.empty ast)
