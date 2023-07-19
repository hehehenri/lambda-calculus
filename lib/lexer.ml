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
