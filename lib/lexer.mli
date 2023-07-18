type token =   
  | LParen
  | RParen
  | Lambda
  | Dot
  | Identifier of string

  val lex : string -> token List.t

  val to_string : token List.t -> string
