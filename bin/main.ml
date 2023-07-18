open Lambda_calculus

let input = {|((\x. x)(\z. z))|}
let tokens = Lexer.lex input
let ast = Parser.parse tokens
let () = print_endline (Ast.to_string ast)
let _ = Ast.(eval Context.empty ast)
