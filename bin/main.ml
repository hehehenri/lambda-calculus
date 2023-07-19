open Lambda_calculus
open Lambda_calculus.Lexer

(** (\a. a)() **)

let tokens = [
  LParen; 
  Lambda; 
  Identifier("x"); 
  Dot; 
  Identifier("x"); 
  RParen;
  LParen; 
  Lambda; 
  Identifier("x"); 
  Dot; 
  Identifier("x");
  RParen;
]

let _ast = Parser.parse tokens

let expected_ast = Ast.(App(
    Abs("x", App(Var("x"), Var("x"))),
    Abs("x", App(Var("x"), Var("x")))
  ))

let _result = Ast.eval Ast.Context.empty expected_ast
