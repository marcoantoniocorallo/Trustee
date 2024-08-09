open TFhree.Syntax;;

let code = 
  {|
  let fun fact(n : int) : int = 
    if n = 0 then 
      1
    else
      n * fact (n - 1)
  in 
  fact 5
  |}
;;

let value = Int(120);;

let%test "test_fact" =
  let (@@) v1 v2 = TFhree.Syntax.compare_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value