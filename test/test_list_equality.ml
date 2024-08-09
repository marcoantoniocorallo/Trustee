open TFhree.Syntax;;

let code = 
  {|
  ["pipp"] = ["pippo"]
  |}
;;

let value = Bool(false);;

let%test "test_list_equality" =
  let (@@) v1 v2 = TFhree.Syntax.compare_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value