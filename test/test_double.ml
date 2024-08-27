open TFhree.Syntax;;

let code = 
  {|
    let fun f (x : int) : int = x*2 in f (-5) // -10
  |}
;;

let value = Int(-10);;

let%test "test_double" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value