open TFhree.Syntax;;

let code = 
  {|
  let fun f (x : ((int -> int) * int)) : int = (proj x 0) (proj x 1) in 
    let fun g (x : int) : int = x*x in 
    f (g,5)
  |}
;;

let value = Int(25);;

let%test "test_high_order" =
  let (@@) v1 v2 = TFhree.Syntax.compare_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value