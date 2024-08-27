open TFhree.Syntax;;

let code = 
  {|
  let fun f (a : int -> (int*int)) : int -> (int*int) = a in f 
  |}
;;

let value = 
  Closure("f","a", TFhree.Utils.dummy_value,[]);;

let%test "program0" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value
