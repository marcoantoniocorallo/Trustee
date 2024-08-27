open TFhree.Syntax;;

let code = 
  {|
  (lambda (x:(int -> int)) : (int->int) -> x)(lambda (y:int) : int -> y)
  |}
;;

let value = Closure("", "y", TFhree.Utils.dummy_value, []);;

let%test "idid" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value