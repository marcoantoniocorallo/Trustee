open TFhree.Syntax;;

let code = 
  {|
    // Parentheses needed
    let fun f (g : (int * int) -> int ) : int = 0 in 
    f ( fun h (t : (int * int) ) : int = 3 )
  |}
;;

let value = Int(0);;

let%test "test_tuple_vs_fun1" =
  let (@@) v1 v2 = TFhree.Syntax.compare_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value

let code = 
  {|
    let fun f (t : (int * (int -> int) ) ) : int = 0 in 
    f ( 1, lambda (x : int) : int -> x )
  |}
;;

let value = Int(0);;

let%test "test_tuple_vs_fun2" =
  let (@@) v1 v2 = TFhree.Syntax.compare_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value
