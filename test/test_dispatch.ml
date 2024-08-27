open TFhree.Syntax;;

let code = 
  {|
    let fun f(x : int -> int) : int = x 1 in
    let fun g(y : int) : int = y + 2 in
    let fun h(z : int) : int = z + 3 in
    (f g) + (f h) // 7
  |}
;;

let value = Int(7);;

let%test "test_dispatch" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value