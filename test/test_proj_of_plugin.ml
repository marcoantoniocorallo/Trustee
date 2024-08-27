open TFhree.Syntax;;

let code = 
  {|
  let plugin p = {
    let fun f (x:int) : int = x+1 in f
  } in 
  (proj (1, "2", p) 2) 5
  |}
;;

let value = Int(6);;

let%expect_test "proj_plugin_output" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let res = TFhree.Interpreter.eval code in
  print_endline (TFhree.Utils.string_of_value res);
  [%expect {| 
  Warning: the computed value can be tainted 
  6
  |}]