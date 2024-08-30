open Trustee.Syntax;;

let code = 
  {|
  let plugin p = {
    let fun f (x:int) : int = x+1 in handle: {f}
  } in 
  (proj (1, "2", p.f) 2) 5
  |}
;;

let value = Int(6);;

let%expect_test "proj_plugin_output" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let res = Trustee.Interpreter.eval code in
  print_endline (Trustee.Utils.string_of_value res);
  [%expect {| 
  Warning: the computed value can be tainted 
  6
  |}]