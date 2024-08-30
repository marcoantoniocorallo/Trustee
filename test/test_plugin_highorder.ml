let code = {|
  // plugin is not imported here for simplicity: automated test env doesn't find the file
  let plugin p = {
    let s = "s" in
    let fun f (x : int) : int = x+1
    in handle: {f}
  } in
  (lambda (x : int) (f : int -> int) : int -> f x) 5 p.f // can now be passed as parameter
|};;

let%expect_test "Plugin high order 1" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let res = Trustee.Interpreter.eval code in
  print_endline (Trustee.Utils.string_of_value res);
  [%expect {| 
    Warning: the computed value can be tainted 
    6
  |}]
;;

let code = {|
  let plugin p = {
    let s = "s" in
    let fun f (x : int) : int = x+1
    in handle: {f}
  } in
  (lambda : (int->int) -> p.f )() 5 // and can be returned!
|};;

let%expect_test "Plugin high order 2" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let res = Trustee.Interpreter.eval code in
  print_endline (Trustee.Utils.string_of_value res);
  [%expect {| 
    Warning: the computed value can be tainted 
    6
  |}]