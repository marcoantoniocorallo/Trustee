open Trustee.Syntax;;

let code = 
  {|
  (lambda (x:(int -> int)) : (int->int) -> x)(lambda (y:int) : int -> y)
  |}
;;

let value = Closure("", "y", Trustee.Utils.dummy_value, []);;

let%test "idid" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ value