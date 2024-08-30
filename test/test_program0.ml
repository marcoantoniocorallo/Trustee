open Trustee.Syntax;;

let code = 
  {|
  let fun f (a : int -> (int*int)) : int -> (int*int) = a in f 
  |}
;;

let value = 
  Closure("f","a", Trustee.Utils.dummy_value,[]);;

let%test "program0" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ value
