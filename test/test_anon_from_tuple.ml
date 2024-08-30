open Trustee.Syntax;;

let code = 
  {|
  let t = (1,3,(lambda (x : int) : int -> 2*x+1)) in (proj t 2) 5
  |}
;;

let value = Int(11);;

let%test "test_anon_tuple" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ value