open Trustee.Syntax;;
let code = 
  {|
  let fun f (x : int) : int = x*x in 
  let t : (int * int * (int -> int)) = (1,3,f) in 
  (proj t 2) 5
  |}
;;

let value = Int(25);;

let%test "sum2_1" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ value