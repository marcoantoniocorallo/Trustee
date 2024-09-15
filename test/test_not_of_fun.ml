open Trustee.Syntax;;

let code = 
  {|
  not( (fun f (x:int) : bool = true) 0 )
  |}
;;

let value = Bool(false);;

let%test "test_not_of_fun" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ value