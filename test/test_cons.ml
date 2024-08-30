open Trustee.Syntax;;

let code = {| 0 :: [1, 2, 3] |};;

let value = ListV([Int(0); Int(1); Int(2); Int(3)]);;

let%test "cons" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 

  vval @@ value