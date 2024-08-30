open Trustee.Syntax;;

let code = 
  {|
  (fun fact(n : int) : int = 
  if n = 0 then 
    1
  else
    n * fact (n - 1)) 5
  |}
;;

let value = Int(120);;

let%test "test_anon_fact" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ value